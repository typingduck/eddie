%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.eddieware.org/EPL
%%%
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is Eddie-0.83b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : dns_xfr.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Perform a zone transfer.
%%% Created :  6 May 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_xfr).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

-compile(export_all).

%% External exports
-export([start_link/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {parent,
	        zone,
	        addrs,
	        serial,
	        retry,
		retry_timer,
	        expire,
	        expire_timer}).

-include("dns.hrl").

-define(UDP_TIMEOUT, 5000). %% TBD, should be configurable !

-define(SERIAL(Data), element(3, Data)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Zone, Addrs, Serial, Retry, Expire) ->
    gen_server:start_link(dns_xfr, [Zone, Addrs, Serial, Retry,
				    Expire, self()], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([Zone, Addrs, Serial, Retry, Expire, Parent]) ->
    ExpTimer = init_timer(Expire, expired),
    self() ! {timeout, retry}, %% Initate initial try.
    {ok, #state{parent = Parent,
		zone = Zone,
	        addrs = Addrs,
	        serial = Serial,
	        retry = Retry,
	        expire = Expire,
	        expire_timer = ExpTimer}}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({timeout, retry}, State) ->
    #state{zone = Zone, addrs = Addrs,
	   serial = Serial, parent = Parent} = State,
    case catch check_serial(Zone, Addrs, Serial) of
	ok ->
	    Parent ! {self(), Zone, uptodate},
	    {stop, shutdown, State};
	{ok, IP} ->
	    case catch do_xfr(IP, Zone, Parent) of
		ok ->
		    Parent ! {self(), Zone, done},
		    {stop, shutdown, State};
		Error ->
		    Parent ! {self(), Zone, retry},
		    RetT = init_timer(State#state.retry, retry),
		    {noreply, State#state{retry_timer = RetT}}
	    end;
	_ ->
	    RetT = init_timer(State#state.retry, retry),
	    {noreply, State#state{retry_timer = RetT}}
    end;

handle_info({timeout, expired}, State) ->
    #state{parent = Parent, zone = Zone} = State,
    Parent ! {self(), Zone, expired},
    {stop, shutdown, State};

handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    #state{expire_timer = ExpT, retry_timer = RetT} = State,
    cancel_timer(ExpT),
    cancel_timer(RetT),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Get the Zone SOA from one of Addrs ip addresses.
%% Returns: ok - if the serial is up to date.
%%          {ok, IP} - if the serial is out of date, perform a zone
%%                     transfer on IP.
%%          false - no soa found yet.
%%
check_serial(Zone, Addrs, Serial) ->
    {ok, Udp} = gen_udp:open(0, [binary]),
    {Query, ID} = mk_query(Zone, ?S_SOA),
    R = case get_serial(Addrs, Udp, Query, ID) of
	    {ok, Serial, _} -> ok;
	    {ok, _, IP}     -> {ok, IP};
	    _               -> false
	end,
    gen_udp:close(Udp),
    R.

mk_query(Zone, Type) ->
    Q = #dns_query{domain = Zone,
		   type = Type,
		   class = in},
    {ok, ID} = dns_catalog:next_id(),
    {ok, Buffer} = inet_dns:encode(#dns_rec{header = #dns_header{id = ID},
					    qdlist = [Q]}),
    {list_to_binary(Buffer), ID}.

get_serial([IP|Addrs], Udp, Query, ID) ->
    ok = gen_udp:send(Udp, IP, ?NAMESERVER_PORT, Query),
    receive
	{udp, Udp, IP, _, Packet} ->
	    case decode_resp(Packet, ID) of
		{ok, Serial} ->
		    {ok, Serial, IP};
		_ ->
		    get_serial(Addrs, Udp, Query, ID)
	    end
    after ?UDP_TIMEOUT ->
	    get_serial(Addrs, Udp, Query, ID)
    end;
get_serial([], _, _, _) ->
    false.

decode_resp(Packet, ID) ->
    case inet_dns:decode(binary_to_list(Packet)) of
	{ok, Answ} when (Answ#dns_rec.header)#dns_header.rcode == ?NOERROR,
			(Answ#dns_rec.header)#dns_header.id == ID ->
	    [A|_] = Answ#dns_rec.anlist,
	    {ok, ?SERIAL(A#dns_rr.data)};
	_ ->
	    error
    end.

%%
%% Perform the zone transfer.
%%
do_xfr(IP, Zone, Parent) ->
    {ok, S} = gen_tcp:connect(IP, ?NAMESERVER_PORT, [{packet, 2}, binary]),
    {Query, _} = mk_query(Zone, ?S_AXFR),
    ok = gen_tcp:send(S, Query),
    case get_initial_soa(S, Zone) of
	{ok, SOA} ->
	    Parent ! {self(), Zone, {insert, SOA}},
	    get_until_soa(S, SOA, Parent, Zone);
	_ ->
	    error
    end.

get_initial_soa(S, Zone) ->
    receive
	{tcp, S, Data} ->
	    case decode_data(Data) of
		{ok, RR} when RR#dns_rr.type == ?S_SOA ->
		    {ok, RR};
		_ ->
		    error
	    end;
	{tcp_closed, S} ->
	    error
    end.

get_until_soa(S, SOA, Parent, Zone) ->
    receive
	{tcp, S, Data} ->
	    case decode_data(Data) of
		{ok, SOA} ->
		    ok;   %% We are done !
		{ok, RR} ->
		    Parent ! {self(), Zone, {insert, RR}},
		    get_until_soa(S, SOA, Parent, Zone);
		_ ->
		    error
	    end;
	{tcp_closed, S} ->
	    error
    end.
		    
%%
%% We do only take one RR in each response as BIND do.
%% TBD, check RFC if it is allowed to handle several RRs at the
%% same time !
%%
decode_data(Data) ->
    case inet_dns:decode(binary_to_list(Data)) of
	{ok, Answ} when (Answ#dns_rec.header)#dns_header.rcode == ?NOERROR ->
	    {ok, hd(Answ#dns_rec.anlist)};
	_ ->
	    error
    end.

%%
%% Send a timeout after TimeS seconds.
%% Can't use erlang:send_after/3 for very long timers due to 27 bit
%% limitation on Time.
%%
init_timer(TimeS, Type) when integer(TimeS), TimeS * 1000 =< 16#7ffffff ->
    Ref = erlang:send_after(TimeS * 1000, self(), {timeout, Type}),
    {erlang, Ref};
init_timer(TimeS, Type) when integer(TimeS) ->
    case timer:send_after(TimeS * 1000, self(), {timeout, Type}) of
	{ok, Ref} -> {timer, Ref};
	_         -> undefined
    end;
init_timer(_, _) ->
    undefined.

%%
%% Cancel a timer (if active).
%%
cancel_timer({erlang, Ref}) -> erlang:cancel_timer(Ref);
cancel_timer({timer, Ref})  -> timer:cancel(Ref);
cancel_timer(_)             -> ok.
