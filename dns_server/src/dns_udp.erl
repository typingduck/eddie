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
%%% File    : dns_udp.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Handles the UDP sockets.
%%% Created : 17 Jun 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_udp).
-author('magnus@erix.ericsson.se').

-export([start_link/2, init/3]).

-record(state, {port,
		ip,
		udp,
		pending = [],
		options,
		parent}).

-record(pend_req, {id,
		   pid,
		   ip,
		   port}).

-define(TIMEOUT(S), if
			S#state.pending == [] -> infinity;
			true                  -> 4
		    end).

-include("dns.hrl").
-include_lib("misc/include/logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Port, IP) ->
    proc_lib:spawn_link(?MODULE, init, [Port, IP, self()]).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init(Port, IP, Parent) ->
    process_flag(trap_exit, true),
    case open_udp(Port, IP) of
	{ok, Udp} ->
	    loop(#state{port = Port,
			ip = IP,
			udp = Udp,
			options = dns_catalog:options(),
			parent = Parent});
	Error ->
	    exit(Error)
    end.

%%
%% The main loop.
%% If no recursive questions currently are being handled wait
%% for inifinity time for next request on the UDP socket.
%% Otherwise, poll for ready recursive queries.
%% We must use recv here as we want let the system be cluttered
%% with to many simultaneous requests (the overall performance
%% will be decreased).
%%
loop(State0) ->
    State = poll_msgs(State0),
    case gen_udp:recv(State#state.udp, 0, ?TIMEOUT(State)) of
	{ok,{IP, Port, Packet}} ->
	    State1 = handle_udp(IP, Port, Packet, State),
	    loop(State1);
	{error, timedout} ->
	    loop(State);
	{error, Error} ->
	    State1 = restart_udp(State, Error),
	    loop(State1)
    end.
	

open_udp(Port, IP) ->
    case gen_udp:open(Port, [{ip, IP},
			     binary,
			     {active, false},
			     {header, 4}]) of
	{ok, Udp} ->
	    {ok, Udp};
	Error ->
	    ?FATAL(?F("Can't open UDP socket ~p:~p - ~p",
		      [IP, Port, Error])),
	    Error
    end.

restart_udp(State, Reason) ->
    #state{port = Port, ip = IP} = State,
    ?INFO(?F("Reopen UDP ~p:~p - ~p", [IP, Port, Reason])),
    case open_udp(Port, IP) of
	{ok, Udp} ->
	    State#state{udp = Udp};
	Error ->
	    exit(Error)
    end.

%%
%% Take care of an UDP request.
%%
handle_udp(IP, Port, Request, State) ->
    #state{pending = Pends, options = Opts} = State,
    case inet_dns:decode_header(Request) of
	{ok, Header, _} ->
	    Id = Header#dns_header.id,
	    case match_delete_old(Pends, Id, IP, Port) of
		false ->
		    case dns_query:start_link(Request,
					      IP,
					      Port,
					      udp,
					      Opts) of
			{ok, Pid} ->
			    Req = #pend_req{id = Id,
					    pid = Pid,
					    ip = IP,
					    port = Port},
			    wait_for_resp(Pid,
					  State#state{pending = [Req|Pends]});
%			    State#state{pending = [Req|Pends]};
			Error ->
			    ?ERROR(?F("Can't start query evaluator "
				      "for ~p:~p - ~p",
				      [IP, Port, Error])),
			    State
		    end;
		{true, Req, NPends} ->
		    dns_query:resent(Req#pend_req.pid, Request),
		    State#state{pending = [Req|NPends]}
	    end;
	Error ->
	    ?INFO(?F("Invalid request from ~p:~p - ~p",
		     [IP, Port, Error])),
	    State
    end.

wait_for_resp(Pid, State) ->
    receive
	{Pid, response, Data, Id} ->
	    udp_response(Pid, Data, Id, State);

	{Pid,  recursive} ->
	    %% The response is delivered later !
	    State;
	
	{'EXIT', Pid, _} ->
	    case match_delete_old(State#state.pending, Pid) of
		false ->
		    State;
		{true, Pend, NPends} ->
		    State#state{pending = NPends}
	    end;

	Msg ->
	    wait_for_resp(Pid, handle_msg(Msg, State))
    end.

%%
%% Poll the message queue.
%%
poll_msgs(State) ->
    receive
	Msg -> poll_msgs(handle_msg(Msg, State))
    after 0 -> State
    end.

handle_msg(Msg, State) ->
    case Msg of 
	{Pid, response, Data, Id} ->
	    udp_response(Pid, Data, Id, State);

	{udp_closed, Udpa} ->
	    restart_udp(State, closed);

	{udp_error, Udp, Error} ->
	    %% We will receive an {udp_closed, Udp} as well !
	    ?INFO(?F("UDP error ~p - ~p~n", [State#state.ip, Error])),
	    State;

	{'EXIT', Pid, _} when Pid == State#state.parent ->
	    exit(shutdown);

	{'EXIT', Pid, _} ->
	    case match_delete_old(State#state.pending, Pid) of
		false ->
		    State;
		{true, Pend, NPends} ->
		    State#state{pending = NPends}
	    end;

	Other ->
%	    io:format("~p got: ~p~n", [?MODULE, Other]),
	    State
    end.

%%
%% Send the response using the same interface as the corresponding
%% request. In case the Udp socket has been restarted only the
%% own_ip is stored (i.e. we can send the response even if the used
%% Udp socket has got a new ID).
%%
udp_response(Pid, Data, Id, State) ->
    case match_delete_old(State#state.pending, Id, Pid) of
	false ->
	    State;
	{true, Pend, NPends} ->
	    Udp = State#state.udp,
	    case catch gen_udp:send(Udp, Pend#pend_req.ip,
				    Pend#pend_req.port, Data) of
		ok ->
		    State#state{pending = NPends};
		{error, closed} ->
		    #state{ip = IP} = State,
		    RealReason = flush_udp_error(Udp, closed),
		    NewUdp = restart_udp(State, RealReason),
		    State#state{udp = NewUdp,
			        pending = NPends};
		{error, Reason} ->
		    %% TBD, store statistics.
		    State#state{pending = NPends};
		{'EXIT', _} ->
		    %% TBD, store statistics if the Udp socket has not been
		    %% successfully restarted.
		    State#state{pending = NPends}
	    end
    end.

flush_udp_error(Udp, Reason) ->
    receive
	{udp_error, Udp, RealReason} ->
	    RealReason
    after 0 ->
	    Reason
    end.

%% match delete IP, PortNo, Id
match_delete_old(Pends, Id, IP, Port) ->
    match_delete_old(Pends, Id, IP, Port, []).

match_delete_old([P|Ps], Id, IP, Port, NPs) when
  P#pend_req.id == Id,
  P#pend_req.ip == IP,
  P#pend_req.port == Port ->
    {true, P, NPs ++ Ps};
match_delete_old([P|Ps], Id, IP, Port, NPs) ->
    match_delete_old(Ps, Id, IP, Port, [P|NPs]);
match_delete_old([], _, _, _, _) -> 
    false.

%% match delete Id, Pid
match_delete_old(Pends, Id, Pid) ->
    match_delete_old1(Pends, Id, Pid, []).

match_delete_old1([P|Ps], Id, Pid, NPs) when
  P#pend_req.id == Id,
  P#pend_req.pid == Pid ->
    {true, P, NPs ++ Ps};
match_delete_old1([P|Ps], Id, Pid, NPs) ->
    match_delete_old1(Ps, Id, Pid, [P|NPs]);
match_delete_old1([], _, _, _) -> 
    false.

%% match delete Pid
match_delete_old(Pends, Pid) ->
    match_delete_old1(Pends, Pid, []).

match_delete_old1([P|Ps], Pid, NPs) when
  P#pend_req.pid == Pid ->
    {true, P, NPs ++ Ps};
match_delete_old1([P|Ps], Pid, NPs) ->
    match_delete_old1(Ps, Pid, [P|NPs]);
match_delete_old1([], _, _) -> 
    false.


