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
%%% File    : dns_tcp_accept.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : The TCP accept part of the DNS server.
%%% Created :  4 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_tcp_accept).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2]).

-record(state, {listen,
		socket,
		peer_address,
	        parent,
		options,
	        id,
	        query_pid,
	        timeout = false}).

-define(CLOSE_TIMEOUT, 120000). %% Close an idle connection after 2 min.

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(u16(X1,X0),
	(((X1) bsl 8) bor (X0))).
 
-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-include("dns.hrl").
-include_lib("misc/include/logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Listen, Parent, Id) ->
    gen_server:start_link(dns_tcp_accept, [Listen, Parent, Id], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([Listen, Parent, Id]) ->
    %% Do the accept call in handle_info !
    self() ! do_accept,
    {ok, #state{listen = Listen,
		parent = Parent,
		options = dns_catalog:options(),
		id = Id}}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({tcp, S, Data}, State) when State#state.socket == S ->
    #state{peer_address = {IP, Port}, options = Opts} = State,
    case dns_query:start_link(Data, IP, Port, tcp, Opts) of
	{ok, Pid} ->
	    {noreply, State#state{query_pid = Pid,
				  timeout = false}};
	Error ->
	    ?ERROR(?F("Can't start query evaluator for ~p:~p - ~p",
		      [IP, Port, Error])),
	    {stop, normal, State}
    end;

handle_info({tcp_closed, S}, State) when State#state.socket == S ->
    {stop, normal, State};

handle_info({Pid, response, Data, Id}, State) ->
    tcp_response(Pid, Data, Id, State);

handle_info(timeout, State) ->
    {stop, normal, State};

handle_info(do_accept, State) ->
    #state{listen = Listen, parent = Parent} = State,
    case gen_tcp:accept(Listen) of
	{ok, Socket} ->
	    %% Don't trap_exit earlier due to blocking accept call
	    process_flag(trap_exit, true),
	    Parent ! {self(), accepted},
	    {ok, {IP, Port}} = inet:peername(Socket),
	    {noreply, State#state{socket = Socket,
				  peer_address = {IP, Port},
				  timeout = true},
	     ?CLOSE_TIMEOUT};
	Error ->
	    ?ERROR(?F("Couldn't accept - ~p", [Error])),
	    {stop, Error, State}
    end;

handle_info({'EXIT', Pid, _}, State) when State#state.query_pid == Pid ->
    {stop, normal, State};

handle_info(_, State) when State#state.timeout == true ->
    {noreply, State, ?CLOSE_TIMEOUT};

handle_info(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(shutdown, State) ->
    %% We, explicitly, do not want an EXIT signal in parent in this case.
    unlink(State#state.parent),
    ok;
terminate(Reason, State) ->
    State#state.parent ! {self(), terminated, State#state.id},
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Send a reply to the querying peer.
%% Don't close the socket, it's up to the client. More queries to come.
%% Set a timeout for 2 minutes in order to cleanup, according to RFC1035.
%%
tcp_response(Pid, Data, _Id, State) ->
    case gen_tcp:send(State#state.socket, Data) of
	ok ->
	    {noreply, State#state{query_pid = undefined,
				  timeout = true},
	     ?CLOSE_TIMEOUT};
	_ ->
	    %% Other end closed the socket.
	    {stop, normal, State}
    end.



