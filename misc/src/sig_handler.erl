-module(sig_handler).
-author('magnus@erix.ericsson.se').
%%%----------------------------------------------------------------------
%%% File    : sig_handler.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : UNIX signal handler.
%%%           Handles SIGINT, SIGHUP, SIGUSR1, SIGUSR2 signals.
%%% Created : 8 May 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.eddieware.org/EPL
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
-vc('$Id: sig_handler.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
%% External exports
-export([start_link/0, pids/0, join/0, leave/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-behaviour(gen_server).

-include("eddie_sig.hrl").
-include("logger.hrl").

-record(state, {port,
		parent,
	        sigh_pid,
	        own_pid,
	        members = []}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self()], []).

%% Returns {ok, SigHandlerPid, ErlangPid}
pids() ->
    gen_server:call(?MODULE, pids).

%% Want signal messages.
join() ->
    gen_server:call(?MODULE, {join, self()}).

leave() ->
    gen_server:call(?MODULE, {leave, self()}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([Parent]) ->
    process_flag(trap_exit, true),
    process_flag(priority, max),
    case start_sig_handler() of
	{error, Reason} ->
	    ?FATAL(?F("Event: Can't start signal handler~n"
		      "Reason: ~p",
		      [Reason])),
	    {stop, Reason};
	{Port, SighPid, OwnPid} ->
	    {ok, #state{port = Port,
			parent = Parent,
			sigh_pid = SighPid,
		        own_pid = OwnPid}}
    end.

start_sig_handler() ->
    Dir = code:priv_dir(misc),
    Port = open_port({spawn, filename:join(Dir, "sig_handler")},
                     [use_stdio]),
    receive
	{Port, {data, [S1,S0,P1,P0]}} ->
	    {Port, (S1 bsl 8) bor S0, (P1 bsl 8) bor P0};
	{'EXIT', Port, Reason} ->
	    {error, {port_died, Reason}}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call(pids, From, State) ->
    #state{sigh_pid = SPid, own_pid = OPid} = State,
    {reply, {ok, SPid, OPid}, State};

handle_call({join, Pid}, _, State) ->
    Members = State#state.members,
    case lists:member(Pid, Members) of
	true ->
	    {reply, ok, State};
	_ ->
	    link(Pid),
	    {reply, ok, State#state{members = [Pid|Members]}}
    end;

handle_call({leave, Pid}, _, State) ->
    #state{members = Members, parent = Parent} = State,
    unlink(Pid, Parent),
    {reply, ok, State#state{members = lists:delete(Pid, Members)}}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({Port, {data, [Signal]}}, State) ->
    Members = State#state.members,
    send_signal(Signal, Members),
    {noreply, State};

handle_info({'EXIT', Pid, _}, State) when pid(Pid) ->
    Members = State#state.members,
    {noreply, State#state{members = lists:delete(Pid, Members)}};

handle_info({'EXIT', Port, Reason}, State) when State#state.port == Port ->
  ?FATAL(?F("Event: Signal handler died (terminating system...)~n"
	    "Reason: ~p",
	    [Reason])),
    init:stop(),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    #state{members = Members, parent = Parent} = State,
    F = fun(Pid) -> unlink(Pid, Parent) end,
    lists:foreach(F, Members),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

unlink(Parent, Parent) -> true;
unlink(Pid, _)         -> unlink(Pid).

send_signal(Signal, Members) ->
    F = fun(Pid) -> Pid ! {?MODULE, Signal} end,
    lists:foreach(F, Members).






