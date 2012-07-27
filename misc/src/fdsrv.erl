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

%%
%% fdsrv.erl
%%
%% Author: Sebastian Strollo <seb@erix.ericsson.se>
%%
%% Program that passes open file descriptors between processes using
%% AF_UNIX stream sockets, as described in Stevens UNIX Network programming.
%%

-module(fdsrv).

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, bind_socket/2, stop/0]).
-export([test/0, test/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-record(state, {dport, sport}).

-include_lib("misc/include/logger.hrl").

-define(TYPE(Type), if
			Type == tcp -> 0;
			true        -> 1
		    end).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [false], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [true], []).

bind_socket(Type, IP_Port) ->
    Spec = create_spec(IP_Port),
    gen_server:call(?MODULE, {bind_socket, Type, Spec}).

stop() ->
    gen_server:call(?MODULE, stop).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([AppFlag]) ->
    case do_start(AppFlag) of
	true ->
	    do_init();
	_ ->
	    %% Not configured to run !
	    ignore
    end.

do_init() ->
    process_flag(trap_exit, true),
    TmpName = tmpname(),
    os:cmd(["rm -f ", TmpName]),
    PrivDir = code:priv_dir(misc),
    FdSrv = filename:join(PrivDir, "fdsrv"),
    SPort = open_port({spawn, lists:flatten([FdSrv, " ", TmpName])},
		      [use_stdio, {packet, 1}]),
    receive
	{SPort, {data, "ok"}} ->
	    case start_driver(PrivDir, TmpName) of
		{ok, DPort} ->
		    {ok, #state{dport = DPort, sport = SPort}};
		Error ->
		    {stop, Error}
	    end;
	{'EXIT', SPort, Reason} ->
	    ?FATAL(?F("Couldn't start ~s - ~p", [FdSrv, Reason])),
	    {stop, {error, Reason}}
    
    end.

%% If started from within an application; check if configured to run !
do_start(false) ->
    true;
do_start(true) ->
    case application:get_env(start_fdsrv) of
	{ok, true} -> true;
	_          -> false
    end.

start_driver(PrivDir, TmpName) ->
    case erl_ddll:load_driver(PrivDir, "fdsrv_drv") of
	ok ->
	    Drv = lists:flatten(["fdsrv_drv ", TmpName]),
	    DPort = open_port({spawn, Drv}, []),
	    {ok, DPort};
	Error ->
	    ?FATAL(?F("Couldn't link in driver ~s/fdsrv_drv - ~p",
		      [PrivDir, Error])),
	    Error
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call({bind_socket, Type, Spec}, From, State) ->
    #state{dport = DPort, sport = SPort} = State,
    case lists:member(Type, [tcp, udp]) of
	true ->
	    SPort ! {self(), {command, [?TYPE(Type)|Spec]}},
	    receive
		{DPort, {data, "-1"}} ->
		    {reply, {error,"Failed to receive file descriptor"},
		     State};
		{DPort, {data, FDStr}} ->
		    {reply, {ok, list_to_integer(FDStr)}, State};
		{SPort, {data, ErrorMsg}} ->
		    {reply, {error, ErrorMsg}, State}
	    end;
	_ ->
	    {reply, {error, "Bad socket type"}, State}
    end;

handle_call(stop, From, State) ->
    {stop, normal, ok, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};
handle_info(Msg, State) ->
%    io:format("Msg: ~w~n", [Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    #state{dport = DPort, sport = SPort} = State,
    DPort ! {self(), close},
    SPort ! {self(), close}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

tmpname() ->
    no_nl(lists:reverse(os:cmd("echo /tmp/fdsrv$$"))).
no_nl([10|T]) ->
    lists:reverse(T);
no_nl(L) ->
    lists:reverse(L).

create_spec(Port) when integer(Port) ->
    [$:|integer_to_list(Port)];
create_spec({{IP1,IP2,IP3,IP4}, Port}) when integer(IP1),
					    integer(IP2),
					    integer(IP3),
					    integer(IP4),
					    integer(Port) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p:~p",
				[IP1, IP2, IP3, IP4, Port])).

%%
%%
%%
test() ->
    test(8888).
test(Port) ->
    fdsrv:start(),
    case fdsrv:bind_socket(tcp, Port) of
	{ok, Fd} ->
	    {ok, LSock} = gen_tcp:listen(0, [{fd, Fd},
					     {active, false},
					     {packet, 0}]),
	    io:format("listening on port ~w (fd ~w, sock ~w), please connect to it.~n",
		      [Port, Fd, LSock]),
	    {ok, Sock} = gen_tcp:accept(LSock),
	    {ok, Data} = gen_tcp:recv(Sock, 0),
	    gen_tcp:send(Sock, Data),
	    gen_tcp:close(Sock),
	    gen_tcp:close(LSock),
	    ok;
	Error ->
	    Error
    end.

