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

%%% File    : sig_ex.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Example usage of the signal handler. 
%%% Created : 23 Jun 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>

-module(sig_ex).
-author('magnus@erix.ericsson.se').

-export([start/0, init/0]).

-include_lib("misc/include/eddie_sig.hrl").

start() ->
    spawn(?MODULE, init, []).

init() ->
    sig_handler:start_link(),
    {ok, UnixPid} = sig_handler:unix_pid(),
    sig_handler:join(),
    io:format("~p: UNIX pid = ~p~n", [?MODULE, UnixPid]),
    loop().

loop() ->
    receive
	{sig_handler, ?ERL_SIGHUP} ->
	    io:format("~p got SIGHUP~n", [?MODULE]),
	    loop();
	{sig_handler, ?ERL_SIGINT} ->
	    io:format("~p got SIGINT~n", [?MODULE]),
	    loop();
	{sig_handler, ?ERL_SIGABRT} ->
	    io:format("~p got SIGABRT~n", [?MODULE]),
	    loop();
	{sig_handler, ?ERL_SIGUSR1} ->
	    io:format("~p got SIGUSR1~n", [?MODULE]),
	    loop();
	{sig_handler, ?ERL_SIGUSR2} ->
	    io:format("~p got SIGUSR2~n", [?MODULE]),
	    loop();
	Other ->
	    io:format("~p got ~p~n", [?MODULE, Other]),
	    loop()
    end.
