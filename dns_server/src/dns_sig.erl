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
%%% File    : dns_sig.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : The DNS signal handler.
%%% Created :  9 Oct 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_sig).
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, terminate/2]).

-record(state, {file}).

-include_lib("misc/include/eddie_sig.hrl").
-include_lib("misc/include/logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    sig_handler:start_link(),  % should be started underneath the misc app.
    {ok, SigHandlerPid, OwnPid} = sig_handler:pids(),
    sig_handler:join(),
    case write_file(SigHandlerPid, OwnPid) of
	{ok, File} ->
	    {ok, #state{file = File}};
	{Error, File} ->
	    ?FATAL(?F("Couldn't write ~s - ~p", [File, Error])),
	    {stop, Error}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({sig_handler, ?ERL_SIGHUP}, State) ->
    ?INFO(?F("Restarting DNS server", [])),
    init:restart(),
    {noreply, State};

handle_info({sig_handler, ?ERL_SIGINT}, State) ->
    %% TBD, dump database to file.
    {noreply, State};

handle_info({sig_handler, ?ERL_SIGABRT}, State) ->
    %% TBD, write statistics.
    {noreply, State};

handle_info({sig_handler, ?ERL_SIGUSR1}, State) ->
    %% TBD, raise debug level
    {noreply, State};

handle_info({sig_handler, ?ERL_SIGUSR2}, State) ->
    %% TBD, turns off debugging.
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    file:delete(State#state.file),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

write_file(SigHandlerPid, OwnPid) ->
    File = pid_file(),
    S = integer_to_list(SigHandlerPid) ++ " " ++ integer_to_list(OwnPid),
    case file:write_file(File, list_to_binary(S)) of
	ok ->
	    {ok, File};
	Error ->
	    {Error, File}
    end.

pid_file() ->    
    case application:get_env(log_dir) of
	{ok, Dir} -> filename:join(Dir, "dns.pid");
	_         -> filename:join(".", "dns.pid")
    end.


