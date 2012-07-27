-module(log_sig).
-author('magnus@erix.ericsson.se').
%%% --------------------------------------------------------------------
%%% File    : log_sig.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Catch Unix signals
%%% Created : 23 Jun 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Apr 99 - geoff@eddieware.org - Appropriated & modified for logger SIGHUP
%%% Modified: 27 Apr 1999 by tobbe@eddieware.org
%%% --------------------------------------------------------------------
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
%%% --------------------------------------------------------------------
-vc('$Id: log_sig.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-export([start_link/0, init/1, handle_info/2, terminate/2]).

-include("eddie_sig.hrl").
-include("logger.hrl").

-record(state, {file}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    sig_handler:start_link(),
    {ok, SigHandlerPid, OwnPid} = sig_handler:pids(),
    sig_handler:join(),
    case write_file(SigHandlerPid, OwnPid) of
    {ok, File} ->
        {ok, #state{file = File}};
    {Error, File} ->
        ?FATAL(?F("Couldn't write ~s - ~p", [File, Error])),
        {stop, Error}
    end.

handle_info({sig_handler, ?ERL_SIGHUP}, State) ->
    ?INFO(?F("Rotating log files.", [])),
    disk_log_handler:reopen(),
    {noreply, State};

handle_info(_, State) ->
%	?INFO(?F("got ~p~n", [State])),
    {noreply, State}.

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
    Append = case node() of
		 'nonode@nohost' -> "";
		 NodeName        -> atom_to_list(NodeName) ++ "_"
	     end,
    case application:get_env(log_dir) of
	{ok, Dir} -> filename:join(Dir, Append ++ "logsig.pid");
	_         -> filename:join(".", Append ++ "logsig.pid")
    end.


%%----------------------------------------------------------------------
%% Should write to a file I guess...
%%----------------------------------------------------------------------
terminate(Reason, State) ->
        ok.

