%%%----------------------------------------------------------------------
%%% File    : queue_timer.erl
%%% Author  : clement@eddieware.org
%%% Created : 31 Aug 2000
%%% Purpose : 
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
%%% AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%----------------------------------------------------------------------

-module(queue_timer).
-author('clement@eddieware.org').
-vc('$Id: queue_timer.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/inet_server/src/queue_timer.erl,v $ ').

%% External exports
-export([start/2]).
%% Internal exports
-export([init/2]).

-include("logger.hrl").

-define(TIMEOUT,60000).  %% How often to check table for timed out sessions.

-record(state,
	{db,        %% Table 
	 time       %% Session time allowed without activity
	}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: start/2
%% Arguments: Admit_db     Table where session information is stored.
%%            Admit_time   How long a session is allowed to remain 
%%                         inactibe before it is removed.
%%----------------------------------------------------------------------

start(Admit_db, Admit_time) ->
    spawn_link(?MODULE,init,[Admit_db, Admit_time]).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
init(Admit_db, Admit_time) ->
    erlang:send_after(?TIMEOUT,self(),timeout),
    loop(#state{db = Admit_db,
		time = Admit_time}).

loop(State) ->
    receive
	timeout ->
	    Sessions = ets:match_object(State#state.db, {{{'_','_','_','_'},'_'},{'_'}}),
	    {Now, _} = statistics(wall_clock),
	    is_db:reset_sessions_size(State#state.db,length(Sessions)),
	    Fun = fun({Client_id,{Last_hit}}) ->
			  Inactive_time = Now - Last_hit,
			  if
			      Inactive_time > State#state.time ->
				  is_db:remove_session(State#state.db,Client_id),
				  is_db:update_sessions_size(State#state.db,-1);
			      true ->
				  true
			  end
		  end,
	    lists:foreach(Fun,Sessions),
	    erlang:send_after(?TIMEOUT,self(),timeout),
	    loop(State)
    end.
