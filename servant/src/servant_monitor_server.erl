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
%%% Contributor(s): 20000419 geoff@eddieware.org Extensions for multiple
%%%     actions and load information handling
%%%
%%% 21/07/00 clement.lyons@eddieware.org
%%% Updated terminate function to handle the list of Pids/Ports as it
%%% previously crashed.

%%%----------------------------------------------------------------------
%%% File    : servant_monitor_server.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 17 Sep 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

-module(servant_monitor_server).
-author('jocke@erix.ericsson.se').
-modified('clement.lyons@eddieware.org').
-vc('$Id: servant_monitor_server.erl,v 1.1 2000/10/27 22:20:27 dredd Exp $ ').
-behaviour(gen_server).
-export([start_monitor/1,stop_monitor/1,start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

-include("load_server.hrl").
-include("db.hrl").
-include("logger.hrl").

-define(Ip_up_freq, 3000).

-record(monitor_state,
    {
        state,
        pid,
        tries,
        action
    }).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% start_monitor

start_monitor(S) ->
  case (S#server.template)#template.monitor of
    [] ->
      ok;
    X ->
      ?TEMP_INFO(?F("monitor template ~p", [ X ])),
      case supervisor:start_child(servant_monitor_sup,
				  {S,
				   {?MODULE,start_link,[S]},
				   temporary,
				   5000,
				   worker,
				   [?MODULE]}) of
	{ok,Child} ->
	  ok;
	{error,Reason} ->
	  {error,Reason}
      end
  end.

%% stop_monitor

stop_monitor(S) ->
  case (S#server.template)#template.monitor of
    [] ->
      ok;
    _ ->
      supervisor:terminate_child(servant_monitor_sup,S),
      supervisor:delete_child(servant_monitor_sup,S)
  end.

%% start_link

start_link(S) ->
  gen_server:start_link(?MODULE,[S],[]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([S]) ->
  process_flag(trap_exit,true),
  T=S#server.template,
  spawn_monitor(S,T#template.monitor,[]).

%%
%% Name: spawn_monitor/3
%% Purpose: go through list of monitors and spawn them all
%%

spawn_monitor(S,[], PL) ->
    {ok,{S,PL,0}}
    ;
    
spawn_monitor(S,[H|T], PL) ->
    NL=PL++spawn_monitor(S,H),
    spawn_monitor(S,T,NL)
    .

spawn_monitor(S,X) ->
  ?TEMP_INFO(?F("spawn monitor ~p", [X])),
  Z=servant_util:extract_command([X],
				    servant_util:replacements(S, [{"@Parent", self()}])),
  ?TEMP_INFO(?F("spawn monitor response ~p", [Z])),
  receive after 1000 ->
  case Z of
    {module,Module,Function,Args} ->
      ?TEMP_INFO(?F("monitor spawned ~s ~s ~p", [ Module, Function, Args ])),
      Pid=proc_lib:spawn_link(Module,Function,Args),
      [Pid];
    {exec,Exec,Args} ->
      Port=open_port({spawn,Exec++" "++Args},[stream]),
      [Port]
  end
  end
  .

%%
%% Generic server call handling.
%% handle_call
%%

handle_call(Request,From,State) ->
  {reply,ok,State}.

%% handle_cast

handle_cast(Msg,State) ->
  {noreply,State}.

%% handle_info

handle_info({report,Info},State) ->
  do_report(Info,State);
handle_info({Ref,{data,Info}},State) ->
  do_report(Info,State);
handle_info({'EXIT',Ref,Reason},{S,Ref,N}) ->
  T=S#server.template,
  servant_util:run(T#template.stop,servant_util:replacements(S)),
  ?FATAL(?F("Monitor died (~p) ~s", [Reason,servant_util:format(S)])),
  servant_util:run(T#template.notify,
		   servant_util:replacements(S,[{"@Reason", ?F("\"~p\"",[Reason])}])),
  %servant_server:kill_monitor(S),
  {noreply,{S,Ref,N}};
handle_info(Info,State) ->
  {noreply,State}.

%%
%% Name: terminate
%% Purpose: terminate a given node(s)
%%

terminate(Reason,{S,Refs,N}) when list(Refs) ->
  lists:foreach(fun(Ref) -> terminate(Reason,{S,Ref,N}) end,Refs),
  ok;
terminate(Reason,{S,Pid,N}) when pid(Pid) ->
  exit(Pid,normal),
  ok;
terminate(Reason,{S,Port,N}) when port(Port) ->
  Port ! {self(),close},
  ok.

%%%----------------------------------------------------------------------
%%% Handler functions (internal to this module)
%%%
%%% These functions actually deal with the return values
%%% from the monitor. Hence if we want more complex responses
%%% to monitor then these need to be extended.
%%%----------------------------------------------------------------------

%% Standard monitor 'ok' handler
do_report(ok,{S,Ref,N}) ->
    {noreply,{S,Ref,0}}
    ;
%% Load information handler
do_report({ip_up,Load},{S,Ref,N}) ->
    % ?INFO(?F("monitor ip_up (~p) ~s", [Load,servant_util:format(S)])),
    % handle load information
    case global:whereis_name(load_server) of
        undefined ->
            erlang:send_after(?Ip_up_freq, self(), {report,{ip_up,Load}}),
            {noreply,{S,Ref,0}}
            ;
        Lpid ->
            ?INFO(?F("global load server ~p on node ~p", [ Lpid, node(Lpid) ])),
            gen_server:abcast(load_server, {Lpid, ip_up, Load}),
            {noreply,{S,Ref,0}}
    end
    ;
do_report({ip_down,Reason,Load},{S,Ref,N}) ->
    ?TEMP_INFO(?F("ip_down (~p) ~s", [Reason,servant_util:format(S)])),
    case global:whereis_name(load_server) of
        undefined ->
            {noreply,{S,Ref,0}}
            ;
        Lpid ->
            gen_server:abcast(load_server, {Lpid, ip_down, Load }),
            {noreply,{S,Ref,0}}
    end
    ;
do_report({load,Load},{S,Ref,N}) ->
    %?TEMP_INFO(?F("monitor load (~p) ~s", [Load,servant_util:format(S)])),
    % handle load information
    case global:whereis_name(load_server) of
        undefined ->
            {noreply,{S,Ref,0}}
            ;
        Lpid ->
            gen_server:abcast(load_server, {Lpid, ip_info, Load}),
            {noreply,{S,Ref,0}}
    end
    ;
%% Log monitoring failure - no action
do_report({log,Info},{S,Ref,N}) ->
  ?TEMP_INFO(?F("monitor reports (~p) ~s", [Info,servant_util:format(S)])),
  T=S#server.template,
  servant_util:run(T#template.notify,
        servant_util:replacements(S,[{"@Reason", ?F("\"~p\"", [Info])}]))
    ;
%% Stop / shutdown node as a result of failure
do_report({stop,Info},{S,Ref,N}) ->
    ?FATAL(?F("Monitor stops node (~p) ~s", [Info,servant_util:format(S)])),
    T=S#server.template,
    servant_util:run(T#template.notify,
        servant_util:replacements(S,[{"@Reason", ?F("\"~p\"", [Info])}])),
    % this suspends the entire node 
    oam:bootsuspend()
    ;
%% Restart server as a result of failure
do_report({restart,Info},X) ->
    do_report(Info,X)
    ;
%% In case of failure (or unhandled response)
do_report(Info,{S,Ref,N}) ->
  ?TEMP_INFO(?F("Monitor restarts server (~p) ~s", [Info,servant_util:format(S)])),
  T=S#server.template,
  servant_util:run(T#template.stop,servant_util:replacements(S)),
  case T#template.max_retries
    of
    N ->
      ?FATAL(?F("Monitor can't restart server: maximum restart frequency reached (~p) ~s", [N,servant_util:format(S)])),
      servant_util:run(T#template.notify,
        servant_util:replacements(S,[{"@Reason", ?F("\"~p\"", [Info])}])),
      servant_server:kill_monitor(S),
      {noreply,{S,Ref,N}};
    _ ->
      case servant_util:run(T#template.start,
			    servant_util:replacements(S)) of
	ok ->
	  {noreply,{S,Ref,N+1}};
	{error,Reason} ->
	  ?FATAL(?F("Event: Monitor can't restart server (~p) ~s",
		    [Reason,servant_util:format(S)])),
	  servant_util:run(T#template.notify,
	        servant_util:replacements(S,[{"@Reason", ?F("\"~p\"", [Reason])}])),
	  servant_server:kill_monitor(S),
	  {noreply,{S,Ref,N}}
      end
  end.
