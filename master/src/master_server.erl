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
%%%----------------------------------------------------------------------
%%% File    : master_server.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 1 Aug 1998 by Joakim G. <jocke@erix.ericsson.se>
%%%----------------------------------------------------------------------
%%% Apr 99 - dwyer@eddieware.org - status updates
%%% 21/07/00 clement.lyons@eddieware.org
%%% function not_started_servers/1 previously returned all the templates
%%% regardless even if the server was started. This caused problems bring
%%% up the interface of a  migrating node.

-module(master_server).
-author('jocke@force.du.etx.ericsson.se').
-vc('$Id: master_server.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-behaviour(gen_server).
-export([start_link/0,ping/1,resync/0,servers/0,status/0,resolve_global/3]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,ugly/2]).

-include_lib("servant/include/db.hrl").
-include_lib("misc/include/ip.hrl").
-include_lib("misc/include/logger.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").
 
-record(state,{global,servers=[],activity_id,resync_timer}).
-define(RESYNC_TIMEOUT,30000).

%% Interval between pings for down nodes.
-define(PING_PERIOD, 5000).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% start_link

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% ping

ping(Node) ->
  gen_server:cast({global,?MODULE},{ping,Node}).

%% resync

resync() ->
  gen_server:cast({global,?MODULE},resync).

%% servers

servers() ->
  gen_server:call({global,?MODULE},servers).

%% status

status() ->
  gen_server:call({global,?MODULE},status).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%% init

init([]) ->
  process_flag(trap_exit,true),
  net_kernel:monitor_nodes(true),
  mnesia:wait_for_tables(?TABLES,infinity),
  mnesia:set_master_nodes([]),
  mnesia:subscribe(system),
  lists:foreach(fun(Table) -> mnesia:subscribe({table,Table}) end,?TABLES),
  case global_register() of
    yes ->
      {ok,ResyncTimer}=timer:apply_interval(?RESYNC_TIMEOUT,?MODULE,resync,[]),
      servant_server:sync_load_server(),
      {ok,#state{global=yes,servers=sync(),resync_timer=ResyncTimer}};
    Pid ->
      servant_server:sync_load_server(),
      {ok,#state{global=Pid}}
  end.

global_register() ->
  global:sync(),
  case catch global:register_name(?MODULE,self(),{?MODULE,resolve_global}) of
    yes ->
      ?DIST_INFO(?F("~p is the newly elected master~n", [ node() ])),
      yes;
    no ->
      case catch global:whereis_name(?MODULE) of
	undefined ->
	  timer:sleep(500),
	  global_register();
	Pid ->
	  link(Pid),
	  Pid
      end
  end.

resolve_global(Name,Pid1,Pid2) ->
  {Min,Max}=
    if node(Pid1) < node(Pid2) ->
	{Pid1,Pid2};
       true ->
	{Pid2,Pid1}
    end,
  Max ! {resolve_name_conflict,Name,Min},
  Min.

%% handle_call

handle_call(servers,From,State) ->
  {reply,{ok,State#state.servers},State};
handle_call(status,From,State) ->
  {reply,format_status(State#state.servers),State}.  

%% handle_cast

handle_cast(resync,State) ->
  {noreply,State#state{servers=sync()}};
handle_cast({ping,Node},State) ->
  IPAddresses=
    db:eval(query
	      [T.ip_address ||
		T <- table(template),
		T.ip_address/=dynamic]
	    end),
  IPAddressPool=
    lists:flatten(db:eval(query
			    [C.ip_address_pool ||
			      C <- table(cluster),
			      C.ip_address_pool/=[]]
			  end)),
  PurgableIPAddresses=
    [IPAddress ||
      IPAddress <- misc:uniq_sort(IPAddresses++IPAddressPool),
      S <- State#state.servers,
      S#server.ip_address == IPAddress,
      not(same_host(S#server.node,Node))],
  servant_server:pong(Node,PurgableIPAddresses),
  {noreply,State#state{servers=takeover(Node,State#state.servers)}}.

same_host(Node1,Node2) ->
  [_,Host]=string:tokens(atom_to_list(Node1),"@"),
  case string:tokens(atom_to_list(Node2),"@") of
    [_,Host] ->
      true;
    _ ->
      false
  end.

%% handle_info

handle_info({nodeup,Node},State) ->
    {noreply, State};

handle_info({nodedown,Node},State) when State#state.global == yes ->
    case check_cluster_node(Node) of
	true ->
	    {noreply,State#state{
		       servers=failover(Node,State#state.servers)}};
	_ ->
	    {noreply,State}
    end;
handle_info({nodedown,Node},State) when Node == node(State#state.global) ->
  case global_register() of
    yes when reference(State#state.resync_timer) ->
      {noreply,#state{global=yes,servers=sync()}};
    yes ->
      {ok,ResyncTimer}=timer:apply_interval(?RESYNC_TIMEOUT,?MODULE,resync,[]),
      {noreply,#state{global=yes,servers=sync(),resync_timer=ResyncTimer}};
    Pid ->
      {noreply,#state{global=Pid}}
  end;
handle_info({nodedown,Node},State) ->
    {noreply, State};
handle_info({resolve_name_conflict,?MODULE,Pid},State) ->
    timer:cancel(State#state.resync_timer),
    link(Pid),
    rpc:abcast(nodes(),?MODULE,{self(),new_global,Pid}),
    {noreply,State#state{global=Pid}};
%handle_info({Pid,new_global,GlobalPid},State)
%  when Pid == State#state.global ->
%    unlink(Pid),
%    link(GlobalPid),
%    {noreply,State#state{global=GlobalPid}};
handle_info({Pid,new_global,GlobalPid},State) ->
    case self() of
	GlobalPid ->
	    {noreply, State};
	_ ->
	    unlink(Pid),
	    link(GlobalPid),
	    {noreply, State#state{global=GlobalPid}}
    end;
handle_info({'EXIT',Pid,Reason},State) when Pid == State#state.global ->
  case global_register() of
    yes when reference(State#state.resync_timer) ->
      {noreply,#state{global=yes,servers=sync()}};
    yes ->
      {ok,ResyncTimer}=timer:apply_interval(?RESYNC_TIMEOUT,?MODULE,resync,[]),
      {noreply,#state{global=yes,servers=sync(),resync_timer=ResyncTimer}};
    GlobalPid ->
      {noreply,#state{global=GlobalPid}}
  end;
handle_info({'EXIT',Pid,Reason},State) ->
  {noreply,State};
handle_info({mnesia_table_event,{Operation,Record,ActivityID}},State)
 when State#state.global == yes ->
  if
    State#state.activity_id == ActivityID ->
      {noreply,State};
    State#state.activity_id == undefined ->
      servant_server:sync_load_server(),
      {noreply,State#state{servers=sync(),activity_id=ActivityID}};
    true ->
      servant_server:sync_load_server(),
      {noreply,State#state{servers=sync(),activity_id=ActivityID}}
  end;
handle_info({mnesia_table_event,{Operation,Record,ActivityID}},State) ->
  if
    State#state.activity_id == ActivityID ->
      {noreply,State};
    State#state.activity_id == undefined ->
      servant_server:sync_load_server(),
      {noreply,State#state{activity_id=ActivityID}};
    true ->
      servant_server:sync_load_server(),
      {noreply,State#state{activity_id=ActivityID}}
  end;
% handle_info({mnesia_system_event,
% 	     {inconsistent_database,Context,Node}},State) ->
%   case State#state.global of
%     yes ->
%       {noreply,State};
%     Pid ->
%       case lists:member(node(Pid),mnesia:system_info(running_db_nodes)) of
% 	true ->
% 	  {noreply,State};
% 	false ->
% 	  case rpc:call(node(Pid),mnesia,system_info,[running_db_nodes]) of
% 	    {badrpc,Reason} ->
% 	      mnesia:set_master_nodes([node(Pid)]),
% 	      init:restart();
% 	    RunningDBNodes ->
% 	      mnesia:set_master_nodes(RunningDBNodes),
% 	      init:restart()
% 	  end,
%           {noreply,State}
%       end
%   end;
handle_info({mnesia_system_event,
 	     {inconsistent_database,Context,Node}},State) ->
    case mnesia:system_info(db_nodes) of
	[] ->
	    init:reboot();
	Nodes ->
	    reconcil_mnesia(lists:sort(Nodes))
    end,
    {noreply, State};

handle_info(Info,State) ->
  {noreply,State}.

%% terminate

terminate(Reason,State) ->
  if
    State#state.global == yes ->
      timer:cancel(State#state.resync_timer);
    true ->
      no_resync_timer
  end,
  mnesia:unsubscribe(system),
  lists:foreach(fun(Table) -> mnesia:unsubscribe({table,Table}) end,?TABLES),
  ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% sync

sync() ->
  StartedSs=started_servers(),
  PurgableSs=purgable_servers(StartedSs),
  stop_groups(grouping(PurgableSs)),
  Ss=StartedSs--PurgableSs,
  update_arp(Ss,PurgableSs),
  NotStartedTs=not_started_servers(Ss),
  Grouping = grouping(NotStartedTs),
  start_groups(Ss,Grouping).

update_arp(Ss,PurgableSs) ->
    Fun = fun(PuragableS) ->
		  case lists:keysearch(element(5,PuragableS),5,Ss) of
		      {value, S} ->
			  servant_server:update_arp(S#server.node,S#server.ip_address,S#server.interface);
		      ok ->
			  ok
		  end
	  end,
    lists:foreach(Fun,PurgableSs).

started_servers() ->
  lists:foldl(fun(Node,Ss) ->
		  case servant_server:servers(Node) of
		    {ok,StartedSs} ->
		      StartedSs++Ss;
		    {error,Reason} ->
		      Ss
		  end
	      end,[],[node()|nodes()]).

%%purgable_servers(StartedSs) ->
%%  db:eval(query
%%	    [StartedS ||
%%	      StartedS <- StartedSs,
%%	      not StartedS <- rule(valid)]
%%	  end).

%%-argtype({valid,server}).

%%valid(StartedS) :-
%%  C <- table(cluster),
%%  T <- table(template),
%%  StartedS.cluster=C,
%%  StartedS.template=T.

purgable_servers(StartedSs) ->
  Cs=db:eval(query [C || C <- table(cluster)] end),
  Ts=db:eval(query [T || T <- table(template)] end),
  purgable_servers(Cs,Ts,StartedSs).

purgable_servers(Cs,Ts,[]) ->
  [];
purgable_servers(Cs,Ts,[StartedS|Rest]) ->
  case lists:keymember(StartedS#server.template,#server.template,Rest) of
    false ->
      case lists:member(StartedS#server.cluster,Cs) of
	false ->
	  [StartedS|purgable_servers(Cs,Ts,Rest)];
	true ->
	  case lists:member(StartedS#server.template,Ts) of
	    false ->
	      [StartedS|purgable_servers(Cs,Ts,Rest)];
	    true ->
	      purgable_servers(Cs,Ts,Rest)
	  end
      end;
    true ->
      Template = StartedS#server.template,
      if
          StartedS#server.node == Template#template.node ->
              purgable_servers(Cs, Ts, Rest ++ [StartedS]);
          true ->
              [StartedS|purgable_servers(Cs,Ts,Rest)]
      end
  end.

grouping(TsSs) ->
  [GroupedTsSs ||
    {_,GroupedTsSs} <- lists:foldl(fun cluster_node_grouping/2,[],TsSs)].

cluster_node_grouping(T,GroupedTs) when record(T,template) ->
  Key={T#template.cluster,T#template.node},
  case lists:keysearch(Key,1,GroupedTs) of
    {value,{Key,Ts}} ->
      lists:keyreplace(Key,1,GroupedTs,{Key,[T|Ts]});
    false ->
      [{Key,[T]}|GroupedTs]
  end;
cluster_node_grouping(S,GroupedSs) when record(S,server) ->
  Key={(S#server.cluster)#cluster.name,(S#server.template)#template.node},
  case lists:keysearch(Key,1,GroupedSs) of
    {value,{Key,Ss}} ->
      lists:keyreplace(Key,1,GroupedSs,{Key,[S|Ss]});
    false ->
      [{Key,[S]}|GroupedSs]
  end.

%% grouping(Xs) ->
%%   lists:map(fun({IPAddress,SubXs}) ->
%% 		SubXs
%% 	    end,lists:foldl(fun ip_node/2,[],Xs)).

%% ip_node(X,GroupedXs) ->
%%   Key=
%%     case X of
%%       X when record(X,template) ->
%% 	{X#template.node,X#template.ip_address};
%%       X ->
%% 	{X#server.node,X#server.ip_address}
%%     end,
%%   case Key of
%%     {_,dynamic} ->
%%       [{dynamic,[X]}|GroupedXs];
%%     {Node,IPAddress} ->
%%       case lists:keysearch({Node,IPAddress},1,GroupedXs) of
%% 	{value,{{Node,IPAddress},Xs}} ->
%% 	  lists:keyreplace({Node,IPAddress},1,GroupedXs,
%% 			   {{Node,IPAddress},[X|Xs]});
%% 	false ->
%% 	  [{{Node,IPAddress},[X]}|GroupedXs]
%%       end
%%   end.

stop_groups([]) ->
  ok;
stop_groups([[S|Ss]|Rest]) ->
    SortedSs=lists:reverse(sort_servers([S|Ss])),
    case servant_server:stop_servers(S#server.node,SortedSs) of
	{ok,Status} ->
	    case lists:all(fun(Result) -> Result == ok end,Status) of
		true ->
		    ?DIST_INFO(?F("servers stopped ~s",
				  [servant_util:format(SortedSs)])),
		    stop_groups(Rest);
		false ->
		    ?DIST_ERROR(?F("All servers could not be stopped (reason: ~p) ~s",
				   [Status,servant_util:format(SortedSs)])),
		    stop_groups(Rest)
	    end
    end.

ugly(X,Y)-> 
    not lists:member(X,Y).

%% This function previously returned all the templates regardless
%% of wether a servant monitor was started for it or not.
not_started_servers(Ss) ->
    Ts = db:eval(query [Tget || Tget <- table(template)] end),
    Fun1 = fun(S,{T,AccSs}) -> 
		   case S#server.template of
		       T -> {T,AccSs++[S]};
		       _ -> {T,AccSs}
		   end
	   end,
    Fun2 = fun(T,{AllSs,AccNotStartedTs}) ->
		   case lists:foldl(Fun1,{T,[]},Ss) of
		       {_,[]} -> {AllSs,AccNotStartedTs++[T]};
		       _ -> {AllSs,AccNotStartedTs}
		   end
	   end,    
    {_,NotStartedTs} = lists:foldl(Fun2,{Ss,[]},Ts),
    NotStartedTs.

%not_started_servers(Ss) ->
%    db:eval(query
%            [T ||
%              T <- table(template),
%              master_server:ugly(S, Ss),
%              C=S.cluster,
%              T.cluster=C#cluster.name,
%              OriginT=S#server.template,
%              T.node=OriginT#template.node]
%    end).

%not_started_servers(Ss) ->
%  db:eval(query
%	    [T ||
%	      T <- table(template),
%	      not S <- Ss,
%	      C=S.cluster,
%	      T.cluster=C#cluster.name,
%	      OriginT=S#server.template,
%	      T.node=OriginT#template.node]	      
%	  end).

start_groups(Ss,[]) ->
  Ss;
start_groups(Ss,[[T|Ts]|Rest]) ->
  SortedTs=sort_templates([T|Ts]),
  {ok,AvailableIPAddresses}=master_heuristics:available_ip_addresses(Ss,T),
  SuitableNodes=
    master_heuristics:suitable_nodes(Ss,[T#template.node],T#template.cluster),
  case start_servers(SortedTs,AvailableIPAddresses,SuitableNodes) of
    {ok,NewSs} ->
      % ?INFO(?F("server started ~s", [servant_util:format(NewSs)])),
      start_groups(NewSs++Ss,Rest);
    {error,no_node_available} ->
      start_groups(Ss,Rest);
    {error,Reason} ->
      ?DIST_FATAL(?F("server could not be started (#1 reason: ~p) ~s",
		     [Reason,servant_util:format(SortedTs)])),
      start_groups(Ss,Rest)
  end.

sort_templates(Ts) ->
  lists:keysort(#template.start_order,Ts).

start_servers(Ts,AvailableIPAddresses,[]) ->
  {error,no_node_available};
start_servers(Ts,AvailableIPAddresses,[Node|Rest]) ->
  case servant_server:start_servers(Node,Ts,AvailableIPAddresses) of
    {ok,NewSs} ->
      {ok,NewSs};
    {error,{{nodedown,_},_}} ->
      start_servers(Ts,AvailableIPAddresses,Rest);
    {error,servant_not_started} ->
      start_servers(Ts,AvailableIPAddresses,Rest);
    {error,Reason} ->
      ?DIST_INFO(?F("servers could not be started (#4 reason: ~p) ~s",
		    [Reason,servant_util:format(Ts)])),
      start_servers(Ts,AvailableIPAddresses,Rest)
  end.

%% takeover

takeover(Node,StartedSs) ->
  PendingSs=
    db:eval(query
	      [PendingS ||
		PendingS <- StartedSs,
		PendingS.node /= Node,
		T=PendingS#server.template,
		T#template.node=Node]
	    end),
  stop_groups(grouping(PendingSs)),
  AllStartedSs=takeover_groups(StartedSs--PendingSs,grouping(PendingSs)),
  NotStartedTs=not_started_servers(AllStartedSs),
  start_groups(AllStartedSs,grouping(NotStartedTs)).

takeover_groups(Ss,[]) ->
  Ss;
takeover_groups(Ss,[[PendingS|PendingSs]|Rest]) ->
  Cluster=(PendingS#server.cluster)#cluster.name,
  MostSuitableNodes=[(PendingS#server.template)#template.node,
		     PendingS#server.node],
  SuitableNodes=
    master_heuristics:suitable_nodes(Ss,MostSuitableNodes,Cluster),
  case restart_servers(sort_servers([PendingS|PendingSs]),SuitableNodes) of
    {ok,NewSs} ->
      ?INFO(?F("server restarted ~s", [servant_util:format(NewSs)])),
      takeover_groups(NewSs++Ss,Rest);
    {error,Reason} ->
      ?DIST_FATAL(?F("server could not be restarted (#2 reason: ~p) ~s",
		     [Reason,servant_util:format([PendingS|PendingSs])])),
      takeover_groups(Ss,Rest)
  end.

sort_servers(Ss) ->
  SortedSs=
    lists:keysort(1,[{(S#server.template)#template.start_order,S} || S <- Ss]),
  [S || {StartOrder,S} <- SortedSs].

restart_servers(Ss,[]) ->
  {error,no_node_available};
restart_servers(Ss,[Node|Rest]) ->
  case servant_server:restart_servers(Node,Ss) of
    {ok,NewSs} ->
      {ok,NewSs};
    {error,{{nodedown,_},_}} ->
      restart_servers(Ss,Rest);
    {error,Reason} ->
      ?DIST_INFO(?F("server could not be restarted (#5 reason: ~p) ~s",
		    [Reason,servant_util:format(Ss)])),
      restart_servers(Ss,Rest)
  end.

%% failover

failover(Node,StartedSs) ->
  PendingSs=
    db:eval(query
	      [PendingS ||
		PendingS <- StartedSs,
		PendingS#server.node=Node]
	    end),
  failover_groups(StartedSs--PendingSs,grouping(PendingSs)).

failover_groups(Ss,[]) ->
  Ss;
failover_groups(Ss,[[PendingS|PendingSs]|Rest]) ->
  Cluster=(PendingS#server.cluster)#cluster.name,
  SuitableNodes=master_heuristics:suitable_nodes(Ss,[],Cluster),
  case restart_servers(sort_servers([PendingS|PendingSs]),SuitableNodes) of
    {ok,NewSs} ->
      ?INFO(?F("servers restarted ~s", [servant_util:format(NewSs)])),
      failover_groups(NewSs++Ss,Rest);
    {error,Reason} ->
      ?DIST_FATAL(?F("servers could not be restarted (#3 reason: ~p) ~s",
		     [Reason,servant_util:format([PendingS|PendingSs])])),
      failover_groups(Ss,Rest)
  end.

%% format_status

format_status(Ss) ->
  case db:read({root,main}) of
    [] ->
      "N/A\n";
    [R] ->
      Clusters=
        misc:uniq_sort([(S#server.template)#template.cluster || S <- Ss]),
      ?F("-- Master is running on node ~w~n~s",
         [node(),format_clusters(Ss,Clusters)])
  end.

format_clusters(Ss,[]) ->
  "";
format_clusters(Ss,[Cluster|Rest]) ->
  GroupedNodeSs=
    lists:foldl(fun server_grouping/2,[],
                [S ||
                  S <- Ss,
		  (S#server.template)#template.cluster == Cluster]),
  ?F("~n-- Servers running in cluster ~s:~n~n~s",
     [Cluster,format_servers(GroupedNodeSs)])++
    format_clusters(Ss,Rest).

format_servers([]) ->
  "";
format_servers([{Node,Ss}|Rest]) ->
  lists:foldl(fun(S,String) ->
		  ?F("  -- ~w (~w)~n"
		     "     ~s:~w (~s)~n"
		     "     ~s:~w (~s)~n",
		     [Node,
		      (S#server.template)#template.node,
		      S#server.interface,
		      S#server.alias,
		      (S#server.template)#template.interface,
		      ?IP2STR(S#server.ip_address),
		      ((S#server.template)#template.port),
		      ?IP2STR((S#server.template)#template.ip_address)])++
		    String
	      end,[],Ss)++
    format_servers(Rest).

server_grouping(S,GroupedSs) ->
  Node=S#server.node,
  case lists:keysearch(Node,1,GroupedSs) of
    {value,{Node,Ss}} ->
      lists:keyreplace(Node,1,GroupedSs,{Node,[S|Ss]});
    false ->
      [{Node,[S]}|GroupedSs]
  end.


%% Check if the specified node is part of the cluster.
check_cluster_node(Node) ->
    case mnesia:transaction(
	   fun () ->
		   mnesia:dirty_read(node, Node)
	   end) of
	{atomic, [_]} ->
	    true;
	_ ->
	    false
    end.


%% Pick the first node that ping successfully to be the master.
%% If no nodes can be picked, we reboot.
reconcil_mnesia([]) ->
    init:reboot();
reconcil_mnesia([Node|Rest]) when Node == node() ->
    resync(),
    ok;
reconcil_mnesia([Node|Rest]) ->
    case rpc:call(Node, mnesia, system_info, [running_db_nodes]) of
	{badrpc, Reason}->
	    reconcil_mnesia(Rest);
	Running_nodes ->
	    case lists:member(node(), Running_nodes) of
		true ->
		    ok;
		_ ->
		    mnesia:set_master_nodes(Running_nodes)
		    %init:restart()
	    end
    end.
