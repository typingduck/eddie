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
%%% File    : servant_server.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 24 Jun 1998 by Joakim G. <jocke@erix.ericsson.se>
%%% Apr 99 - geoff@eddieware.org - Fixed (kindof) ifdel problem
%%%----------------------------------------------------------------------

-module(servant_server).
-author('jocke@erix.ericsson.se').
-vc('$Id: servant_server.erl,v 1.1 2000/10/27 22:20:27 dredd Exp $ ').
-behaviour(gen_server).
-export([start_link/0,servers/1,start_servers/3,restart_servers/2,
	 stop_servers/2,kill_monitor/1,pong/2,abort/0,db_tables/0,
	 sync_load_server/0,update_arp/3]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

-include("db.hrl").
-include("ip.hrl").
-include("logger.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").
 
-record(state,{connected=false,servers=[]}).

-define(POLL_TIMEOUT,5000).
-define(DEFAULT_LOAD_THRESHOLD,{1,3}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% start_link

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% servers

servers(Node) ->
  case rpc:call(Node,erlang,whereis,[?MODULE]) of
    Pid when pid(Pid) ->
      case catch gen_server:call({?MODULE,Node},servers,infinity) of
        {'EXIT',Reason} ->
	  {error,Reason};
	Reply ->
          Reply
      end;
    _ ->
      {error,servant_not_started}
  end.

%% start_servers

start_servers(Node,Ts,AvailableIPAddresses) ->
  case rpc:call(Node,erlang,whereis,[?MODULE]) of
    Pid when pid(Pid) ->
      case catch gen_server:call({?MODULE,Node},
				 {start_servers,Ts,AvailableIPAddresses},
                                 infinity) of
        {'EXIT',Reason} ->
	  {error,Reason};
	Reply ->
	  Reply
      end;
    _ ->
      {error,servant_not_started}
  end.

%% restart_servers

restart_servers(Node,Ss) ->
  case catch gen_server:call({?MODULE,Node},{restart_servers,Ss},infinity) of
    {'EXIT',Reason} ->
      {error,Reason};
    Reply ->
      Reply
  end.

%% update_arp

update_arp(Node,IPAddress,Interface) ->
  case catch gen_server:call({?MODULE,Node},{update_arp,IPAddress,Interface},infinity) of
    {'EXIT',Reason} ->
      {error,Reason};
    Reply ->
      Reply
  end.

%% stop_servers

stop_servers(Node,Ss) ->
  case catch gen_server:call({?MODULE,Node},{stop_servers,Ss},infinity) of
    {'EXIT',Reason} ->
      {error,Reason};
    Reply ->
      Reply
  end.

%% kill_monitor

kill_monitor(S) ->
  case catch gen_server:call(?MODULE,{kill_monitor,S},infinity) of
    {'EXIT',Reason} ->
      {error,Reason};
    Reply ->
      Reply
  end.

%% pong

pong(Node,PurgableIPAddresses) ->
  case catch gen_server:call({?MODULE,Node},{pong,PurgableIPAddresses},
			     infinity) of
    {'EXIT',Reason} ->
      {error,Reason};
    Reply ->
      Reply
  end.

%% abort

abort() ->
  case catch gen_server:call(?MODULE,abort,infinity) of
    {'EXIT',Reason} ->
      {error,Reason};
    Reply ->
      Reply
  end.

%% db_tables

db_tables() ->
  ?TABLES.

%% sync_load_server

sync_load_server() ->
  gen_server:cast(?MODULE,sync_load_server).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%% init

init([]) ->
  db:wait_for_tables(?TABLES,5000),
  timer:send_after(?POLL_TIMEOUT,self(),ping),
  {ok,#state{}}.

%% handle_call

handle_call(servers,From,State) ->
  {reply,{ok,State#state.servers},State};

% Start the service if not already started.
%
handle_call({start_servers,Ts,AvailableIPAddresses},From,State) ->
    {Started_servers, Not_started_ts} =
	separate_template(Ts, State#state.servers),
    case Not_started_ts of
	[] ->
	    {reply, {ok, Started_servers}, State};
	_ ->
	    case setup_network({AvailableIPAddresses,Not_started_ts}) of
		{ok,Network} ->
		    case setup_templates(Network) of
			{ok,Ss} ->
			    update_load_server(new,Ss),
                             ?INFO(?F("server started ~s", [servant_util:format(Ss)])),
			    {reply,{ok,Ss++Started_servers},
			     State#state{servers=Ss++State#state.servers}};
			{error,Reason} ->
			    takedown_network(Network),
			    {reply,{error,Reason},State}
		    end;
		{error,Reason} ->
		    {reply,{error,Reason},State}
	    end
    end;
%handle_call({start_servers,Ts,AvailableIPAddresses},From,State) ->
%  case setup_network({AvailableIPAddresses,Ts}) of
%    {ok,Network} ->
%      case setup_templates(Network) of
%	{ok,Ss} ->
%	  update_load_server(new,Ss),
%	  {reply,{ok,Ss},State#state{servers=Ss++State#state.servers}};
%	{error,Reason} ->
%	  takedown_network(Network),
%	  {reply,{error,Reason},State}
%      end;
%    {error,Reason} ->
%      {reply,{error,Reason},State}
%  end;
handle_call({restart_servers,Ss},From,State) ->
  case setup_network(Ss) of
    {ok,Network} ->
      case setup_servers(Network) of
	{ok,UpdatedSs} ->
	  update_load_server(new,UpdatedSs),
	  {reply,{ok,UpdatedSs},
	   State#state{servers=UpdatedSs++State#state.servers}};
	{error,Reason} ->
	  takedown_network(Network),
	  {reply,{error,Reason},State}
      end;
    {error,Reason} ->
      {reply,{error,Reason},State}
  end;
handle_call({update_arp,IPAddress,Interface},From,State) ->
    case get_hwaddr(Interface) of
	{ok,HWAddr} ->
	    case send_gratuitous_arp(Interface,HWAddr,IPAddress) of
		ok ->
		    {reply,ok,State};
		{error,Reason} ->
		    {reply,{error,Reason},State}
	    end;
	{error,Reason} ->
	    {reply,{error,Reason},State}
  end;
handle_call({stop_servers,Ss},From,State) ->
  Status=
    takedown_servers(Ss)++
    takedown_network([{S,
		       S#server.ip_address,
		       S#server.interface,
		       S#server.alias} ||
		       S <- Ss]),
  update_load_server(delete,Ss),
  {reply,{ok,Status},State#state{servers=State#state.servers--Ss}};
handle_call({kill_monitor,S},From,State) ->
  servant_monitor_server:stop_monitor(S),
  PendingSs=
    [PendingS ||
      PendingS <- State#state.servers,
      (PendingS#server.template)#template.node ==
	(S#server.template)#template.node,
      (PendingS#server.cluster)#cluster.name ==
	(S#server.cluster)#cluster.name],
  takedown_servers(PendingSs),
  takedown_network([{SS,
		     SS#server.ip_address,
		     SS#server.interface,
		     SS#server.alias} ||
		     SS <- PendingSs]),
  update_load_server(delete,PendingSs),
  ?INFO(?F("Event: Monitor killed itself~n"
	   "~s",
	   [servant_util:format(S)])),
  master_server:resync(),
  {reply,ok,State#state{servers=State#state.servers--PendingSs}};
handle_call({pong,PurgableIPAddresses},From,State) ->
  purge_ip_addresses(PurgableIPAddresses),
  {reply,ok,State#state{connected=true}};
handle_call(abort,From,State) ->
  cleanup(State),
  halt(),
  {reply,ok,State#state{servers=[]}}.

%% handle_cast

handle_cast(sync_load_server,State) ->
  case db:read({root,main}) of
    [] ->
      {noreply,State};
    [R] ->
      case catch load_server:dns_servers(R#root.dns_servers) of
	  {'EXIT', Reason} ->
	      {noreply,State};
	  _ ->
	      update_load_server(new,State#state.servers),
	      {noreply,State}
      end
  end.

%% handle_info

handle_info(ping,State) when State#state.connected == false ->
  master_server:ping(node()),
  timer:send_after(?POLL_TIMEOUT,self(),ping),
  {noreply,State};
handle_info(ping,State) ->
  {noreply,State};
handle_info(Info,State) ->
  {noreply,State}.

%% terminate

terminate(Reason,State) ->
  cleanup(State),
  ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% setup_network

setup_network({AvailableIPAddresses,Ts}) ->
  setup_network(AvailableIPAddresses,[],Ts);
setup_network(Ss) ->
  setup_network([],Ss).

setup_network(AvailableIPAddresses,Network,[]) ->
  {ok,lists:reverse(Network)};
setup_network(AvailableIPAddresses,Network,[T|Rest])
 when T#template.auto_config == on ->
  case lists:keysearch(T#template.ip_address,2,Network) of
    {value,{_,IPAddress,Interface,Alias}} ->
      setup_network(AvailableIPAddresses,
		    [{T,IPAddress,Interface,Alias}|Network],Rest);
    false ->
      case suitable_interfaces(T) of
	{ok,SuitableInterfaces} ->
	  case setup_interface(AvailableIPAddresses,T,SuitableInterfaces) of
	    {dynamic,{IPAddress,Interface,Alias}} ->
	      setup_network(lists:delete(IPAddress,AvailableIPAddresses),
			    [{T,IPAddress,Interface,Alias}|Network],Rest);
	    {static,{IPAddress,Interface,Alias}} ->
	      setup_network(AvailableIPAddresses,
			    [{T,IPAddress,Interface,Alias}|Network],Rest);
	    {error,Reason} ->
	      takedown_network(Network),
	      {error,Reason}
	  end;
	{error,Reason} ->
	  {error,Reason}
      end
  end;
setup_network(AvailableIPAddresses,Network,[T|Rest]) ->
  setup_network(AvailableIPAddresses,[{T,
				       T#template.ip_address,
				       T#template.interface,
				       none}|Network],Rest).

setup_network(Network,[]) ->
  {ok,lists:reverse(Network)};
setup_network(Network,[S|Rest])
 when (S#server.template)#template.auto_config == on ->
  case lists:keysearch(S#server.ip_address,2,Network) of
    {value,{_,IPAddress,Interface,Alias}} ->
      setup_network([{S,IPAddress,Interface,Alias}|Network],Rest);
    false ->
      case suitable_interfaces(S) of
	{ok,SuitableInterfaces} ->
	  case setup_interface(S,SuitableInterfaces) of
	    {ok,{IPAddress,Interface,Alias}} ->
	      setup_network([{S,IPAddress,Interface,Alias}|Network],Rest);
	    {error,Reason} ->
	      takedown_network(Network),
	      {error,Reason}
	  end;
	{error,Reason} ->
	  {error,Reason}
      end
  end;
setup_network(Network,[S|Rest]) ->
  setup_network([{S,S#server.ip_address,S#server.interface,none}|Network],
		Rest).

%% suitable_interfaces

suitable_interfaces(T) when record(T,template) ->
  if
    T#template.node == node() ->
      {ok,[T#template.interface]};
    true ->
      default_interfaces(node())
  end;
suitable_interfaces(S) ->
  if
    (S#server.template)#template.node == node() ->
      {ok,[(S#server.template)#template.interface]};
    true ->
      default_interfaces(node())
  end.

default_interfaces(Node) ->
  case db:read({node,Node}) of
    [] ->
      {error,unknown_node};
    [N] ->
      {ok,N#node.interfaces}
  end.

%% setup_interface

setup_interface(AvailableIPAddresses,T,[]) ->
  {error,out_of_interfaces};
setup_interface(AvailableIPAddresses,T,[Interface|Rest]) ->
  case T#template.ip_address of
    dynamic when AvailableIPAddresses == [] ->
      {error,out_of_ip_addresses};
    dynamic ->
      case configure_interface(hd(AvailableIPAddresses),Interface) of
	{ok,Alias} ->
	  {dynamic,{hd(AvailableIPAddresses),Interface,Alias}};
	{error,Reason} ->
	  ?INFO(?F("Event: Can't add ~s to interface ~s~n"
		   "Reason: ~p",
		   [?IP2STR(hd(AvailableIPAddresses)),Interface,Reason])),
	  setup_interface(AvailableIPAddresses,T,Rest)
      end;
    IPAddress ->
      case configure_interface(IPAddress,Interface) of
	{ok,Alias} ->
	  {static,{IPAddress,Interface,Alias}};
	{error,Reason} ->
	  ?INFO(?F("Event: Can't add ~s to interface ~s~n"
		   "Reason: ~p",
		   [?IP2STR(IPAddress),Interface,Reason])),
	  setup_interface(AvailableIPAddresses,T,Rest)
      end
  end.

setup_interface(S,[]) ->
  {error,out_of_interfaces};
setup_interface(S,[Interface|Rest]) ->
  case configure_interface(S#server.ip_address,Interface) of
    {ok,Alias} ->
      {ok,{S#server.ip_address,Interface,Alias}};
    {error,Reason} ->
      %?INFO(?F("Can't configure interface ~p: ~p", [ Interface, Reason ])),
      setup_interface(S,Rest)
  end.

configure_interface(IPAddress,Interface) ->
  case db:read({root,main}) of
    [] ->
      {error,unknown_root};
    [R] ->
      purge_ip_addresses([IPAddress]),
      case suitable_alias(Interface) of
	{ok,Alias} ->
	  case get_hwaddr(Interface) of
	    {ok,HWAddr} ->
	      case add_ip_address(Interface,Alias,IPAddress,R#root.netmask) of
		ok ->
		  case add_route(Interface,Alias,IPAddress) of
		    ok when HWAddr == "00:00:00:00:00:00" ->    
		      {ok,Alias};
		    ok ->
		      case send_gratuitous_arp(Interface,HWAddr,IPAddress) of
			ok ->
			  {ok,Alias};
			{error,Reason} ->
			  {error,Reason}
		      end;
		    {error,Reason} ->
		      {error,Reason}
		  end;
		{error,Reason} ->
		  {error,Reason}
	      end;
	    {error,Reason} ->
	      {error,Reason}
	  end;
	{error,Reason} ->
	  {error,Reason}
      end
  end.

suitable_alias(Interface) ->
  case reserved_aliases(Interface) of
    {ok,Aliases} ->
      {ok,pick_alias([0|misc:uniq_sort(Aliases)])};
    {error,Reason} ->
      {error,Reason}
  end.

reserved_aliases(Interface) ->
  Ifget=filename:join([code:priv_dir("servant"),"ifget"]),
  case os:cmd(io_lib:format("~s if ~s",[Ifget,Interface])) of
    [$i,$f,$g,$e,$t,$:,$ |Reason] ->
      {error,[$i,$f,$g,$e,$t,$:,$ |Reason]};
    Aliases ->
      AliasList=string:tokens(Aliases,"\n"),  
      {ok,lists:map(fun(Alias) -> list_to_integer(Alias) end,AliasList)}
  end.

pick_alias([N]) ->
  N+1;
pick_alias([N,M|Rest]) when N+1 == M ->
  pick_alias([M|Rest]);
pick_alias([N,M|Rest]) ->
  N+1.

get_hwaddr(Interface) ->
  Ifget=filename:join([code:priv_dir("servant"),"ifget"]),
  case os:cmd(io_lib:format("~s hw ~s",[Ifget,Interface])) of
    [$i,$f,$g,$e,$t,$:,$ |Reason] ->
      {error,Reason};
    "" ->
      {error,unknown_hwaddr};
    HWAddr ->
      {ok,HWAddr}
  end.

add_ip_address(Interface,Alias,IPAddress,Netmask) ->
  Ifadd=filename:join([code:priv_dir("servant"),"ifadd"]),
  case os:cmd(io_lib:format("~s ~s ~w ~s ~s",
			    [Ifadd,Interface,Alias,?IP2STR(IPAddress),
			     ?IP2STR(Netmask)])) of
    "" ->
      ok;
    Reason ->
      % ugly FreeBSD hack to ignore a stupid error message
      X = string:str(Reason, "File exists"),
      case X of
        0 ->
            {error,Reason};
        Z ->
            ok
      end
  end.

add_route(Interface,Alias,IPAddress) ->
  Routeadd=filename:join([code:priv_dir("servant"),"routeadd"]),
  case os:cmd(io_lib:format("~s ~s ~w ~s",		    
			    [Routeadd,Interface,Alias,?IP2STR(IPAddress)])) of
    "" ->
      ok;
    Reason ->
      {error,Reason}
  end.

send_gratuitous_arp(Interface,HWAddr,IPAddress) ->
  Garp=filename:join([code:priv_dir("servant"),"garp"]),
  case os:cmd(io_lib:format("~s ~s ~s ~s",
			    [Garp,Interface,HWAddr,?IP2STR(IPAddress)])) of
    "" ->
      ok;
    Reason ->
      {error,Reason}
  end.

%% takedown_network

takedown_network(Network) ->
  Ifdel=filename:join([code:priv_dir("servant"),"ifdel"]),
  takedown_network(Ifdel,misc:uniq_sort(2,Network)).

takedown_network(Ifdel,[]) ->
  [];
takedown_network(Ifdel,[{_,IPAddress,Interface,none}|Rest]) ->
  [ok|takedown_network(Ifdel,Rest)];
takedown_network(Ifdel,[{_,IPAddress,Interface,Alias}|Rest]) ->
  %?INFO(?F("Take down IP ~s", [?IP2STR(IPAddress)])),
  case os:cmd(io_lib:format("~s ~s ~w ~s",
			    [Ifdel,
			     Interface,
 			     Alias,
			     ?IP2STR(IPAddress)])) of
    "" ->
      [ok|takedown_network(Ifdel,Rest)];
    Reason ->
      [{error,Reason}|takedown_network(Ifdel,Rest)]
  end.

%% setup_templates

setup_templates([]) ->
  {ok,[]};
setup_templates([{T,IPAddress,Interface,Alias}|Rest]) ->
  case db:read({cluster,T#template.cluster}) of
    [] ->
      {error,unknown_cluster};
    [C] ->
      setup_templates(C,[],[{T,IPAddress,Interface,Alias}|Rest])
  end.

setup_templates(C,Ss,[]) ->
  {ok,lists:reverse(Ss)};
setup_templates(C,Ss,[{T,IPAddress,Interface,Alias}|Rest]) ->
  NewS=
    #server{node=node(),
	    interface=Interface,
	    alias=Alias,
	    ip_address=IPAddress,
	    cluster=C,
	    template=T},
  servant_util:run(T#template.stop,servant_util:replacements(NewS)),
  case servant_util:run(T#template.start,servant_util:replacements(NewS)) of
    ok ->
      case servant_monitor_server:start_monitor(NewS) of
	ok ->
	  setup_templates(C,[NewS|Ss],Rest);
	{error,Reason} ->
	  takedown_servers(Ss),
	  {error,Reason}
      end;
    {error,Reason} ->
      takedown_servers(Ss),
      {error,Reason}
  end.

%% setup_servers

setup_servers(Network) ->
  setup_servers([],Network).

setup_servers(Ss,[]) ->
  {ok,lists:reverse(Ss)};
setup_servers(Ss,[{S,IPAddress,Interface,Alias}|Rest]) ->
  UpdatedS=
    S#server{node=node(),
	     interface=Interface,
	     alias=Alias},
  servant_util:run((S#server.template)#template.stop,
		   servant_util:replacements(UpdatedS)),
  case servant_util:run((S#server.template)#template.start,
			servant_util:replacements(UpdatedS)) of
    ok ->
      case servant_monitor_server:start_monitor(UpdatedS) of
	ok ->
	  setup_servers([UpdatedS|Ss],Rest);
	{error,Reason} ->
	  takedown_servers(Ss),
	  {error,Reason}
      end;
    {error,Reason} ->
      takedown_servers(Ss),
      {error,Reason}
  end.

%% takedown_servers

takedown_servers([]) ->
  [];
takedown_servers([S|Rest]) ->
  [servant_monitor_server:stop_monitor(S),
   servant_util:run((S#server.template)#template.stop,
		    servant_util:replacements(S))|
   takedown_servers(Rest)].

%% cleanup

cleanup(State) ->
  update_load_server(delete,State#state.servers),
  takedown_servers(State#state.servers),
  takedown_network([{S,
		     S#server.ip_address,
		     S#server.interface,
		     S#server.alias} ||
		     S <- State#state.servers]),
  file:delete("/tmp/dummy_dets."++atom_to_list(node())).

%% update_load_server

update_load_server(_,[]) ->
  ok;
update_load_server(new,[S|Rest]) ->
  T=S#server.template,
  case T#template.node of
    Node when Node == node() ->
      case (S#server.cluster)#cluster.cluster_type of
	frontend ->
          case backend_nodes(T) of
            {backend_nodes,BackendNodes} ->
	      load_server:add_fe(S#server.ip_address,
                                 load_threshold(T#template.node),
				 BackendNodes);
	    {frontend_nodes,FrontendNodes} ->
              LoadThreshold=load_threshold(T#template.node),
%%              load_server:add_be(S#server.ip_address,LoadThreshold),
              load_server:add_fe(S#server.ip_address,LoadThreshold,
                                 FrontendNodes)
          end,
          update_load_server(new,Rest);
	backend ->
      ?INFO(?F("UPDATE_load_server (~p): ~p ~p", [ node(), S#server.ip_address, load_threshold(T#template.node) ])),
	  load_server:add_be(S#server.ip_address, load_threshold(T#template.node)),
	  update_load_server(new,Rest)
      end;
    Node ->
      case (S#server.cluster)#cluster.cluster_type of
	frontend ->
	  load_server:failover_fe(S#server.ip_address),
	  update_load_server(new,Rest);
	backend ->
	  %%catch load_server:failover_be(S#server.ip_address),	  
	  update_load_server(new,Rest)
      end
  end;
update_load_server(delete,[S|Rest]) ->
  T=S#server.template,
  case T#template.node of
    Node when Node == node() ->
      case (S#server.cluster)#cluster.cluster_type of
	frontend ->
	  load_server:delete_fe(S#server.ip_address),
	  update_load_server(delete,Rest);
	backend ->
	  load_server:delete_be(S#server.ip_address),
	  update_load_server(delete,Rest)
      end;
    Node ->
      case (S#server.cluster)#cluster.cluster_type of
	frontend ->
%%	  load_server:release_fe(S#server.ip_address),
	  update_load_server(delete,Rest);
	backend ->
	  %%catch load_server:release_be(S#server.ip_address),
	  update_load_server(delete,Rest)
      end
  end.

load_threshold(Node) ->
  case db:read({node,Node}) of
    [N] -> N#node.load_threshold;
    []  -> ?DEFAULT_LOAD_THRESHOLD
  end.

backend_nodes(T) ->
  Cluster=T#template.cluster,
  case db:read({cluster,Cluster}) of
    [] ->
      {backend_nodes,[]};
    [C] ->
      case C#cluster.backend_clusters of
        [] ->
          FrontendNodes=
	    db:eval(query
		      [TT.node ||
			TT <- table(template),
			TT.cluster=Cluster]
		    end),
          {frontend_nodes,FrontendNodes};
        Clusters ->
          BackendNodes=
	    db:eval(query
		      [TT.node ||
			TT <- table(template),
			CC <- Clusters,
			TT.cluster=CC]
		    end),
          {backend_nodes,BackendNodes}
      end
  end.

%% purge_ip_addresses

purge_ip_addresses(PurgableIPAddresses) ->
  Ifget=filename:join([code:priv_dir("servant"),"ifget"]),
  Ifdel=filename:join([code:priv_dir("servant"),"ifdel"]),
  purge_ip_addresses(Ifget,Ifdel,PurgableIPAddresses).

purge_ip_addresses(Ifget,Ifdel,[]) ->
  ok;
purge_ip_addresses(Ifget,Ifdel,[PurgableIPAddress|Rest]) ->
  case os:cmd(io_lib:format("~s ip ~s",[Ifget,?IP2STR(PurgableIPAddress)])) of
    [$i,$f,$g,$e,$t,$:,$ |Reason] ->
      ?INFO(?F("Event: Can't purge ~s~n"
	       "Reason: ~p",
	       [?IP2STR(PurgableIPAddress),Reason])),
      purge_ip_addresses(Ifget,Ifdel,Rest);
    "" ->
      purge_ip_addresses(Ifget,Ifdel,Rest);
    Interfaces ->
      delete_interfaces(Ifdel,string:tokens(Interfaces,"\n"), ?IP2STR(PurgableIPAddress)),
      purge_ip_addresses(Ifget,Ifdel,Rest)
  end.

delete_interfaces(Ifdel,[], _) ->
  ok;
delete_interfaces(Ifdel,[InterfaceAlias|Rest], IPAddr) ->
  case string:tokens(InterfaceAlias,":") of
    [Interface,Alias] ->
      os:cmd(io_lib:format("~s ~s ~s ~s",[Ifdel,Interface,Alias,IPAddr])),
      delete_interfaces(Ifdel,Rest,IPAddr);
    _ ->
      delete_interfaces(Ifdel,Rest,IPAddr)
  end.


%% We need to separate the templates that have already been started
%% from the templates that are not started. Return the servers that
%% belongs to the template that are started paired with the templates
%% that have not been started.
%%
separate_template([], Servers) ->
    {[], []};
separate_template([Template|Rest], Servers) ->
    {Started_servers, Unstarted_templates} = separate_template(Rest, Servers),
    case template_started(Template, Servers) of
	{yes, Server} ->
	    {[Server|Started_servers], Unstarted_templates};
	_ ->
	    {Started_servers, [Template|Unstarted_templates]}
    end.

%% If the template is already started, i.e. the server record has
%% the same template, {yes, Server} is returned. Otherwise, return no.
%%
template_started(Template, []) ->
    no;
template_started(Template, [Server|Rest])
  when Server#server.template == Template ->
    {yes, Server};
template_started(Template, [Server|Rest]) ->
    template_started(Template, Rest).
