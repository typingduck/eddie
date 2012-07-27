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
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%%     Created : 20 Aug 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%
%%% Contributor(s): Apr 2000 geoff@eddieware.rog
%%%     Changes so it'll start up initially even if other nodes
%%%     aren't running. These can be started later and incorporated
%%%     with a eddie config(). Probably should make them read their
%%%     config files and try to connect themselves.
%%% Contributor(s): Jan 2000 geoff@eddieware.rog
%%%     Lotsa changes to make booting smoother
%%% Contributor(s): Sep 1999 geoff@eddieware.rog
%%%     Startup stuff added.
%%% Contributor(s): Nov 1999 geoff@eddieware.rog
%%%     More startup stuff added.
%%%

-module(oam).
-author('jocke@erix.ericsson.se').
-export([started/0,schema/1,add/1,delete/1,application/1, nodelist/0,
         bootstrap/3, initiate/1, reconfig/2, bootstart/0, 
         dnsonly/0, dnsstart/0, daemon/0, dns_initiate/1,
         cluster_stop/0, init_db_schema/1, read_config/2, bootsuspend/0,
         installroot/0 ]).

-include("logger.hrl").

-include("db.hrl").
%-include("inet_server.hrl").

%%
%% Name: started
%% Purpose: this should be the last application started
%%  so when this can return 'yes' then the node should be up 
%%  and running.
%%

started() ->
  case init:get_status() of
    {started,_} ->
      yes;
    _ ->
      no
  end.

%%
%% schema
%%

schema(Nodes) ->
  case mnesia:delete_schema(Nodes) of
    ok ->
      case mnesia:create_schema(Nodes) of
        ok ->
	  rpc:multicall(Nodes,mnesia,start,[]),
	  ok;
	{error,Reason} ->
	  ?ERROR(?F("create_schema: ~p",[mnesia:error_description(Reason)]))
      end;
    {error,Reason} ->
      ?ERROR(?F("delete_schema: ~p",[mnesia:error_description(Reason)]))
  end.

%%
%% Name: add
%% Purpose: Add copies of mnesia tables to another node
%%

add(Node) ->
  Tables=servant_server:db_tables()++inet_server:db_tables(),
  db:wait_for_tables(Tables,5000),
  case mnesia:change_table_copy_type(schema,Node,disc_copies) of
    {atomic,ok} ->
      add_table_copies(Node,Tables);
    {aborted,Reason} ->
      ?F("mnesia_change_table_copy: ~p",[mnesia:error_description(Reason)])
  end.

add_table_copies(Node,[]) ->
  ok;
add_table_copies(Node,[Table|Rest]) ->
  case mnesia:add_table_copy(Table,Node,disc_copies) of
    {atomic,ok} ->
      add_table_copies(Node,Rest);
    {aborted,Reason} ->
      ?F("add_table_copy: ~p",[mnesia:error_description(Reason)])
  end.

%%
%% Name: delete
%% Purpose: delete copies of tables from a node.
%%

delete(Node) ->
  Tables=servant_server:db_tables()++inet_server:db_tables(),
  db:wait_for_tables(Tables,5000),
  case del_table_copies(Node,lists:delete(schema,
					  mnesia:system_info(tables))) of
    ok ->
      case mnesia:change_table_copy_type(schema,Node,ram_copies) of
	{atomic,ok} ->
	  case mnesia:del_table_copy(schema,Node) of
	    {atomic,ok} ->
	      ok;
	    {aborted,Reason} ->
	      ?F("del_table_copy: ~p",[mnesia:error_description(Reason)])
	  end;
	{aborted,Reason} ->
	  ?F("change_table_copy_type: ~p",[mnesia:error_description(Reason)])
      end;
    {error,Reason} ->
      ?F("del_table_copy: ~p",[Reason])
  end.

del_table_copies(Node,[]) ->
  ok;
del_table_copies(Node,[Table|Rest]) ->
  case mnesia:del_table_copy(Table,Node) of
    {atomic,ok} ->
      del_table_copies(Node,Rest);
    {aborted,Reason} ->
      {error,mnesia:error_description(Reason)}
  end.

%%
%% Name: delete_tables
%% Purpose: blow away all tables we're currently using
%%

delete_tables() ->
    Tables=servant_server:db_tables()++inet_server:db_tables(),
    db:wait_for_tables(Tables,5000),
    delete_tables(Tables)
    .

delete_tables([]) ->
    ok
    ;

delete_tables([Table|Rest]) ->
    case mnesia:delete_table(Table) of
        {atomic,ok} ->
            delete_tables(Rest);
        {aborted,Reason} ->
            {error,mnesia:error_description(Reason)}
    end.

%%
%% Name: distribute_tables/1
%% Purpose: actually blow away existing tables and
%%  recreate them distributed over all the nodes we have
%%  Ensures mnesia is started on all nodes.
%%

distribute_tables(N) ->
    rpc:multicall(N,mnesia,start,[]),
    delete_tables(),
    % hmm - should do something about failures here...
    % (ignore them?)
    case mnesia:change_config(extra_db_nodes, N) of
        { ok, RV } ->
            true;
        { error, Why } ->
            ?ERROR(?F("mnesia:change_config failed: ~p~n", Why));
        X ->
            ?FATAL("Unexpected return result from mnesia:change_config()~n")
    end,
    lists:map(fun(Node) ->
        io:format("fun Node = ~p~n", [ Node ]),
        XTC = node(),
        case Node of
        XTC ->
            true;
        _ ->
            mnesia:change_table_copy_type(schema,Node,disc_copies)
        end
    end, N),
    create_tables(N)
    .

%% application

application(Name) ->
  lists:keymember(Name,1,application:which_applications()).


%%
%% Name: sliver/1 & sliver/2
%% Purpose: take a list of records and slice out the node field
%%      and return a list of that particular field
%%

sliver(N) ->
    sliver(N,[])
    .

sliver([H|T], L) when record(H,node) ->
    sliver(T,  [ H#node.name | L ] )
    ;

sliver([], L) ->
    L
    .

%%
%% Name: nodelist/0
%% Purpose: return the current list of known nodes.
%%

nodelist() ->
    case oam_config:get_node() of
        { ok, NL } ->
            sliver(NL);
        { error, Why } ->
            ?INFO(?F("Unable to access mnesia database: ~p~n", [ Why ]))
    end
    .

%%
%% Name: bootstrap/3
%% Purpose: "boots" a cluster given configuration files
%%   Starting sequence:
%%      Load configuration files
%%      Find all the nodes (stored in the mnesia DB) from config file
%%      (Re)init db with new config files
%%      Done on an  "eddie config"
%% Note: assumes all nodes are already started in "daemon" mode.
%% 
%%

bootstrap(Gate, Mig, N) ->
    DNSonly = dnsonly(),
    case catch nodelist() of 
        { 'EXIT', Reason } ->
            % Mnesia isn't started - we're uninitiliased.
            %is_ok(mnesia:start()),
            %is_ok(reconfig(Gate, Mig)),
            %N = nodelist(),
            %mnesia:stop(),
            case DNSonly of
                true ->
                    dns_initiate(N),
                    % is_ok(reconfig(Gate, Mig)),
                    % Fire it up.
                    lonelb:conf_load_server(Mig)
                    ;
                false ->
                    %distribute_tables(N),
                    init_db_schema(N),
                    ?INFO(?F("Schema re-initialised on nodes ~p~n", [N])),
                    initiate(N),
                    is_ok(reconfig(Gate, Mig))
            end,
            'ok';
        _  ->
            % Otherwise re-read the config file
            Old = nodelist(),
            is_ok(reconfig(Gate, Mig)),
            New = nodelist(),
            % if Old & New differ (& New length > Old length) then redist tables
            Diff = Old -- New,
            case (Diff == []) and (length(New) > length(Old)) of
                true ->
                    distribute_tables(New),
                    is_ok(reconfig(Gate, Mig));
                false ->
                    true
            end,
            % check if master is running - if not re-start all apps
            Apps = application:which_applications(),
            case lists:keymember(master,1,Apps) of
                true ->
                    true;
                false ->
                    initiate(New)
            end,
            'ok'
    end
    .


%%
%% Name: initiate/1

%% Purpose: given a list of "live" Eddie nodes
%%   actually re-init their database schemas
%%   and reconfig them all so they should be running
%% Note: ignores failed nodes (without report)
%% Todo: Needs to check if a node is already running and
%%   note execute the rpcs if it is
%%

initiate(Nodes) ->
        lists:foreach(fun(Node)-> rpc:call(Node, oam, bootstart, []) end,Nodes).
    
%%
%% Name: dns_initiate/1
%% Purpose: start up load info stuff only
%%   Basically same as initiate but less stuff is started
%%

dns_initiate([]) ->
    ok
    ;
dns_initiate([H|T]) ->
    R = rpc:call(H, oam, dnsstart, []),
    case R of
        'ok' ->
            dns_initiate(T);
        _ ->
            R
    end
    .

%%
%% Name: is_ok/1
%% Purpose: check a return value is "ok" otherwise terminate this node
%%  after printing the error.
%%

is_ok(V) ->
    case V of
        ok ->
            true;
        { ok, Why } ->
            true;
        { error, Why } ->
            ?ERROR(?F("Error (oam.erl) detected: ~p", [ Why ] ));
        _ ->
            ?FATAL(?F("Error (oam.erl) return value not 'ok': ~p", [ V ] ))
    end
    .

%%
%% Name: init_db_schema/1
%% Purpose: (Re)initialize an Eddie Mnesia database for given nodes
%%      Only do those that are up and responding currently.
%%
%%

init_db_schema(N) ->
    % Creating schema
    { _, Down } = rpc:multicall(N, erlang, time, []),
    Up = N -- Down,
    is_ok(oam:schema(Up)),
    create_tables(Up)
    .

%%
%% Name: create_tables/1
%% Purpose: create tables for a given list of nodes
%%

create_tables(N) ->
    % Creating mandatory inet_server tables...
    case inet_server:create_tables(N) of
        ok ->
            % Creating servant tables...
            case servant_config:create_tables(N) of
                ok ->
                    ok;
                Why ->
                    { aborted, Why }
            end
            ;
        Why ->
            { aborted, Why }
    end
    .


%% 
%% Name: reconfig
%% Purpose: reconfig Eddie
%%

reconfig(Gate, Mig) ->
    R = inet_server:load(Gate),
    case R of
        'ok' ->
            X = servant_config:load(Mig),
            case X of
                'ok' ->
                    N = nodelist(),
                    ?INFO(?F("Reconfigured cluster, in config file: ~p", [N])),
                    'ok';
                _ ->
                    { error, X } 
            end;
        _ ->
            { error, R }
    end
    .

%%
%% Name: read_config/2
%% Purpose: start mnesia and read in config files (returning any errors)
%%

read_config(Gate, Mig) ->
    init_db_schema([node()]),
    mnesia:start(),
    reconfig(Gate, Mig),
    nodelist()
    .


%%
%% Name: daemon/0
%% Purpose: invoke in "daemon" mode, given configuration information
%%  try and restart the node with an existing database,
%%  otherwise create ourselves one and then wait ..
%%

daemon() ->
    % check for existing schema
    error_logger:add_report_handler(disk_log_handler, ["/tmp/daemonlog", verbose()]),
    ?INFO(?F("Node ~p entering daemon mode~n", [node()])),
    DNSonly=dnsonly(),
    mnesia:start(),
    case catch nodelist() of
        { 'EXIT', Reason } ->
            % Stop mnesia and wait until we're properly initialised
            %mnesia:stop(),
            % No node tables initialised - so create an empty single node 
            % init_db_schema([node()]),
            % Stop here so there's an error forcing a reconfig first time.
            mnesia:stop(),
            'ok';
        _ ->
            % io:format("Using existing Mnesia tables.\n"),
            case DNSonly of
                true ->
                    dnsstart();
                _ ->
                    bootstart()
            end,
            'ok'
    end
    .



%%
%% Name: bootstart
%% Purpose: start up the processes that make up Eddie
%%  here because we don't want to use the boot script to do it
%%  because we want to do some initialisation first ..
%%
%% Fix: os_mon won't start because disksup fails on unknown OS.
%% sasl - already started?
%%

bootstart() ->
    mnesia:start(),
    ?INFO(?F("~p is connected to the following nodes: ~p~n", [node(), nodes()])),
    appstart([os_mon,mnemosyne,sync_nodes,crypto,misc,lonelb,load_info,inet_server,servant,master,oam])
    .

%%
%% Name: dnsstart/0
%% Purpose: start up a load info node for the DNS server only
%%

dnsstart() ->
    % mnesia:start(),
    appstart([os_mon,crypto,misc,lonelb,load_info,oam]).


%%
%% Name: bootsuspend/0
%% Purpose: put it back in daemon waiting mode and wait for a new config
%%

bootsuspend() ->
    ?INFO(?F("Node ~p being suspended back to daemon mode", [ node() ])),
    appstop([oam,master,servant,inet_server,load_info,lonelb,misc,crypto,sync_nodes,mnemosyne,os_mon]),
    mnesia:stop(),
    'suspended'.


%%
%% Name: cluster_stop
%%

cluster_stop() ->
    ?INFO("Cluster stopped"),
    lists:map(fun (Node) ->
                case Node /= node() of
                true ->
                  case rpc:call(Node, oam, application, [servant]) of
                      true ->
                          rpc:call(Node, servant_server, abort, []);
                      _ ->
                          rpc:call(Node, init, stop, [])
                  end
                  ;
                _ ->
                    ok
                end
              end,
              nodelist()),
    % rpc:multicall(nodelist(), init, stop, []),
    servant_server:abort(),
    init:stop(),
    'ok'
    .


%%
%% Name: appstart
%% Purpose: startup a list of applications
%% Returns: 'ok' or the error that occurred.
%%

appstart([]) ->
    'ok'
    ;

appstart([H|T]) ->
    Res = application:start(H,permanent),
    case Res of
        'ok' -> 
            appstart(T);
        { E, Why } ->
            Why
    end
    .

%%
%% Name: appstop
%% Purpose: startup a list of applications
%% Returns: 'ok' or the error that occurred.
%%

appstop([]) ->
    'ok'
    ;

appstop([H|T]) ->
    Res = application:stop(H),
    case Res of
        'ok' -> 
            appstop(T);
        { E, Why } ->
            Why
    end
    .

%%
%% Help functions.
%%

tolower([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs])                       -> [C | tolower(Cs)];
tolower([])                           -> [].

%%
%% Configuration params.
%%

dnsonly() ->
    case application:get_env(oam,dnsonly) of
    {ok,X} -> X;
    undefined -> false
    end.

installroot() ->
    case application:get_env(oam,installroot) of
    {ok,X} -> X;
    undefined -> false
    end.

verbose() ->
    case application:get_env(verbose) of
    {ok, false} -> false;
    _           -> true
    end.

