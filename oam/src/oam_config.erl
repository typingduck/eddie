-module(oam_config).
-author('tobbe@eddieware.org').
%%%----------------------------------------------------------------------
%%% File    : oam_config.erl
%%% Created : 12 Jul 1999 by tobbe@eddieware.org
%%% Function: Routines for manipulating the configuration data.
%%%----------------------------------------------------------------------
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
-vc('$Id: oam_config.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').

-export([get_dns_servers/0,set_dns_servers/1,get_netmask/0,set_netmask/1,
	 add_new_node/1,get_node_interfaces/1,set_node_interfaces/2,
	 get_node_load_threshold/1,set_node_load_threshold/2,get_frontends/0,
	 add_new_cluster/1,get_cluster/1,add_new_template/1,get_node/1,
	 get_templates/1,get_template/2,set_admit_ctrl/1,get_admit_ctrl/1,
	 add_admit_static/1,get_admit_static/1,set_frontend/1,get_clusters/0,
	 get_frontend/1,add_erlets/1,get_erlets/1,add_backend/1,get_erlets/0,
	 get_backends/0,get_backends/1,get_backends/2,set_cluster_type/2,
	 get_admit_never/0,get_admit_always/0,get_node/0,get_backends_node/1,
     mnesia_delete_table/1, save/10, open/1, test/0, test2/0, test3/0,clear/0,
     mnesia_exec/1]).

-include("db.hrl").
-include("inet_server.hrl").

-define( I(X), integer(X) ).
-define( IP(A,B,C,D), 
	 ?I(A),?I(B),?I(C),?I(D),A>=0,A<256,B>=0,B<256,C>=0,C<256,D>=0,D<256 ).

%%% --------------------------------------------------------------------
%%% Atomic save function
%%% --------------------------------------------------------------------
save(DnsServers, NetMask, NodeList, ClusterList, TemplateList, Admission1,
    Admission2, Frontend, Backends, Erlets) ->
    F = fun() ->
        setRootTable(DnsServers, NetMask),
        mnesia_delete_table(node),     % deletes contents of node table
        setNodeTable(NodeList),
        mnesia_delete_table(cluster),  % deletes contents of cluster table
        setClusterTable(ClusterList),
        checkClusters(ClusterList),      % checks that backend clusters 
                                         % mentioned in frontend cluster 
                                         % settings are there.
                                         % This check is done after all cluster
                                         % have been added.
        mnesia_delete_table(template),   % deletes contents of template table
        setTemplateTable(TemplateList),
        mnesia_delete_table(admit_ctrl), % deletes contents of admit_ctrl table
        set_admit_ctrl(Admission1),
        mnesia_delete_table(admit_static),  % deletes contents of admit_static 
                                            % table
        setAdmitStaticTable(Admission2),
        mnesia_delete_table(frontend),     % deletes contents of frontend table
        set_frontend(Frontend),
        mnesia_delete_table(backend),     % deletes contents of backend table
        setBackendTable(Backends),
        mnesia_delete_table(erlets),     % deletes contents of erlets table
        setErletsTable(Erlets)

    end,
    mnesia_exec(F).

clear() ->
    F = fun() ->
        mnesia_delete_table(node),     % deletes contents of node table
        mnesia_delete_table(template),    % deletes contents of template table
        mnesia_delete_table(admit_ctrl),  % deletes contents of admit_ctrl table
        mnesia_delete_table(admit_static),% deletes contents of admit_static 
                                          % table
        mnesia_delete_table(cluster)   % deletes contents of cluster table
    end,
    mnesia_exec(F).

%%% --------------------------------------------------------------------
%%% Atomic open function
%%% --------------------------------------------------------------------
%% Returns a tuple of data(below) or an error tuple: {error, message}
%% {[Host, Port], {int,int,int,int}}
%%      Host = string
%%      Port = int
open(ConfigName) when list(ConfigName) ->
    F = fun() ->
        {DNSStatus, DNSServers} = get_dns_servers(), 
        {NetMaskStatus, NetMask} = get_netmask(), 
        {NodesStatus, Nodes} = get_node(),
        {AdmitCtrlStatus, AdmitCtrl} = get_admit_ctrl(ConfigName),
        {AdmitNeverStatus, AdmitNever} = get_admit_never(),
        {AdmitAlwaysStatus, AdmitAlways} = get_admit_always(),
        {BackendsStatus, Backends} = get_backends(),
        {ClustersStatus, Clusters} = get_clusters(),
        {ErletsStatus, Erlets} = get_erlets(),
        {FrontendsStatus, Frontends} = get_frontends(),
        {TemplatesStatus, Templates} = get_all_templates(Clusters),
        case {DNSStatus, NetMaskStatus, NodesStatus, AdmitCtrlStatus,
              AdmitNeverStatus, AdmitAlwaysStatus, BackendsStatus,
              ClustersStatus, ErletsStatus, FrontendsStatus, TemplatesStatus} of
            {ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok} -> 
                {DNSServers, NetMask, Nodes, AdmitCtrl,
                 AdmitNever, AdmitAlways, Backends,
                 Clusters, Erlets, Frontends, Templates};
            X -> {error, {could_not_open_configuration,X}}
        end
    end,
    mnesia_exec(F).

get_all_templates(Clusters) ->
    get_all_templates(Clusters, []). 

get_all_templates([], TemplatesAcc) ->
    {ok, TemplatesAcc};
get_all_templates([C|Rest], TemplatesAcc) ->
    CurrTemplates = get_templates(C#cluster.name),
    case CurrTemplates of
        {error, _} -> {error, could_not_retrieve_templates};
        _ -> 
            NewAcc = lists:append(TemplatesAcc, CurrTemplates),
            get_all_templates(Rest, NewAcc)
    end.



%%% --------------------------------------------------------------------
%%% Operations towards the root record.
%%% --------------------------------------------------------------------

get_dns_servers() -> 
    case g_root() of
	{atomic,[R]} -> {ok,R#root.dns_servers};
	{_,Reason}   -> {error,Reason}
    end.

set_dns_servers(Servers) when list(Servers) -> 
    dns_servers_format(Servers),
    F = fun() -> 
		R = mnesia:read({root,main}),
        if 
            R == [] ->
                NewR1 = #root{name=main, dns_servers=Servers, netmask=[]},
		        mnesia:write(NewR1);
            true ->
                [OldR] = R,
                NewR2 = #root{name=OldR#root.name, dns_servers=Servers, 
                    netmask=OldR#root.netmask},
		        mnesia:write(NewR2)
        end
%		mnesia:write(R#root{dns_servers=Servers})
	end,
    mnesia_exec(F).

%%% Format should be a list of: {HostName,PortNo}

dns_servers_format([]) -> true;
dns_servers_format([{Host,Port}|T]) when list(Host),integer(Port) ->
    dns_servers_format(T);
dns_servers_format([H|_]) ->
    exit({error,{wrong_format,H}}).

get_netmask() -> 
    case g_root() of
	{atomic,[R]} -> {ok,R#root.netmask};
	{_,Reason}   -> {error,Reason}
    end.

set_netmask({A,B,C,D}) when ?I(A),?I(B),?I(C),?I(D) -> 
    F = fun() -> 
		R = mnesia:read({root,main}),
        if 
            R == [] ->
                NewR1 = #root{name=main, dns_servers=[], netmask={A,B,C,D}},
                mnesia:write(NewR1);
            true -> 
                [OldR] = R,
                NewR2 = #root{name=OldR#root.name, 
                    dns_servers=OldR#root.dns_servers, netmask={A,B,C,D}},
                mnesia:write(NewR2)
        end
%		mnesia:write(R#root{netmask={A,B,C,D}})
	end,
    mnesia_exec(F).

g_root() ->
    F = fun() -> mnesia:read({root,main}) end,
    mnesia:transaction(F).

%% DnsServers = [{Host, Port}]
%%      Host = string
%%      Port = int
%% Netmask = {int,int,int,int}
setRootTable(DnsServers, Netmask) when list (DnsServers) ->
    dns_servers_format(DnsServers),
    checkNetMask(Netmask),
    Root = #root{name=main, dns_servers=DnsServers, netmask=Netmask},
            mnesia:write(Root).

checkNetMask(NetMask) ->
    checkFourIntTuple(NetMask).

checkFourIntTuple({A,B,C,D}) when ?I(A), ?I(B), ?I(C), ?I(D)  ->
    true;
checkFourIntTuple(X) ->
    false.


%%% --------------------------------------------------------------------
%%% Operations towards the node record
%%% ----------------------------------
%%% Just for those curious people out there: I think that the 
%%% Interfaces directive really should be named FailoverInterfaces 
%%% to make it clear what it is used for (i.e if an interface doesn't
%%% work Eddie will try another alternative).
%%% --------------------------------------------------------------------

add_new_node(N) when record(N,node),atom(N#node.name),
		     list(N#node.interfaces) ->
    check_load(N#node.load_threshold),
    mnesia_write(N).

check_load({L1,L2}) when number(L1), number(L2) ->
    true;
check_load(_) ->
    false.

%%% Get all nodes 

get_node() ->
    W = mnesia:table_info(node,wild_pattern),
    F = fun() -> mnesia:match_object(W)	end,
    case mnesia:transaction(F) of
	{atomic,Nodes}   -> {ok,Nodes};
	{aborted,Reason} -> {error,Reason}
    end.

%%% Get a particular node

get_node(Name) when atom(Name) -> 
    case g_node(Name) of
	{atomic,[Node]}  -> {ok,Node};
	{aborted,Reason} -> {error,Reason}
    end.

get_node_interfaces(Name) when atom(Name) ->
    case g_node(Name) of
	{atomic,[N]} when record(N,node) -> 
	    {ok,N#node.interfaces};
	{_,Reason} ->
	    {error,Reason}
    end.

set_node_interfaces(Name,If) when atom(Name),list(If) ->
    F = fun() -> 
		case mnesia:read({node,Name}) of
		    [N] -> mnesia:write(N#node{interfaces=If});
		    _   -> mnesia:abort({node_not_found,Name})
		end
	end,
    mnesia_exec(F).
    
get_node_load_threshold(Name) when atom(Name) ->
    case g_node(Name) of
	{atomic,[N]} when record(N,node) -> 
	    {ok,N#node.load_threshold};
	{_,Reason} ->
	    {error,Reason}
    end.

set_node_load_threshold(Name,Load) when atom(Name),number(Load) ->
    F = fun() -> 
		case mnesia:read({node,Name}) of
		    [N] -> mnesia:write(N#node{load_threshold = {0,Load}});
		    _   -> mnesia:abort({node_not_found,Name})
		end
	end,
    mnesia_exec(F).
    
g_node(Name) ->
    F = fun() -> mnesia:read({node,Name}) end,
    mnesia:transaction(F).

%% [Node | Rest] = [#node]
setNodeTable([]) ->
    true;
setNodeTable([Node | Rest]) ->
    addSingleNode(Node),
    setNodeTable(Rest).

%% Node = #node
%% #node.name = atom
%% #node.interfaces = list of interface separated by a space, eg. "eth0 eth1"
%% #node.load_threshold = {float, float}
addSingleNode(Node) when record(Node, node), atom(Node#node.name), 
                        list(Node#node.interfaces) ->
    check_load(Node#node.load_threshold),
    mnesia:write(Node).

%%% --------------------------------------------------------------------
%%% Operations towards the cluster record.
%%% --------------------------------------------------------------------

add_new_cluster(C) when record(C,cluster),list(C#cluster.name),
			atom(C#cluster.cluster_type),
			list(C#cluster.failover_nodes),
			list(C#cluster.backend_clusters),
			list(C#cluster.ip_address_pool) ->
    check_cluster_type(C#cluster.cluster_type),
    check_ip_address_pool(C#cluster.ip_address_pool),
    check_failover_nodes(C),
    check_backend_clusters(C),
    mnesia_write(C).

get_cluster(ClusterName) when list(ClusterName) ->
    case g_cluster(ClusterName) of
	{atomic,[C]} -> {ok,C};
	_ -> throw({error,{non_existing_cluster,ClusterName}})
    end.

%%% Get all the clusters
get_clusters() ->
    F = fun() -> 
		W = mnesia:table_info(cluster,wild_pattern),
		mnesia:match_object(W)
	end,
    case mnesia:transaction(F) of
	{atomic,Clusters} -> {ok,Clusters};
	{_,Reason}        -> {error,Reason}
    end.

check_cluster_type(frontend) -> true;
check_cluster_type(backend)  -> true;
check_cluster_type(Type)     -> 
    exit({error,{wrong_cluster_type,Type}}).
    
check_ip_address_pool([]) -> true;
check_ip_address_pool([{A,B,C,D}|T]) when ?I(A),?I(B),?I(C),?I(D) ->
    check_ip_address_pool(T);
check_ip_address_pool([H|_]) -> 
    exit({error,{wrong_ip_address_format,H}}).
    
%%% Check that each node in the failover list, really is defined.

check_failover_nodes(C) ->
    F = fun(NodeName) ->
		case g_node(NodeName) of
            {atomic, []} -> throw({error,{non_existing_failover_node,
				       NodeName}});
		    {atomic,_} when atom(NodeName)-> ok;
		    _ -> throw({error,{non_existing_failover_node,
				       NodeName}})
		end
	end,
    case catch lists:foreach(F,C#cluster.failover_nodes) of
	{error,Reason} -> exit({error,Reason});
	_ -> true
    end.
		
%%% Check that each cluster in specified as backend clusters
%%% really is defined.

check_backend_clusters(C) when C#cluster.cluster_type == frontend ->
    F = fun(ClusterName) ->
		case g_cluster(ClusterName) of
            {atomic, []} -> throw({error,{non_existing_backend_cluster,
				       ClusterName}});
		    {atomic,_} -> ok;
		    _ -> throw({error,{non_existing_backend_cluster,
				       ClusterName}})
		end
	end,
    case catch lists:foreach(F,C#cluster.backend_clusters) of
	{error,Reason} -> exit({error,Reason});
	_ -> true
    end;
check_backend_clusters(C) when C#cluster.cluster_type == backend ->
    case {C#cluster.backend_clusters} of
        {[]} -> 
            true;
        _ ->
            exit({error,{cannot_define_backend_clusters,C#cluster.name}})
    end;
check_backend_clusters(C) ->
    exit({error,{cannot_define_backend_clusters,C#cluster.name}}).
    
set_cluster_type(Name,frontend) when list(Name) -> 
    set_cluster_type_0(Name,frontend);
set_cluster_type(Name,backend) when list(Name) -> 
    set_cluster_type_0(Name,backend).

set_cluster_type_0(Name,Type) ->
    F = fun() -> 
		case mnesia:read({cluster,Name}) of
		    [C] -> mnesia:write(C#cluster{cluster_type = Type});
		    _   -> mnesia:abort({cluster_not_found,Name})
		end
	end,
    mnesia_exec(F).

g_cluster(Name) ->
    F = fun() -> mnesia:read({cluster,Name}) end,
    mnesia:transaction(F).

%% [Cluster|Rest] = [#cluster]
setClusterTable([]) ->
    [];
setClusterTable([Cluster|Rest]) ->
    addSingleCluster(Cluster),
    setClusterTable(Rest).

%% Cluster = #cluster
%% #cluster.name = string
%% #cluster.cluster_type = atom
%% #cluster.failover_nodes = [atom]
%% #cluster.backend_clusters = [string]
%% #cluster.ip_address_pool = [{int,int,int,int}]
addSingleCluster(Cluster) when record(Cluster, cluster), 
                        list(Cluster#cluster.name),
                        atom(Cluster#cluster.cluster_type),
                        list(Cluster#cluster.failover_nodes),
                        list(Cluster#cluster.backend_clusters),
                        list(Cluster#cluster.ip_address_pool) ->
    check_cluster_type(Cluster#cluster.cluster_type),
    check_ip_address_pool(Cluster#cluster.ip_address_pool),
    check_failover_nodes(Cluster),
    mnesia:write(Cluster).

checkClusters([]) ->
    true;
checkClusters([C | Rest]) ->
    check_backend_clusters(C),
    checkClusters(Rest).


%%% --------------------------------------------------------------------
%%% Operations towards the template record.
%%% --------------------------------------------------------------------

%% Template = #template
%% Template#cluster = string
%% Template#start_order = it
%% Template#node = atom
%% Template#interface = string
%% Template#auto_config = on|off
%% Template#ip_address = dynamic | {int,int,int,int}
%% Template#port = int
%% Template#start = [{atom, 'module' (which is an atom), atom, atom, string}] |
%%                  [{atom, 'exec' (which is an atom), string, string}]
%% Template#stop = same as Template#start
%% Template#monitor = []
%% Template#max_retries = int
%% Template#notify = []
add_new_template(T) when record(T,template),list(T#template.interface),
			 ?I(T#template.port),T#template.port>=0,
			 ?I(T#template.max_retries) ->
    check_autoconfig(T),
    check_ip_address(T),
    check_start(T),
    check_stop(T),
    check_monitor(T),
    check_notify(T),
    F = fun() -> 
%		case mnesia:read({template,T#template.cluster}) of
%            [] ->
%			mnesia:write(T#template{start_order=1});
%		    Ts when list(Ts) ->
%			mnesia:write(T#template{start_order=(length(Ts) + 1)});
%		    _ ->
%			mnesia:abort({error,failed_to_read_template_table})
%		end
        mnesia:write(T)
	end,
    mnesia_exec(F).
		
get_templates(ClusterName) when list(ClusterName) ->
    case g_templates(ClusterName) of
	{atomic,Ts} -> Ts;
	_ -> {error,no_templates_found}
    end.

get_template(ClusterName,NodeName) when list(ClusterName),atom(NodeName) ->
    W = mnesia:table_info(template,wild_pattern),
    F = fun() -> 
		mnesia:match_object(W#template{cluster=ClusterName,
					       node=NodeName}) 
	end,
    case mnesia:transaction(F) of
	{atomic,[T]} when record(T,template) -> {ok,T};
	{atomic,Ts} when length(Ts)>1 -> 
	    %% NB: This shouldn't happen ! We include this
	    %% case only for debugging purposes...
	    exit({error,{identical_nodes_in_cluster,Ts}});
	Else ->
	    {error,no_template_found}
    end.

check_autoconfig(T) when T#template.auto_config == on  -> true;
check_autoconfig(T) when T#template.auto_config == off -> true;
check_autoconfig(T) -> 
    exit({error,{wrong_autoconfig_value,T#template.auto_config}}).

check_ip_address(T) when T#template.ip_address == dynamic -> true;
check_ip_address(T) when tuple(T#template.ip_address),
			 size(T#template.ip_address) == 4 ->
    case T#template.ip_address of
	{A,B,C,D} when ?IP(A,B,C,D) -> true;
	Else ->
	    exit({error,{wrong_ip_address_format,Else}})
    end;
check_ip_address(T) ->
    exit({error,{wrong_ip_address_format,T#template.ip_address}}).

check_start(T) when list(T#template.start) ->
    check_scripts(T#template.start).

check_stop(T) when list(T#template.stop) ->
    check_scripts(T#template.stop).

check_monitor(T) when list(T#template.monitor) ->
    check_scripts(T#template.monitor).

check_notify(T) when list(T#template.notify) ->
    check_scripts(T#template.notify).

check_scripts(List) when list(List) ->
    F = fun({Node,module,Mod,Fun,Args}) when atom(Node),atom(Mod),
					atom(Fun),list(Args) -> true;
	   ({Node,exec,Exec,Args}) when atom(Node),list(Exec),
					list(Args) -> true;
	   (Else) ->
		exit({error,{wrong_script_format,Else}})
	end,
    lists:foreach(F,List).

g_templates(ClusterName) when list(ClusterName) ->
    F = fun() -> mnesia:read({template,ClusterName}) end,
    mnesia:transaction(F).

%% [Template | Rest] = [#template]
setTemplateTable([]) ->
    true;
setTemplateTable([Template | Rest]) ->
    add_new_template(Template),
    setTemplateTable(Rest).


%%% --------------------------------------------------------------------
%%% Operations towards the admit_ctrl record.
%%% --------------------------------------------------------------------
%% A = #admit_ctrl
%% A#frontend = string
%% A#time = int (in milliseconds)
%% A#blocked_page = string
%% A#reject_page = string
%% A#queue_page = string
%% A#queue_places = int
%% A#max_sessions = int
%% A#qos_config = []
set_admit_ctrl(A) when record(A,admit_ctrl),
%		       list(A#admit_ctrl.frontend),
		       ?I(A#admit_ctrl.time),
		       list(A#admit_ctrl.blocked_page),
		       list(A#admit_ctrl.reject_page),
		       list(A#admit_ctrl.queue_page),
		       ?I(A#admit_ctrl.queue_places),
		       ?I(A#admit_ctrl.max_sessions) ->
    mnesia_write(A).

%get_admit_ctrl(FrontEnd) when list(FrontEnd) ->
get_admit_ctrl(FrontEnd) ->
    F = fun() -> mnesia:read({admit_ctrl,FrontEnd}) end,
    case mnesia:transaction(F) of
	{atomic,[A]} -> {ok,A};
	_ -> {error,not_found}
    end.

%%% --------------------------------------------------------------------
%%% Operations towards the admit_static record.
%%% --------------------------------------------------------------------

%% A = #admit_static
%% A#ip = a tuple of size 4
%% A#status = never | always
add_admit_static(A) when record(A,admit_static),
			 tuple(A#admit_static.ip),
			 size(A#admit_static.ip) == 4 ->
    check_status(A),
    mnesia_write(A).

get_admit_never()   -> get_admit_static(never).
get_admit_always()  -> get_admit_static(always).

get_admit_static(never)  -> get_admit_static_0(never);
get_admit_static(always) -> get_admit_static_0(always).

get_admit_static_0(Status) -> 
    W = mnesia:table_info(admit_static,wild_pattern),
    F = fun() -> mnesia:match_object(W#admit_static{status=Status}) end,
    case mnesia:transaction(F) of
	{atomic,As} -> {ok,As};
	_           -> {error,not_found}
    end.

    
check_status(A) when A#admit_static.status == never -> true;
check_status(A) when A#admit_static.status == always -> true;
check_status(A) -> 
    exit({error,{wrong_status,A#admit_static.status}}).

%% [A|Rest] = [#admit_static]
setAdmitStaticTable([]) ->
    true;
setAdmitStaticTable([A | Rest]) ->
    add_admit_static(A),
    setAdmitStaticTable(Rest).



%%% --------------------------------------------------------------------
%%% Operations towards the frontend record.
%%% --------------------------------------------------------------------

%%% NB: Tri ! The time values should be in milliseconds !!

%% Fe = #frontend
%% Fe#name=atom
%% Fe#protocol_module=atom
%% Fe#endpoints_config = [#endpoint_config]
%% Fe#admit_ctrl= true|false
%% Fe#arrival_counting_period = NOT USED
%% The endpoint_config record use the following types:
%%      #endpoint_config.name=atom
%%      #endpoint_config.port=int
%%      #endpoint_config.read_timeout=int (in milliseconds)
%%      #endpoint_config.keep_alive_timeout=int (in milliseconds)
%%      #endpoint_config.external_proxy=string
%set_frontend(Fe) when record(Fe,frontend),atom(Fe#frontend.name),
set_frontend(Fe) when record(Fe,frontend),
		      atom(Fe#frontend.protocol_module),
		      list(Fe#frontend.endpoints_config) ->
    check_endpoints_config(Fe#frontend.endpoints_config),
    check_admit_ctrl(Fe),
    mnesia_write(Fe).

get_frontends() ->
    F = fun() -> 
		W = mnesia:table_info(frontend,wild_pattern),
		mnesia:match_object(W) 
	end,
    case mnesia:transaction(F) of
	{atomic,Fes} -> {ok,Fes};
	{_,Reason}   -> {error,Reason}
    end.

%get_frontend(Name) when list(Name) ->
get_frontend(Name) ->
    F = fun() -> mnesia:read({frontend,Name}) end,
    case mnesia:transaction(F) of
	{atomic,[Fe]} -> {ok,Fe};
        X -> {error,not_found}
    end.

check_endpoints_config([]) -> true;
check_endpoints_config([E|Es]) when record(E,endpoint_config),
				    atom(E#endpoint_config.name),
				    ?I(E#endpoint_config.port), 
				    E#endpoint_config.port >= 0,
				    ?I(E#endpoint_config.read_timeout),
				    E#endpoint_config.read_timeout >= 0,
				    ?I(E#endpoint_config.keep_alive_timeout),
				    E#endpoint_config.keep_alive_timeout >= 0,
                    list(E#endpoint_config.external_proxy) ->
    check_endpoints_config(Es);
check_endpoints_config([E|_]) ->
    exit({error,{endpoint_config,E}}).

check_admit_ctrl(Fe) when Fe#frontend.admit_ctrl == true  -> true;
check_admit_ctrl(Fe) when Fe#frontend.admit_ctrl == false -> true;
check_admit_ctrl(Fe) ->
    exit({error,{admit_ctrl_not_boolean,Fe#frontend.admit_ctrl}}).

%%% --------------------------------------------------------------------
%%% Operations towards the backend record.
%%% --------------------------------------------------------------------

%% B = #backend
%% B#endpoint_name = atom
%% B#spec = {erlets (which is an atom), Name} | 
%%          {proxy(which is an atom), IP, port} 
%%      Name = atom
%%      IP = string
%%      port = int
%% B#backend_node = atom
%% B#schedule_patterns = [{key, {[pattern], [antipattern]}}] 
%%      key = atom
%%      pattern = string
%%      antipattern = string
add_backend(B) when record(B,backend),atom(B#backend.endpoint_name),
		    atom(B#backend.backend_node), list(B#backend.schedule_patterns) ->
    check_spec(B#backend.spec),
    check_schedule_patterns(B#backend.schedule_patterns),
    mnesia_write(B).

%%% Note that the 'backend' table is of type 'bag'.
%%% This means that a key may return several values.
%%% The (default) key is EndpointName = 'http_tcp' which
%%% also is the only possible key value so far (Aug-1999).
%%% To restrict outcome get_backends/2 gives the possibility
%%% to also specify a particula node. This "should" result
%%% in a unique backend record being returned.

get_backends() -> get_backends(http_tcp).

get_backends(EPname) when atom(EPname) ->
    W = mnesia:table_info(backend,wild_pattern),
    get_backends_0(W#backend{endpoint_name=EPname}).

get_backends_node(BeNode) when atom(BeNode) ->
    W = mnesia:table_info(backend,wild_pattern),
    get_backends_0(W#backend{backend_node=BeNode}).

get_backends(EPname,BeNode) when atom(EPname),atom(BeNode) ->
    W = mnesia:table_info(backend,wild_pattern),
    get_backends_0(W#backend{endpoint_name=EPname,
			     backend_node=BeNode}).

get_backends_0(W) when record(W,backend) ->
    F = fun() -> mnesia:match_object(W) end,
    case mnesia:transaction(F) of
	{atomic,Result} -> {ok,Result};
	_               -> {error,not_found}
    end.

%check_spec({proxy,{A,B,C,D},Port}) when ?IP(A,B,C,D),?I(Port) -> true;
check_spec({proxy,IP,Port}) when list(IP),?I(Port) -> true;
check_spec({erlets,Name}) when atom(Name)                     -> true;
check_spec(X) -> exit({error,{backend_sched_spec,X}}).

check_schedule_patterns([]) ->
    true;
check_schedule_patterns([Pat | Rest]) ->
    Result = check_schedule_pattern(Pat),
    case Result of
        true -> 
            check_schedule_patterns(Rest);
        _ ->
            exit({error,{wrong_schedule_pattern,Pat}})
    end.

check_schedule_pattern({Key,{Patterns,AntiPatterns}}) when atom(Key),
							   list(Patterns),
							   list(AntiPatterns) ->
    true;
check_schedule_pattern(Else) ->
    false.

%% [Be | Rest] = [#backend]
setBackendTable([]) -> 
    true;
setBackendTable([Be | Rest]) -> 
    add_backend(Be),
    setBackendTable(Rest).

    
%%% --------------------------------------------------------------------
%%% Operations towards the erlets record.
%%% NB: As it is today (12 Aug 1999) no #gen_storage{} records
%%%     are ever generated since no 'Erlet' directives exist.
%%%     So we don't bother implement operations for it.
%%% --------------------------------------------------------------------
%% E = #erlets
%% E#name = atom
%% E#list = [atom]
add_erlets(E) when record(E,erlets),atom(E#erlets.name),list(E#erlets.list) ->
    mnesia_write(E).

get_erlets() ->
    F = fun() ->
		W = mnesia:table_info(erlets,wild_pattern),
		mnesia:match_object(W)
	end,
    case mnesia:transaction(F) of
	{atomic,Es}      -> {ok,Es};
	{aborted,Reason} -> {error,Reason}
    end.

get_erlets(Name) when atom(Name) ->
    F = fun() -> mnesia:read({erlets,Name}) end,
    case mnesia:transaction(F) of
	{atomic,[E]} -> {ok,E};
	_            -> {error,not_found}
    end.

%% [E | Rest] = [#erlets]
setErletsTable([]) ->
    true;
setErletsTable([E | Rest]) ->
    add_erlets(E),
    setErletsTable(Rest).

%%%---------------------------------------------------------------------------
%%% Testing operations
%%%---------------------------------------------------------------------------
test() ->
Node1 = #node{name=fe1@machine1, interfaces="etho1 eth2", load_threshold=5.00},
Node2 = #node{name=fe2@machine1, interfaces="etho3 eth4", load_threshold=9.99},
Node3 = #node{name=be1@machine2, interfaces="etho5 eth6", load_threshold=9.99},
Node4 = #node{name=be2@machine2, interfaces="etho7 eth8", load_threshold=9.99},
Cluster1 = #cluster{name="frontendCluster1", cluster_type=frontend, 
            failover_nodes=[fe1@machine1, fe2@machine1], 
            backend_clusters=["backendCluster1"],
            ip_address_pool=[{1,1,1,1}, {2,2,2,2}]},
Cluster2 = #cluster{name="backendCluster1", cluster_type=backend, 
            failover_nodes=[], 
            backend_clusters=[],
            ip_address_pool=[{3,3,3,3}, {4,4,4,4}]},
Template1 = #template{cluster="backendCluster1",
	 start_order=1,
	 node=be1@machine4,
	 interface="eth0",
	 auto_config=off,
	 ip_address={9,9,9,9},
	 port=92,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=5,
	 notify=[]},
Template2 = #template{cluster="backendCluster1",
	 start_order=2,
	 node=be2@machine5,
	 interface="eth1",
	 auto_config=off,
	 ip_address={8,8,8,8},
	 port=91,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=7,
	 notify=[]},
Template3 = #template{cluster="frontendCluster1",
	 start_order=3,
	 node=fe2@machine3,
	 interface="eth0",
	 auto_config=on,
	 ip_address=dynamic,
	 port=92,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=8,
	 notify=[]},
Template4 = #template{cluster="frontendCluster1",
	 start_order=4,
	 node=fe1@machine3,
	 interface="eth0",
	 auto_config=on,
	 ip_address=dynamic,
	 port=93,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=9,
	 notify=[]},
Admission1 = #admit_ctrl{
    frontend="my_config",
	time=6000,
	blocked_page="/lod_blocked_page",
	reject_page="/lod_reject_page",
	queue_page="/lod_queue_page",
	queue_places=1000,
    max_sessions=50,
	qos_config=[]
    },
AStatic1 = #admit_static{ip={1,1,1,1}, status=never},
AStatic2 = #admit_static{ip={22,33,55,"*"}, status=never},
AStatic3 = #admit_static{ip={3,3,3,'*'}, status=always},
AStatic4 = #admit_static{ip={88,99,250,1}, status=always},
EndPoint1 = #endpoint_config{
                name=http_tcp,
                port=81,
                read_timeout=111111,
                keep_alive_timeout=22222,
                external_proxy="THis is the external proxy"},
EndPoint2 = #endpoint_config{
                name=http_tcp2,
                port=91,
                read_timeout=22222,
                keep_alive_timeout=33333,
                external_proxy="The second external proxy"},
Frontend = #frontend{
                name="my_config",
                protocol_module=http,
                endpoints_config=[EndPoint1, EndPoint2],
                admit_ctrl=true},
Backend1 = #backend{
                endpoint_name=http_tcp,
                spec={erlets, status},
                backend_node=be1@machine2,
                schedule_patterns=[{key1, {["pat1_1", "pat1_2"], 
                    ["antipat1_1", "antiPat1_2"]}}]},
Backend2 = #backend{
                endpoint_name=http_tcp,
                spec={proxy, {1,1,1,1}, 81},
                backend_node=be2@machine2,
                schedule_patterns=[{key2, {["pat2_1", "pat2_2"], 
                    ["antipat2_1", "antiPat2_2"]}}]},
Backend3 = #backend{
                endpoint_name=http_tcp,
                spec={proxy, {2,2,2,2}, 91},
                backend_node=be1@machine2,
                schedule_patterns=[{key3, {["pat3_1", "pat3_2"], 
                    ["antipat3_1", "antiPat3_2"]}}]},
Erlet1 = #erlets{name=erlet1,
                list=[list1]},
Erlet2 = #erlets{name=erlet2,
                list=[list2]},
Erlet3 = #erlets{name=erlet3,
                list=[list3]},
oam_config:save([{"hello.hello.com", 222},{"Eddieware.org", 999}], {9,9,9,9},
    [Node1, Node2, Node3, Node4], [Cluster1, Cluster2], [Template1, Template2, 
    Template3, Template4], Admission1, [AStatic1, AStatic2, AStatic3, 
    AStatic4], Frontend, [Backend1, Backend2, Backend3], [Erlet1, Erlet2,
    Erlet3]).


test2() ->
Node1 = #node{name=fe1@machine3, interfaces="interf1 interf2", 
    load_threshold=1.11},
Node2 = #node{name=fe2@machine3, interfaces="interf3 interf4", 
    load_threshold=2.22},
Node3 = #node{name=be1@machine4, interfaces="interf5 interf6", 
    load_threshold=9.99},
Node4 = #node{name=be2@machine5, interfaces="interf7 interf8", 
    load_threshold=9.99},
Cluster1 = #cluster{name="frontendCluster2", cluster_type=frontend, 
            failover_nodes=[fe1@machine3, fe2@machine3], 
            backend_clusters=["backendCluster2"],
            ip_address_pool=[{1,1,1,1}, {2,2,2,2}]},
Cluster2 = #cluster{name="backendCluster2", cluster_type=backend, 
            failover_nodes=[], 
            backend_clusters=[],
            ip_address_pool=[{3,3,3,3}, {4,4,4,4}]},
Template1 = #template{cluster="backendCluster2",
	 start_order=1,
	 node=be1@machine4,
	 interface="eth0",
	 auto_config=off,
	 ip_address={1,2,3,4},
	 port=80,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=5,
	 notify=[]},
Template2 = #template{cluster="backendCluster2",
	 start_order=2,
	 node=be2@machine5,
	 interface="eth1",
	 auto_config=off,
	 ip_address={5,6,7,8},
	 port=81,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=7,
	 notify=[]},
Template3 = #template{cluster="frontendCluster2",
	 start_order=3,
	 node=fe2@machine3,
	 interface="eth0",
	 auto_config=on,
	 ip_address=dynamic,
	 port=82,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=8,
	 notify=[]},
Template4 = #template{cluster="frontendCluster2",
	 start_order=4,
	 node=fe1@machine3,
	 interface="eth0",
	 auto_config=on,
	 ip_address=dynamic,
	 port=83,
	 start=[{generic,module,eddie,start,"@IPAddress @Port http"}],
	 stop=[{generic,module,eddie,stop,"@IPAddress @Port http"}],
	 monitor=[],
	 max_retries=9,
	 notify=[]},
Admission1 = #admit_ctrl{
    frontend="my_config",
	time=999999,
	blocked_page="/your_blocked_page",
	reject_page="/your_reject_page",
	queue_page="/lyourqueue_page",
	queue_places=88888,
    max_sessions=77,
	qos_config=[]
    },
AStatic1 = #admit_static{ip={2,2,2,2}, status=never},
AStatic2 = #admit_static{ip={66,77,88,"*"}, status=never},
AStatic3 = #admit_static{ip={5,5,5,'*'}, status=always},
AStatic4 = #admit_static{ip={00,11,250,2}, status=always},
EndPoint1 = #endpoint_config{
                name=another_ep_name,
                port=11,
                read_timeout=44444,
                keep_alive_timeout=5555555,
                external_proxy="other external"},
EndPoint2 = #endpoint_config{
                name=another_ep_name2,
                port=21,
                read_timeout=666666,
                keep_alive_timeout=777777,
                external_proxy="other external2"},
Frontend = #frontend{
                name="my_config",
                protocol_module=aaaa,
                endpoints_config=[EndPoint1, EndPoint2],
                admit_ctrl=false},
Backend1 = #backend{
                endpoint_name=http_tcp,
                spec={erlets, other_status},
                backend_node=be1@machine4,
                schedule_patterns=[{my_key1, {["My_pat1_1", "My_pat1_2"], 
                    ["My_antipat1_1", "My_antiPat1_2"]}}]},
Backend2 = #backend{
                endpoint_name=http_tcp,
                spec={proxy, {3,3,3,3}, 21},
                backend_node=be2@machine5,
                schedule_patterns=[{my_key2, {["My_pat2_1", "My_pat2_2"], 
                    ["My_antipat2_1", "My_antiPat2_2"]}}]},
Backend3 = #backend{
                endpoint_name=http_tcp,
                spec={proxy, {4,4,4,4}, 31},
                backend_node=be1@machine4,
                schedule_patterns=[{my_key3, {["My_pat3_1", "My_pat3_2"], 
                    ["My_antipat3_1", "My_antiPat3_2"]}}]},
Erlet1 = #erlets{name=my_erlet1,
                list=[my_list1]},
Erlet2 = #erlets{name=my_erlet2,
                list=[my_list2]},
Erlet3 = #erlets{name=my_erlet3,
                list=[my_list3]},
oam_config:save([{"bye.bye.com", 222},{"cya.com", 123}], {8,8,8,8},
    [Node1, Node2, Node3, Node4], [Cluster2, Cluster1], [Template1, Template2,
    Template3, Template4], Admission1, [AStatic1, AStatic2, AStatic3, 
    AStatic4], Frontend, [Backend1, Backend2, Backend3], [Erlet1, Erlet2, 
    Erlet3]).

test3() ->
    Config = open("my_config"),
    case Config of
        {error, Msg} -> 
            io:format("~w~n", [Config]);
        _ ->
            {ok,{DNSServers, NetMask, Nodes, AdmitCtrl,
             AdmitNever, AdmitAlways, Backends,
             Clusters, Erlets, Frontends, Templates}} = Config,
             io:format("DNS:~n"),
             printDNS(DNSServers),
             io:format("NETMASK:~n"),
             printNetmask(NetMask),
             io:format("NODES:~n"),
             printNodes(Nodes),
             io:format("ADMISSION CONTROL:~n"),
             printAdmission(AdmitCtrl, AdmitNever, AdmitAlways),
             io:format("CLUSTERS:~n"),
             printClusters(Clusters),
             io:format("FRONTENDS:~n"),
             printFrontends(Frontends),
             io:format("BACKENDS:~n"),
             printBackends(Backends),
             io:format("TEMPLATES:~n"),
             printTemplates(Templates),
             io:format("ERLETS:~n"),
             printErlets(Erlets)
    end.

printDNS([]) ->
    io:format("============================================================~n");
printDNS([{Host, Port}| Rest]) ->
    io:format("    ~s  ~w~n", [Host, Port]),
    printDNS(Rest).

printNetmask({A,B,C,D}) ->
    io:format("    {~w,~w,~w,~w}~n", [A,B,C,D]),
    io:format("============================================================~n").

printNodes([]) ->
    io:format("============================================================~n");
printNodes([N|Rest]) ->
    io:format("    Name:       ~w~n", [N#node.name]),
    io:format("    Interface:  ~s~n", [N#node.interfaces]),
    io:format("    Load Thres: ~w~n", [N#node.load_threshold]),
    io:format("------------------------------~n"),
    printNodes(Rest).

printAdmission(AdmitCtrl, Never, Always) ->
    printAdmit1(AdmitCtrl),
    printAdmit2(Never),
    printAdmit2(Always),
    io:format("============================================================~n").

printAdmit1(A) ->
    io:format("    Frontend:    ~s~n", [A#admit_ctrl.frontend]),
    io:format("    Time:        ~w~n", [A#admit_ctrl.time]),
    io:format("    BlockedPg:   ~s~n", [A#admit_ctrl.blocked_page]),
    io:format("    RejectPg:    ~s~n", [A#admit_ctrl.reject_page]),
    io:format("    QueuePg:     ~s~n", [A#admit_ctrl.queue_page]),
    io:format("    QueuePlaces: ~w~n", [A#admit_ctrl.queue_places]),
    io:format("    MaxSessions: ~w~n", [A#admit_ctrl.max_sessions]),
    io:format("    QOS Config:  ~w~n", [A#admit_ctrl.qos_config]),
    io:format("------------------------------~n").

printAdmit2([]) ->
    true;
printAdmit2([A|Rest]) ->
    io:format("    IP: ~w\t\tStatus: ~w~n", 
            [A#admit_static.ip, A#admit_static.status]),
    printAdmit2(Rest).

printClusters([]) ->
    io:format("============================================================~n");
printClusters([C|Rest]) ->
    io:format("    Name:           ~s~n", [C#cluster.name]),
    io:format("    Type:           ~w~n", [C#cluster.cluster_type]),
    io:format("    Failover Nodes: ~w~n", [C#cluster.failover_nodes]),
    io:format("    Backends:       "),
    printStringList(C#cluster.backend_clusters),
    io:format("    IP Pool:        ~w~n", [C#cluster.ip_address_pool]),
    io:format("------------------------------~n"),
    printClusters(Rest).


printFrontends([]) ->
    io:format("============================================================~n");
printFrontends([F|Rest]) ->
    io:format("    Name:                ~s~n", [F#frontend.name]),
    io:format("    Protocol:            ~w~n", [F#frontend.protocol_module]),
    io:format("    Admission:           ~w~n", [F#frontend.admit_ctrl]),
    io:format("    Arrival Acct Period: ~w~n", 
        [F#frontend.arrival_counting_period]),
    io:format("    EndPoints:~n"),
    printEndPoints(F#frontend.endpoints_config),
    io:format("------------------------------~n"),
    printFrontends(Rest).

printEndPoints([]) ->
    true;
printEndPoints([E|Rest]) ->
    io:format("        Name:               ~w~n", [E#endpoint_config.name]),
    io:format("        Port:               ~w~n", [E#endpoint_config.port]),
    io:format("        Read Timeout:       ~w~n", 
        [E#endpoint_config.read_timeout]),
    io:format("        Kepp Alive Timeout: ~w~n", 
        [E#endpoint_config.keep_alive_timeout]),
    io:format("        External Proxy:     ~s~n", 
        [E#endpoint_config.external_proxy]),
    io:format("    --------------------------~n"),
    printEndPoints(Rest).


printBackends([]) ->
    io:format("============================================================~n");
printBackends([B|Rest]) ->
    io:format("    Endpoint Name: ~w~n", [B#backend.endpoint_name]),
    printSpec(B#backend.spec),
%    io:format("    Spec:          ~w~n", [B#backend.spec]),
    io:format("    Backend Node:  ~w~n", [B#backend.backend_node]),
%    io:format("    Sch. Pattern:  w~n", [B#backend.schedule_patterns]),
    io:format("    Schedule Patterns:~n"),
    printSchPattern(B#backend.schedule_patterns),
    io:format("    --------------------------~n"),
    printBackends(Rest).

printSpec({erlets, Name}) ->
    io:format("    Spec:~n"),
    io:format("        Erlet Name: ~w~n", [Name]);
printSpec({proxy, IP, Port}) ->
    io:format("    Spec:~n"),
    io:format("        Name: ~w~n", [IP]),
    io:format("        Name: ~w~n", [Port]).

printSchPattern([]) ->
    true;
printSchPattern([{Key, {Pattern, AntiPattern}}|Rest]) ->
    io:format("        Key:           ~w~n", [Key]),
    io:format("        Patterns:      "),
    printStringList(Pattern),
    io:format("        AntiPatterns:  "),
    printStringList(AntiPattern),
    io:format("        ----------------------~n"),
    printSchPattern(Rest).

printTemplates([]) ->
    io:format("============================================================~n");
printTemplates([T|Rest]) ->
    io:format("    Cluster:     ~s~n", [T#template.cluster]),
    io:format("    Start Order: ~w~n", [T#template.start_order]),
    io:format("    Node:        ~w~n", [T#template.node]),
    io:format("    Interface:   ~s~n", [T#template.interface]),
    io:format("    AutoConfig:  ~w~n", [T#template.auto_config]),
    io:format("    Port:        ~w~n", [T#template.port]),
    io:format("    Start: ~n"),
    printStartStop(T#template.start),
    io:format("    Stop: ~n"),
    printStartStop(T#template.stop),
    io:format("    Monitor:     ~w~n", [T#template.monitor]),
    io:format("    Max Retries: ~w~n", [T#template.max_retries]),
    io:format("    Notify:      ~w~n", [T#template.notify]),
    io:format("    --------------------------~n"),
    printTemplates(Rest).

printStartStop([]) ->
    true;
printStartStop([{Node, module, Mod, Fun, Args}|Rest]) ->
    io:format("        Node:     ~w~n", [Node]),
    io:format("        Mod:      ~w~n", [Mod]),
    io:format("        Function: ~w~n", [Fun]),
    io:format("        Node:     ~s~n", [Args]),
    io:format("        ----------------------~n"),
    printStartStop(Rest);
printStartStop([{Node, exec, Exec, Args}|Rest]) ->
    io:format("        Node:     ~w~n", [Node]),
    io:format("        Exec:     ~s~n", [Exec]),
    io:format("        Args:     ~s~n", [Args]),
    io:format("        ----------------------~n"),
    printStartStop(Rest).

printErlets([]) ->
    io:format("============================================================~n");
printErlets([E|Rest]) ->
    io:format("    Name: ~w~n", [E#erlets.name]),
    io:format("    List: "),
    printAtomList(E#erlets.list),
    io:format("    --------------------------~n"),
    printErlets(Rest).
    

printAtomList(As) ->
    printAtomList(As,0).

printAtomList(As, Count) when Count > 55 ->
    io:format("~n                    "),
    printAtomList(As, 0);
printAtomList([], _) ->
    io:format("~n");
printAtomList([A|Rest], Count) ->
    io:format("~w, ", [A]),
    L = length(atom_to_list(A)),
    printAtomList(Rest, Count+L+2).
    
    



printStringList(Ss) ->
    printStringList(Ss,0).

printStringList(Ss, Count) when Count > 55 ->
    io:format("~n                    "),
    printStringList(Ss, 0);
printStringList([], _) ->
    io:format("~n");
printStringList([S|Rest], Count) ->
    io:format("~s, ", [S]),
    L = length(S),
    printStringList(Rest, Count+L+2).
    
    
    
%%% --------------
%%% Misc. routines

mnesia_write(Data) ->
    F = fun() -> mnesia:write(Data) end,
    mnesia_exec(F).

% deletes all entries of a given table
mnesia_delete_table(Table) ->
    F = fun() ->    
        Keys = (catch mnesia:all_keys(Table)),
        case Keys of
            {'EXIT', _} ->
                true;
            _ ->
                DelFunc = fun(K) -> mnesia:delete({Table, K}) end,
                lists:foreach(DelFunc, Keys)
        end
    end,
    mnesia_exec(F).


mnesia_exec(F) when function(F) ->
    case mnesia:transaction(F) of
	{atomic, A} -> {ok, A};
	{_,Reason} -> {error,Reason}
    end.


