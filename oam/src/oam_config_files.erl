-module(oam_config_files).
-author('tobbe@eddieware.org').
%%%----------------------------------------------------------------------
%%% File    : oam_config_files.erl
%%% Created : 16 Aug 1999 by tobbe@eddieware.org
%%% Function: Routines for writing config files.
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
-vc('$Id: oam_config_files.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').

-export([write/1,write/2]).

-include("db.hrl").
-include("../../inet_server/src/inet_server.hrl").

%%% --------------------------------------------------------------------
%%% Write the config data to the corresponding .mig/.gate files
%%% --------------------------------------------------------------------

write(PrefixName) when list(PrefixName) ->
    {ok,CurDir} = file:get_cwd(),
    write(PrefixName,CurDir).

write(PrefixName,Dir) when list(PrefixName),list(Dir) ->
    A=write_mig_file(PrefixName,Dir),
    B=write_gate_file(PrefixName,Dir),
    {A,B}.

write_mig_file(PrefixName,Dir) -> 
    Fname = PrefixName ++ ".mig",
    MigFile = build_mig_file(),
    write_file(Dir ++ "/" ++ Fname,MigFile).

write_gate_file(PrefixName,Dir) -> 
    Fname = PrefixName ++ ".gate",
    GateFile = build_gate_file(),
    write_file(Dir ++ "/" ++ Fname,GateFile).

%%% Have to use file:write/2 here since we 
%%% are dealing with a deep I/O list.
write_file(Fname,File) ->
    case file:open(Fname,write) of
	{ok,Fd} ->
	    file:write(Fd,File),
	    file:sync(Fd),
	    file:close(Fd),
	    ok;
	Else ->
	    Else
    end.

%%% --------------------------------------------------------------------
%%% Build a .gate file
%%% --------------------------------------------------------------------

build_gate_file() ->
    %% Will we ever get more than one frontend here... ?
    case oam_config:get_frontends() of
        {_, []} ->
            exit(no_frontend);
        {ok, DBFe} ->
            [Fe] = DBFe,
            ["<Frontend ",a2l(Fe#frontend.name),">\n\n",
             "  ProtocolModule ",a2l(Fe#frontend.protocol_module),"\n\n",
     build_endpoint_config(Fe),
     build_admit_info(Fe),
     build_erlets(),
%             build_backends(l2a(Fe#endpoint_config.name))],
             build_all_backends(Fe#frontend.endpoints_config), "\n</Frontend>\n"];
        {error, Reason} ->
            exit(Reason)
    end.


%%% The backend table is of type bag. That means that the key 
%%% 'endpoint_name' may result in a number of 'backend' records, 
%%% where each record represent a (endpoint_name,backend_node) 
%%% combination.  So to be able to build the schedule directive 
%%% we have to partition the set of records according to the 
%%% 'backend_node', and for each backend_node build the 
%%% corresponding Schedule directives.

build_all_backends([]) ->
    [];
build_all_backends(EndPoints) ->
    F = fun(E) -> build_backends(E#endpoint_config.name) end,
    lists:map(F, EndPoints).

build_backends(EPname) ->
    case oam_config:get_backends(EPname) of
        {ok, Back} ->
            Bs = lists:keysort(#backend.backend_node,Back),
            build_backend(EPname,Bs);
        {error, Reason} ->
            exit(Reason)
    end.

build_backend(EPname,Bs) ->
    {Set,Rest} = first_node_partition(Bs),
    build_backend(EPname,Set,Rest).

build_backend(_,[],[]) ->
    [];
build_backend(EPname,BeNodes,Bs) ->
%    F = fun(B) -> 
    [B | Others]=BeNodes,
    Be=["  \n  <Backend ",a2l(EPname)," ",a2l(B#backend.backend_node),">",
		 build_schedule(BeNodes),
		 "  </Backend>\n"]
%   end,
    ,
%    Be = lists:map(F,BeNodes),
    {Set,Rest} = first_node_partition(Bs),
    [Be|build_backend(EPname,Set,Rest)].

first_node_partition([]) ->
    {[], []};
first_node_partition([H|T]) ->
    Node = H#backend.backend_node,
    first_node_partition(Node,T,[H]).

first_node_partition(_,[],Acc) -> 
    {lists:reverse(Acc),[]};
first_node_partition(Node,[H|T],Acc) when H#backend.backend_node == Node ->
    first_node_partition(Node,T,[H|Acc]);
first_node_partition(Node,Rest,Acc) ->
    {lists:reverse(Acc),Rest}.

%%% The #backend.schedule_patterns field contains tuples
%%% looking like: {Key, {Patterns, AntiPatterns}}

build_schedule(BeNodes) ->
    F = fun(B) ->
       ["\n    <Schedule>\n",
        buildPatterns(B#backend.schedule_patterns),
	    build_schedule_type(B#backend.spec),
	    "    </Schedule>\n"]
	end,
    lists:map(F,BeNodes).

buildPatterns([]) ->
    [];
buildPatterns([Pat | Rest]) ->
       {Key,{Patterns,AntiPatterns}} = Pat, 
        BuiltPat = ["      Patterns ",a2l(Key)," ", pat(Patterns)," ",
            anti(AntiPatterns),"\n"],
        [BuiltPat | buildPatterns(Rest)].

pat([]) ->
    [];
pat([[]]) ->
    [];
pat(Patterns) ->
    F = fun(P) -> P++" " end,
    lists:map(F,Patterns).

anti([]) ->
    [];
anti([[]]) ->
    [];
anti(Patterns) ->
    F = fun(P) -> [$^|P]++" " end,
    lists:map(F,Patterns).

build_schedule_type({erlets,Erlets}) -> 
    ["      Erlets ",a2l(Erlets),"\n"];
build_schedule_type({proxy,Ip,Port}) -> 
    ["      Proxy ",ip2str(Ip)," ",i2l(Port),"\n"].

build_erlets() ->
    case oam_config:get_erlets() of
        {ok, Es} ->
    F = fun(E) when record(E,erlets) ->
                ["  \n  <Erlets ",a2l(E#erlets.name),">\n",
		 build_erlets_list(E#erlets.list),
		 "  </Erlets>\n"]
	end,
            lists:map(F,Es);
        {error, Reason} ->
            exit(Reason)
    end.

build_erlets_list(L) when list(L) ->
    F = fun(E) ->
        ["    <Erlet ",a2l(E),">\n",
		 "    </Erlet>\n"]
	end,
    lists:map(F,L).

build_admit_info(Fe) ->
    DBAc = oam_config:get_admit_ctrl(Fe#frontend.name),
    DBAlways = oam_config:get_admit_always(),
    DBNever = oam_config:get_admit_never(),
    case {DBAc, DBAlways, DBNever} of
        {{ok, Ac},{ok, Always},{ok, Never}} ->
            Admiss = [
                "  AdmitControl ",bool2str(Fe#frontend.admit_ctrl),"\n\n",
                "  AdmitTime ",i2l(Ac#admit_ctrl.time div 1000),"\n\n"],
            case Fe#frontend.admit_ctrl of
                true ->
                     MainAdmit = [
                     "  AdmitQueuePlaces ",i2l(Ac#admit_ctrl.queue_places),"\n\n",
                     "  AdmitMaxSessions ",i2l(Ac#admit_ctrl.max_sessions),"\n\n",
     "  AdmitBlockedPage ",Ac#admit_ctrl.blocked_page,"\n\n",
     "  AdmitRejectPage ",Ac#admit_ctrl.reject_page,"\n\n",
                     "  AdmitQueuePage ",Ac#admit_ctrl.queue_page,"\n\n"
],
%                     Admiss1 = addAlways(MainAdmit, Always),
%                     Admiss2 = addNever(Admiss1, Never),
                     Admiss1 = addNever(MainAdmit, Never),
                     Admiss2 = addAlways(Admiss1, Always),

                    Admiss ++ Admiss2;
                false ->
                    Admiss;
                _ ->
                    exit(wrong_admission_control_settings)
            end;
        _ ->
            exit(could_not_get_admission_control)
    end.

addAlways(MainAdmit, Always) when Always == [] ->
    MainAdmit;
addAlways(MainAdmit, Always) ->
    AlwaysString = ["  AdmitAlways ",static2str(Always),"\n\n"],
    [AlwaysString | MainAdmit].
     
addNever(MainAdmit, Never) when Never == [] ->
    MainAdmit;
addNever(MainAdmit, Never) ->
    NeverString = ["  AdmitNever ",static2str(Never),"\n\n"],
    [NeverString | MainAdmit].
     
static2str(L) when list(L) ->
    F = fun(A) when record(A,admit_static) ->
		Ip = A#admit_static.ip,
		[ip2str(Ip)," "]
	end,
    lists:map(F,L).

bool2str(true)  -> "true";
bool2str(false) -> "false".

build_endpoint_config(Fe) ->
    F = fun(E) ->
        Start = ["  <EndpointConfig ",a2l(E#endpoint_config.name),">\n",
         "    Port ",i2l(E#endpoint_config.port),"\n",
         "    ReadTimeout ",i2l(E#endpoint_config.read_timeout div 1000),"\n",
		 "    KeepAliveTimeout ",
         i2l(E#endpoint_config.keep_alive_timeout div 1000),"\n"],
         End = ["  </EndpointConfig>\n\n"],
         case E#endpoint_config.external_proxy of
            [] ->
                Start ++ End;
            _ ->
                External = ["    ExternalProxy ", 
                            E#endpoint_config.external_proxy, "\n"],
                Start ++ External ++ End
         end
	end,
    lists:map(F,Fe#frontend.endpoints_config).


%%% --------------------------------------------------------------------
%%% Build a .mig file
%%% --------------------------------------------------------------------

build_mig_file() ->
    DNS = build_dns_server(),
    Netmask = build_netmask(),
    Nodes = build_nodes(),
    Clusters = build_clusters(),
    [DNS,Nodes,Netmask,Clusters].

build_dns_server() ->
    case oam_config:get_dns_servers() of
	{ok,Ds} when list(Ds) ->
	    F = fun({Host,Port}) -> 
            PortList = i2l(Port),
            "DNSServer " ++ Host ++ " " ++ PortList ++ "\n\n"
		end,
	    lists:map(F,Ds);
	{error,Reason} ->
	    exit(Reason)
    end.
    
build_netmask() ->
    case oam_config:get_netmask() of
	{ok,{A,B,C,D}} ->
        "Netmask " ++ ip2dotted(A,B,C,D) ++ "\n\n";
	{error,Reason} ->
	    exit(Reason)
    end.

build_nodes() ->
    {ok,Nodes} = oam_config:get_node(),
    F = fun(N) ->
        ["<Node ", a2l(N#node.name),">\n",
		 "  Interfaces ",N#node.interfaces,"\n",
         "  LoadThreshold ",f2l_load(N#node.load_threshold),"\n",
		 "</Node>\n"]
	end,
    lists:map(F,Nodes).

build_clusters() ->
    {ok,Clusters} = oam_config:get_clusters(),
    F = fun(C) ->
		Servers = build_servers(C),
        ["\n<Cluster ",C#cluster.name,">\n",
		 "  ClusterType ",cluster_type(C),"\n\n",
		 failover_nodes(C),
		 backend_clusters(C),
         ip_address_pool(C),
		 Servers,
		 "\n</Cluster>\n\n"]
	end,
    lists:map(F,Clusters).

build_servers(C) ->
    Templates = oam_config:get_templates(C#cluster.name),
    F = fun(T) ->
        ["\n<Server ",a2l(T#template.node),">\n",
         auto_config(T)]++
	interface_line(T) ++
         ["  IPAddress ",ip2str(T#template.ip_address),"\n",
		 "  Port ",i2l(T#template.port),"\n",
%         "  Start ",script2str(T#template.start),"\n",
%         "  Stop ",script2str(T#template.stop),"\n",
	wrap_script2str(start,T),
	wrap_script2str(stop,T),
		 "</Server>\n"]
	end,
    lists:map(F,Templates).

interface_line(T) when T#template.auto_config == on ->
        ["  Interface ",T#template.interface,"\n"];
interface_line(T) ->
	[].

wrap_script2str(start, T) when T#template.start==[] ->
	[];
wrap_script2str(start, T)  ->
         ["  Start ",script2str(T#template.start),"\n"];
wrap_script2str(stop, T) when T#template.stop==[] ->
	[];
wrap_script2str(stop, T)  ->
         ["  Stop ",script2str(T#template.stop),"\n"].


script2str([{Node,module,M,F,A} | _]) ->
%    mod_script({Node,module,M,F,A});
    [a2l(Node)," module ",a2l(M)," ",a2l(F)," ",ll2ll_with_spaces(A)];
script2str([{Node,exec,E,A} | _]) ->
    [a2l(Node)," exec ",E," ",ll2ll_with_spaces(A)].

%mod_script({Node,module,M,F,A}) -> 
%    case oam_config:get_frontends() of
%        {_, []} ->
%            exit(no_frontend);
%        {ok, DBFe} ->
%            [Fe] = DBFe,
%            [a2l(Node)," module ",a2l(M)," ",a2l(F)," ",ll2ll_with_spaces(A)
%             ," ", a2l(Fe#frontend.name)];
%        {error, Reason} ->
%            exit(Reason)
%    end;
%mod_script(X) ->
%	[].

ip2str(dynamic)   -> "dynamic";
ip2str(List) when list(List) -> List;
ip2str({A,B,C,D}) -> [i2l(A),".",i2l(B),".",i2l(C),".",i2l(D)].

auto_config(T) when T#template.auto_config == off -> "  AutoConfig Off\n";
auto_config(T) when T#template.auto_config == on  -> "  AutoConfig On\n".
		 
backend_clusters(C) when C#cluster.cluster_type == frontend ->
    ["  BackendClusters",C#cluster.backend_clusters,"\n"];
backend_clusters(C) -> [].

ip_address_pool(C) when C#cluster.ip_address_pool == [] -> 
	[];
ip_address_pool(C) when C#cluster.cluster_type == frontend -> 
    ["  ipaddresspool ",
    string:strip(ip_tuples2dotted(C#cluster.ip_address_pool)),"\n"];
ip_address_pool(C) -> [].

failover_nodes(C) when C#cluster.cluster_type == frontend ->
    ["  FailoverNodes ",la2ll_with_spaces(C#cluster.failover_nodes),"\n"];
failover_nodes(C) -> [].

cluster_type(C) when C#cluster.cluster_type == frontend -> "Frontend";
cluster_type(C) when C#cluster.cluster_type == backend  -> "Backend".
    
ip_tuples2dotted([{A,B,C,D}|Rest]) ->
    [ip2dotted(A,B,C,D)++" "|ip_tuples2dotted(Rest)];
ip_tuples2dotted([]) -> [].

ip2dotted(A,B,C,D) ->
    i2l(A) ++ "." ++ i2l(B) ++ "." ++ i2l(C) ++ "." ++ i2l(D).

l2i(L) when list(L)    -> list_to_integer(L);
l2i(I) when integer(I) -> I.

i2l(I) when integer(I) -> integer_to_list(I);
i2l(I) when I == '*' -> "*";
i2l(L) when list(L)    -> L.

f2l(F) when float(F) -> float_to_list(F);
f2l(L) when list(L) -> L.

l2a(L) when list(L) -> list_to_atom(L);
l2a(A) when atom(A) -> A.

a2l(A) when atom(A) -> atom_to_list(A);
a2l(L) when list(L) -> L.

la2ll(ListOfAtom) ->
    lists:map(fun(A) -> a2l(A) end, ListOfAtom).

ll2ll_with_spaces(ListOfAtom) ->
    LL=lists:map(fun(A) -> A++" " end, ListOfAtom),
    filter_last_space_in_ll(LL).

la2ll_with_spaces(ListOfAtom) ->
    LL=lists:map(fun(A) -> a2l(A)++" " end, ListOfAtom),
    filter_last_space_in_ll(LL).

filter_last_space_in_ll([]) ->
    [];
filter_last_space_in_ll([L|[]]) ->
    [lists:sublist(L, length(L)-1)];
filter_last_space_in_ll([L|Ls]) ->
    [L|filter_last_space_in_ll(Ls)].

f2l_load({F1,F2}) when float(F2) ->
    float_to_list(F2);
f2l_load({F1,F2}) when list(F2) ->
    F2.

