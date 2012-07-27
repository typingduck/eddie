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
%%% File    : get_mig_config.erl
%%% Author  : Pekka H <pekka@eddieware.orge>
%%% Created :  Jul 29 1999 by Pekka H <pekka@eddieware.orge>
%%% Original Author: Jocke Grebeno
%%% ----------------------------------------------------------------------

-module(get_mig_conf).
-include("logger.hrl").
-include("db.hrl").
-author('pekka@eddieware.org').

-export([get_config1/1, get_config/1, get_config_t/1, root/2]).
% get config from the parsed .mig file

get_config1(Body) ->
    put(start_order,0),
    case (catch root(#root{},Body)) of
	{error, Error} -> ?F("~p",[Error]);
	Config         -> lists:flatten(Config)
    end.

% get config provided a filename
get_config(ConfigFile) ->
    case config_file:parse(ConfigFile) of
	{ok, Body} ->
	    put(start_order,0),
	    case (catch root(#root{},Body)) of
		{error, Error} ->	
		    ?F("~p",[Error]);
		Config   ->
		    lists:flatten(Config)
	    end;
	{error, Reason} ->
	    ?F("~s: ~p",[ConfigFile,Reason])
    end.

% get config provided a filename, return them as 4 arity tuple 
% {TemplateL, ClusterL, Root, NodeL}
get_config_t(ConfigFile) ->
    Conf = get_config(ConfigFile),
    FilterFun = fun(Type, Con) -> [X||X <- Con, Type==element(1,X)]  end,
    TemplateL = FilterFun(template,Conf),
    ClusterL  = FilterFun(cluster,Conf),
    [Root]    = FilterFun(root,Conf),
    NodeL     = FilterFun(node,Conf),
    {TemplateL, ClusterL, Root, NodeL}.

root(R,[]) ->
    R;
root(R,[{Row,"DNSServer",[Host]}|Rest]) ->
    root(R#root{dns_servers=[{Host,0}|R#root.dns_servers]},Rest);
root(R,[{Row,"DNSServer",[Host,Port]}|Rest]) ->
    root(R#root{dns_servers=[{Host,numeric(Row,Port)}|
			     R#root.dns_servers]},Rest);
root(R,[{Row,"Netmask",[Netmask]}|Rest]) ->
    case get_addr([Netmask]) of
	{ok,[ConvertedNetmask]} ->
	    root(R#root{netmask=ConvertedNetmask},Rest);    
	{bad_ip_address,BadIPAddress} ->
	    throw ({error, "Bad IP address "++BadIPAddress++" on line "++
	     integer_to_list(Row)})
    end;
root(R,[{Row,Key,Values} | Rest]) ->
    throw({error, "Syntax error on line "++integer_to_list(Row)});
root(R,[{Row,Key,Values,[]} | Rest]) ->
    throw({error, "Empty context on line "++integer_to_list(Row)});
root(R,[{Row,"Node",[Name],Body} | Rest]) ->
    [snode(#node{name=list_to_atom(Name)},Body) | [root(R,Rest)]];
root(R,[{Row,"Cluster",[Name],Body} | Rest]) ->
    [cluster(#cluster{name=Name},Body) | [root(R,Rest)]];
root(R,[{Row,Key,Values,Body}|Rest]) ->
    throw({error,"Syntax error on line "++integer_to_list(Row)}).

%% node

snode(N,[]) -> N;
snode(N,[{Row,"Interfaces",Interfaces} | Rest]) ->
    snode(N#node{interfaces=Interfaces}, Rest);
snode(N,[{Row,"LoadThreshold",[Percentage, Value]} | Rest]) ->
    LoadT = check_load_t(Row,numeric(Row,Value)),
    snode(N#node{load_threshold={numeric(Row,Percentage), LoadT}}, Rest);
snode(N,[{Row,Key,Values}|Rest]) ->
    throw({error,"Syntax error on line "++integer_to_list(Row)});
snode(N,[{Row,Key,Values,[]}|Rest]) ->
    throw({error,"Empty context on line "++integer_to_list(Row)});
snode(N,[{Row,Key,Values,Body}|Rest]) ->
    throw({error, "Syntax error on line "++integer_to_list(Row)}).

%% Unacceptable with a load threshold
%% value less or equal to zero !!
check_load_t(_,LoadT) when LoadT > 0 -> 
    LoadT;
check_load_t(Row,LoadT) ->
    throw({error, "Load threshold on line " ++ integer_to_list(Row) ++ 
     " invalid, was: " ++ integer_to_list(LoadT)}).

%% cluster

cluster(C,[]) ->
    C;
cluster(C,[{Row,"ClusterType",["Frontend"]}|Rest]) ->
    cluster(C#cluster{cluster_type=frontend},Rest);
cluster(C,[{Row,"ClusterType",["Backend"]}|Rest]) ->
    cluster(C#cluster{cluster_type=backend},Rest);
cluster(C,[{Row,"FailoverNodes",NodeList}|Rest]) ->
    Nodes=lists:map(fun(Node) -> list_to_atom(Node) end,NodeList),
    cluster(C#cluster{failover_nodes=Nodes},Rest);
cluster(C,[{Row,"BackendClusters",BackendClusters}|Rest]) ->
    cluster(C#cluster{backend_clusters=BackendClusters},Rest);
cluster(C,[{Row,"IPAddressPool",IPAddressPool}|Rest]) ->
    case get_addr(IPAddressPool) of
	{ok,ConvertedIPAddressPool} ->
	    cluster(C#cluster{ip_address_pool=
			      ConvertedIPAddressPool},Rest);
	{bad_ip_address,BadIPAddress} ->
	    throw({error, "Bad IP address "++BadIPAddress++" on line "++
	     integer_to_list(Row)})
    end;
cluster(C,[{Row,Key,Values}|Rest]) ->
    throw({error, "Syntax error on line "++integer_to_list(Row)});
cluster(C,[{Row,Key,Values,[]}|Rest]) ->
    throw({error, "Empty context on line "++integer_to_list(Row)});
cluster(C,[{Row,"Server",[Node],Body}|Rest]) ->
    N=get(start_order)+1,
    put(start_order,N),
    [template(#template{cluster=C#cluster.name,
			node=list_to_atom(Node),
			start_order=N},Body)
     |
     [cluster(C,Rest)]];
cluster(C,[{Row,Key,Values,Body}|Rest]) ->
    throw({error, "Syntax error on line "++integer_to_list(Row)}).

%% template

template(T,[]) ->
    T;
template(T,[{Row,"Interface",[Interface]}|Rest]) ->
    template(T#template{interface=Interface},Rest);
template(T,[{Row,"AutoConfig",["On"]}|Rest]) ->
    template(T#template{auto_config=on},Rest);
template(T,[{Row,"AutoConfig",["Off"]}|Rest]) ->
    template(T#template{auto_config=off},Rest);
template(T,[{Row,"IPAddress",["dynamic"]}|Rest]) ->
    template(T#template{ip_address=dynamic},Rest);
template(T,[{Row,"IPAddress",[IPAddress]}|Rest]) ->
    case get_addr([IPAddress]) of
	{ok,[ConvertedIPAddress]} ->
	    template(T#template{ip_address=ConvertedIPAddress},Rest);    
	{bad_ip_address,BadIPAddress} ->
	    throw({error, "Bad IP address "++BadIPAddress++" on line "++
	     integer_to_list(Row)})
    end;
template(T,[{Row,"Port",[Port]}|Rest]) ->
    template(T#template{port=numeric(Row,Port)},Rest);
template(T,[{Row,"Start",[Node,"module",M,F|A]}|Rest]) ->
    template(T#template{start=[{list_to_atom(Node),
				module,
				list_to_atom(M),
				list_to_atom(F),
				A}|T#template.start]},Rest);
template(T,[{Row,"Start",[Node,"exec",Exec|Args]}|Rest]) ->
    template(T#template{start=[{list_to_atom(Node),
				exec,
				Exec,
				Args}|T#template.start]},Rest);
template(T,[{Row,"Stop",[Node,"module",M,F|A]}|Rest]) ->
    template(T#template{stop=[{list_to_atom(Node),
			       module,
			       list_to_atom(M),
			       list_to_atom(F),
			       A}|T#template.stop]},Rest);
template(T,[{Row,"Stop",[Node,"exec",Exec|Args]}|Rest]) ->
    template(T#template{stop=[{list_to_atom(Node),
			       exec,
			       Exec,
			       Args}|T#template.stop]},Rest);
template(T,[{Row,"Monitor",[Node,"module",M,F|A]}|Rest]) ->
    template(T#template{monitor=[{list_to_atom(Node),
				  module,
				  list_to_atom(M),
				  list_to_atom(F),
				  A}|T#template.monitor]},Rest);
template(T,[{Row,"Monitor",[Node,"exec",Exec|Args]}|Rest]) ->
    template(T#template{monitor=[{list_to_atom(Node),
				  exec,
				  Exec,
				  Args}|T#template.monitor]},Rest);
template(T,[{Row,"MaxRetries",[MaxRetries]}|Rest]) ->
    template(T#template{max_retries=list_to_integer(MaxRetries)},Rest);
template(T,[{Row,"Notify",[Node,"module",M,F|A]}|Rest]) ->
    template(T#template{notify=[{list_to_atom(Node),
				 module,
				 list_to_atom(M),
				 list_to_atom(F),
				 A}|T#template.monitor]},Rest);
template(T,[{Row,"Notify",[Node,"exec",Exec|Args]}|Rest]) ->
    template(T#template{notify=[{list_to_atom(Node),
				 exec,
				 Exec,
				 Args}|T#template.notify]},Rest);
template(T,[{Row,Key,Values}|Rest]) ->
    throw({error, "Syntax error on line "++integer_to_list(Row)});
template(T,[{Row,Key,Values,[]}|Rest]) ->
    throw({error,"Empty context on line "++integer_to_list(Row)});
template(T,[{Row,Key,Values,Body}|Rest]) ->
    throw({error,"Syntax error on line "++integer_to_list(Row)}).


%% numeric

numeric(Row,String) ->
    case catch list_to_integer(String) of
	{'EXIT',_} ->
	    case catch list_to_float(String) of
		{'EXIT',_} ->
		    throw({error, "Bad numeric on line "++integer_to_list(Row)});
		Float ->
		    Float
	    end;
	Integer ->
	    Integer
    end.

%% get_addr

get_addr(IPAddresses) ->    get_addr(IPAddresses,[]).
get_addr([],ConvertedIPAddress) ->    {ok,ConvertedIPAddress};
get_addr([IPAddress|Rest],ConvertedIPAddresses) ->
    case inet:getaddr(IPAddress,inet) of
	{ok,ConvertedIPAddress} ->
	    get_addr(Rest,[ConvertedIPAddress|ConvertedIPAddresses]);
	{error,Reason} ->
	    {bad_ip_address,IPAddress}
    end.




	    

