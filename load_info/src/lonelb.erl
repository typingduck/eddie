-module(lonelb).
-author('Pekka@eddieware.org').
%%%----------------------------------------------------------------------
%%% File    : lone_lb.erl
%%% Author  : Pekka Hedqvist <Pekka@eddieware.org>
%%% Purpose : directly start lone loadbalancer from configfile
%%% Created : 22 Jul 1999 by Pekka Hedqvist <Pekka@eddieware.org>
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
%%% The Original Code is Eddie-1.0.1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------

-include("db.hrl").
-include("load_server.hrl").
-define(DEFAULT_LOAD_THRESHOLD,{1,3}). % taken from servant_server.erl
-export([sw_backend/2, conf_load_server/1]).

%% to be called from shell so to know howto start erl
sw_backend(FileName, HostName) ->
%% ecmd -s -sname zzz -a 'lonelb sw_backend ["ex1.mig", "hippo"]'
%% FileName "ex1.mig"
    {TemplateL, ClusterL, Root, NodeL} =get_mig_conf:get_config_t(FileName),
    MNNodeFun = fun(Node) -> NA = atom_to_list(Node),
			     string:substr(NA,string:rstr(NA,"@")+1)
		end,
    TemplateThisNodeL =
	[T || T <- TemplateL, MNNodeFun(T#template.node) == HostName],
    ADD_FE_L =
 	[T#template.node ||
 	    C <- ClusterL, N <- NodeL, T <- TemplateThisNodeL,
 	    C#cluster.cluster_type == backend,
 	    C#cluster.name         == T#template.cluster,
 	    N#node.name            == T#template.node],
    ADD_FE_L,
    case ADD_FE_L of
	[] -> badconfig;
	_Else -> T = atom_to_list(hd(ADD_FE_L)),
		  list_to_atom(
		    string:sub_string(T,1,string:str(T, "@") -1)) %yuck!
    end.

%called when load server application is up and running
conf_load_server(File) when list(File) ->
    %% filter here and filter there.. 
    {TemplateL, ClusterL, Root, NodeL} = get_mig_conf:get_config_t(File),
    MNNodeFun = fun(Node) -> NA = atom_to_list(Node),
			     string:substr(NA,string:rstr(NA,"@"))
		end,
    MNNode    = MNNodeFun(node()),
    TemplateThisNodeL =
	[T || T <- TemplateL, MNNodeFun(T#template.node) == MNNode],
    ADD_FE_L =
	[{T#template.ip_address, N#node.load_threshold} ||
	    C <- ClusterL, N <- NodeL, T <- TemplateThisNodeL,
	    C#cluster.cluster_type == frontend,
	    C#cluster.name         == T#template.cluster,
	    N#node.name            == T#template.node],
    ADD_BE_L =
	[{T#template.ip_address, N#node.load_threshold, T#template.node} ||
	    T <- TemplateThisNodeL, N <- NodeL, C <- ClusterL,
	    C#cluster.cluster_type == backend,	    
	    C#cluster.name         == T#template.cluster,
	    N#node.name            == T#template.node],
    BE_nodes = [N || {_, _, N} <- ADD_BE_L],
    ADD_FE_L2 = [{IP, Node, BE_nodes} || {IP, Node} <- ADD_FE_L],
    load_server:dns_servers(Root#root.dns_servers),
    lists:foreach(fun({IP, LoadThreshold, BE_NodeL}) ->
  			  load_server:add_fe(IP, LoadThreshold, BE_NodeL)
  		  end,
  		  ADD_FE_L2),
    lists:foreach(fun({IP, LoadThreshold,_}) ->
  			  load_server:add_be(IP, LoadThreshold)
  		  end,
 		  ADD_BE_L).
