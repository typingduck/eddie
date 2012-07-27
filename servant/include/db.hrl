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
%%% File    : db.hrl
%%% Author  : Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Purpose : Record definitions.
%%% Created : 26 Jun 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Generic record definition
%%%----------------------------------------------------------------------

-record(server,
	{node,
	 interface,
	 alias,
	 ip_address,
	 cluster,
	 template}).

%%%----------------------------------------------------------------------
%%% Mnesia table definitions
%%%----------------------------------------------------------------------

-define(TABLES,[root,node,cluster,template]).

-record(root,
	{name=main,
	 dns_servers=[],
	 netmask={255,255,255,0}}).

-record(node,
	{name,
	 interfaces=[],
	 load_threshold=0}).

-record(cluster,
	{name,
	 cluster_type=frontend,
	 failover_nodes=[],
	 backend_clusters=[],
	 ip_address_pool=[]}).

-record(template,
	{cluster,
	 start_order,
	 node,
	 interface="eth0",
	 auto_config=off,
	 ip_address=dynamic,
	 port=0,
	 start=[],
	 stop=[],
	 monitor=[],
	 max_retries=5,
	 notify=[]}). % bag



