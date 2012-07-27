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
%%% File    : master_heuristics.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created :  1 Jul 1998 by Joakim G. <jocke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(master_heuristics).
-author('jocke@erix.ericsson.se').
-vc('$Id: master_heuristics.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-export([suitable_nodes/3,available_ip_addresses/2]).

-include_lib("servant/include/db.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").
 
%% suitable_nodes

suitable_nodes(Ss,MostSuitableNodes,Cluster) ->
  FailoverNodes=
    case db:read({cluster,Cluster}) of
      [] ->
	[];
      [C] ->
	C#cluster.failover_nodes
    end,
  NodeActivity=
    lists:foldl(fun count_nodes/2,[],
		FailoverNodes++
		[StartedS#server.node ||
		  StartedS <- Ss,
		  lists:member(StartedS#server.node,FailoverNodes)]),
  SortedNodes=[Node || {Node,N} <- lists:keysort(2,NodeActivity)],
  MostSuitableNodes++(SortedNodes--MostSuitableNodes).

count_nodes(Node,CountedNodes) -> 
  case lists:keysearch(Node,1,CountedNodes) of
    {value,{Node,N}} ->
      lists:keyreplace(Node,1,CountedNodes,{Node,N+1});
    false ->
      [{Node,1}|CountedNodes]
  end.

%% available_ip_addresses

available_ip_addresses(Ss,T) ->
  case db:read({cluster,T#template.cluster}) of
    [] ->
      {error,unknown_cluster};
    [C] ->
      AvailableIPAddresses=
	[IPAddress ||
	  IPAddress <- C#cluster.ip_address_pool,
	  not(lists:keymember(IPAddress,#server.ip_address,Ss))],
      {ok,AvailableIPAddresses}
  end.
