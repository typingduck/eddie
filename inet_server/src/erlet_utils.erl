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
%%% Mar, Apr 99 - jon@eddieware.org
%%%

-module(erlet_utils).
-author('patrik@elrond').
-modified_by('Eric.Yeo@ericsson.com.au').
-vc('$Id: erlet_utils.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').

-include("inet_server.hrl").

-export([keysearch_value/2, keysearch_must_find/2, 
	 keysearch_default_find/3,
	 is_bool/1, is_int/1,
	 make_ip_list/1,
	 keysearch_all_values/2]).

keysearch_value(Key, Table) ->
    case lists:keysearch(Key, 1, Table) of
	{value, {_, Value}} ->
	    Value;
	_ ->
	    false
    end.

keysearch_must_find(Key, Table) ->
    case lists:keysearch(Key, 1, Table) of
	{value, {_, Value}} ->
	    Value;
	_ ->
	    exit(keynotfound)
    end.

keysearch_default_find(Key, Table, Default) ->
    case lists:keysearch(Key, 1, Table) of
	{value, {_, Value}} ->
	    Value;
	_ ->
	    Default
    end.

%% Return a list of values matching the key.
%%
keysearch_all_values(Key, [{Key, Value} | Rest]) ->
    [Value | keysearch_all_values(Key, Rest)];
keysearch_all_values(_, []) ->
    [];
keysearch_all_values(Key, [No_match | Rest]) ->
    keysearch_all_values(Key, Rest).

is_bool("true") ->
    true;
is_bool("false") ->
    true;
is_bool(_) ->
    false.

is_int([F|R]) when F >= $0, F =< $9 ->
    is_int(R);
is_int([]) ->
    true;
is_int([F|R]) ->
    false.
    
make_ip_list(IPs) ->
    case catch make_ip_list1(IPs) of
	false ->
	    false;
	Else ->
	    {ok ,Else}
    end.

make_ip_list1([Iplist1 | Iplistrest]) ->
    case ipv4_address(Iplist1) of
	{ok, {Ip1, '*', '*', '*'}} ->
	    [{Ip1, '*', '*', '*'} | make_ip_list1(Iplistrest)];
	{ok, {Ip1, Ip2, '*', '*'}} ->
	    [{Ip1, Ip2, '*', '*'} | make_ip_list1(Iplistrest)];
	{ok, {Ip1, Ip2, Ip3, '*'}} ->
	    [{Ip1, Ip2, Ip3, '*'} | make_ip_list1(Iplistrest)];
	{ok, {Ip1, Ip2, Ip3, Ip4}} when Ip1 =/= '*',
					Ip2 =/= '*',
					Ip3 =/= '*',
					Ip4 =/= '*' ->
	    [{Ip1, Ip2, Ip3, Ip4} | make_ip_list1(Iplistrest)];

	_ ->
	    throw(false)
    end;
make_ip_list1([]) ->
    [].

ipv4_address(Cs) ->
    case catch ipv4_addr(Cs, []) of
	{'EXIT',_} -> {error, einval};
	Addr -> {ok,Addr}
    end.

ipv4_addr([C | Cs], IP) when C >= $0, C =< $9 -> ipv4_addr(Cs, C-$0, IP);
%% Added clause (to allow * in the IP address).
ipv4_addr([$* | Cs], IP) -> ipv4_addr(Cs, '*', IP).

ipv4_addr([$.|Cs], '*', IP) -> ipv4_addr(Cs, ['*'|IP]);
ipv4_addr([$.|Cs], N, IP) when N < 256 -> ipv4_addr(Cs, [N|IP]);
ipv4_addr([C|Cs], N, IP) when C >= $0, C =< $9 ->
    ipv4_addr(Cs, N*10 + (C-$0), IP);
%% Added clause (to allow * in the IP address).
ipv4_addr([], '*', [C,B,A]) -> {A,B,C,'*'};
ipv4_addr([], D, [C,B,A]) when D < 256 -> {A,B,C,D}.

