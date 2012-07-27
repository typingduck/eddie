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
%%% File    : dns_cache.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : The DNS cache administrator.
%%% Created : 10 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Modified: 21 Feb 2000 by Pekka Hedqvist <pekka@eddieware.org>
%%% Done: The TTL refresh_cache that is initiated by REFRESH_TIME 
%%%        is done in a more space efficient manner
%%%
%%% TBD:
%%% Set an upper limit on the size of the cache and refresh when the
%%% limit is reached.
%%% REFRESH_TIME 24 hours, should be configurable ?
%%%----------------------------------------------------------------------

-module(dns_cache).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-vsn('$Revision: /main/eddie/eddie-1.0/4').
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/0, lookup/1, lookup/3, lookup/4, insert/1,
	 insert/2, clear/0, print/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {cache,
	        refresh_timer}).

-define(REFRESH_TIME, 1000 * 60 * 60 * 24).

-include("dns.hrl").
-include_lib("../../misc/include/logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, dns_cache}, dns_cache, [], []).

lookup(Domain) -> gen_server:call(dns_cache, {lookup, Domain}).

lookup(Domain, Class, Type) ->
    gen_server:call(dns_cache, {lookup, Domain, Class, Type}).

lookup(Domain, Class, Type, Time) ->
    gen_server:call(dns_cache, {lookup, Domain, Class, Type, Time}).

insert(RR) -> gen_server:call(dns_cache, {insert, RR}).

insert(Auth, RRs) -> gen_server:call(dns_cache, {insert, Auth, RRs}).

clear() -> gen_server:call(dns_cache, clear_cache).

print() -> gen_server:call(dns_cache, print).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    case ets:new(dns_cache, [named_table, {keypos, #dns_rr.domain}, bag]) of
	_ ->
	    Timer = erlang:send_after(?REFRESH_TIME, self(),
				      {timeout, refresh}),
	    {ok, #state{cache = dns_cache,
		        refresh_timer = Timer}};
	Error ->
	    ?FATAL(?F("Can't create cache table - ~p", [Error])),
	    {stop, Error}
    end.

handle_call({lookup, Domain}, From, State) ->
    R = case lookup_rr(State#state.cache, Domain) of
	    []  -> {error, nxdomain};
	    RRs -> {ok, RRs}
	end,
    {reply, R, State};

handle_call({lookup, Domain, Class, Type}, From, State) ->
    R = case lookup_rr(State#state.cache, Domain, Class, Type) of
	    []  -> {error, nxdomain};
	    RRs -> {ok, RRs}
	end,
    {reply, R, State};

handle_call({lookup, Domain, Class, Type, Time}, From, State) ->
    R = case lookup_rr(State#state.cache, Domain, Class, Type, Time) of
	    []  -> {error, nxdomain};
	    RRs -> {ok, RRs}
	end,
    {reply, R, State};

handle_call({insert, RR}, From, State) ->
    RR1 = lower_rr(RR),
    R = cache_rr(State#state.cache, RR1#dns_rr { tm = times() }),
    {reply, R, State};

handle_call({insert, Auth, RRs}, From, State) ->
    R = cache_rrs(Auth, RRs, State#state.cache),
    {reply, R, State};

handle_call(clear_cache, From, State) ->
    R = ets:match_delete(State#state.cache, '_'),
    {reply, R, State};

handle_call(print, From, State) ->
    F = fun(X) -> io:format("~p~n", [X]), continue end,
    traverse(State#state.cache, F),
    {reply, ok, State}.

%% This code is for the ets cache version.
traverse(Db, F) ->
    Key = ets:first(Db),
    traverse(Key, Db, F).

traverse('$end_of_table', _, _) ->
    ok;
traverse(Key, Db, F) ->
    RRs = ets:lookup(Db, Key),
    lists:foreach(F, RRs),
    NKey = ets:next(Db, Key),
    traverse(NKey, Db, F).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({timeout, refresh}, State) ->
    Cache = State#state.cache,
    ?INFO(?F("cleaning old RRs from dns cache, objects: ~p, memory size: ~p",
	     [ets:info(Cache, size), ets:info(Cache, memory)])),
    refresh_cache(Cache),
    ?INFO(?F("after cleaning RRs dns cache, objects: ~p, memory size: ~p",
	     [ets:info(Cache, size), ets:info(Cache, memory)])),
    Timer = erlang:send_after(?REFRESH_TIME, self(), {timeout, refresh}),
	      {noreply, State#state{refresh_timer = Timer}};

%handle_info({ets_commands, CommandList}, State) ->
%    Fun = fun({ets_match_delete, Del}) ->
%		  ?INFO(?F("ets:match_delete ~p ",[Del])),
%		  ets:match_delete(?MODULE, Del);
%	     ({ets_insert, Ins})       ->
%		  ?INFO(?F("ets:insert ~p ",[Ins])),
%		  ets:insert(?MODULE, Ins)
%	  end,
%    lists:foreach(Fun, CommandList),
%    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    erlang:cancel_timer(State#state.refresh_timer),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% 
%% Delete all cached RR's with elapsed TTL.
%% 
refresh_cache(Cache) ->
    ?INFO(?F("refresh_cache", [])),
    Now = times(),
    CMPFun = fun(RR) when RR#dns_rr.tm + RR#dns_rr.ttl < Now -> 
%		     ?INFO(?F("true cmpfun rr: ~p",[RR])),
		     true;
		(RR) ->
%		     ?INFO(?F("false cmpfun rr: ~p",[RR])),
		     false
	     end,
    DelFun = fun(RR) ->
%		     ?INFO(?F("cache ttl exp ~p", [RR])),
		     ets:match_delete(Cache, RR) end,
    First = ets:first(Cache),
    Next = case (catch ets:next(Cache, First)) of
	       {'EXIT', _} -> '$end_of_table';
	       E           -> E
	   end,
    fetch_delete_all(First, Next, CMPFun, DelFun, Cache),
    ok.

%% This code is for the ets cache version.
fetch_delete_all('$end_of_table', _NextKeyK, _CMPFun, FilterFun, _Db) -> ok;
fetch_delete_all(Key, NextKey, FilterFun, DelFun, Db) ->
%    ?INFO(?F("key: ~p, NextKey: ~p",[Key, NextKey])),
    NextKey2 = case (catch ets:next(Db, Key)) of
		   {'EXIT', _} -> '$end_of_table';
		   E           -> E
	       end,
    RRs = ets:lookup(Db, Key),
%    ?INFO(?F("RRs to filter: ~p ", [RRs])),
    lists:foreach(DelFun, lists:filter(FilterFun, RRs)), %% remove all old
    fetch_delete_all(NextKey, NextKey2, FilterFun, DelFun, Db).

times() ->
    {Mega,Secs,_} = erlang:now(),
    Mega*1000000 + Secs.

% refresh_cache(Cache) ->
%     Now = times(),
%     Fun = fun(RR) when RR#dns_rr.tm + RR#dns_rr.ttl < Now -> true;
% 	     (_) -> false
% 	  end,
%     DelFun = fun(RR) -> ets:match_delete(Cache, RR) end,
%     lists:foreach(DelFun, lists:filter(Fun, fetch_all(Cache))),
%     ok.

% %% This code is for the ets cache version.
% fetch_all(Db) ->
%     Key = ets:first(Db),
%     fetch_all(Key, Db).

% fetch_all('$end_of_table', _) ->
%     [];
% fetch_all(Key, Db) ->
%     RRs = ets:lookup(Db, Key),
%     NKey = ets:next(Db, Key),
%     RRs ++ fetch_all(NKey, Db).

% times() ->
%     {Mega,Secs,_} = erlang:now(),
%     Mega*1000000 + Secs.

%%
%% Insert a new RR into the cache.
%% SOA and wildcard (*...) domain names shouldn't
%% be cached.
%%
cache_rr(Cache, RR) when RR#dns_rr.type /= ?S_SOA,
			 hd(RR#dns_rr.domain) /= $* ->
    %% delete possible old entry
    ets:match_delete(Cache, RR#dns_rr{cnt = '_',
    				      tm = '_',
    				      ttl = '_',
    				      bm = '_'}),
    
    %%    DelRR = RR#dns_rr{cnt = '_', tm = '_', ttl = '_', bm = '_'},    
     ets:insert(Cache, RR);
%%    self() ! {ets_commands, [{ets_match_delete, DelRR}, {ets_insert, RR}]},
%%true;
cache_rr(_, _) ->
    {error, rejected}.

%%
%% lookup and remove old entries
%%
lookup_rr(Cache, Domain) ->
    match_rr(Cache, #dns_rr{domain = tolower(Domain),
			    class = '_',
			    type = '_', 
			    cnt = '_',
			    tm = '_',
			    ttl = '_',
			    data = '_',
			    bm = '_'}).

lookup_rr(Cache, Domain, Class, Type) ->
    match_rr(Cache, #dns_rr{domain = tolower(Domain),
			    class = Class,
			    type = Type, 
			    cnt = '_',
			    tm = '_',
			    ttl = '_',
			    data = '_',
			    bm = '_'}).

lookup_rr(Cache, Domain, Class, Type, Time) ->
    match_rr(Cache, #dns_rr{domain = tolower(Domain),
			    class = Class,
			    type = Type, 
			    cnt = '_',
			    tm = '_',
			    ttl = '_',
			    data = '_',
			    bm = '_'},
	     Time).

match_rr(Cache, RR) ->
    filter_rr(Cache, ets:match_object(Cache, RR), times()).

match_rr(Cache, RR, Time) ->
    filter_rr(Cache, ets:match_object(Cache, RR), Time).

%%
%% filter old resource records.
%% Set a new TTL according to Time.
%%
filter_rr(Cache, [RR | RRs], Time) when RR#dns_rr.ttl == 0 -> %% at least once
        ets:match_delete(Cache, RR), %%pekka HERE pekka
  %%  self() ! {ets_commands, [{ets_match_delete, RR}]},
    [RR | filter_rr(Cache, RRs, Time)];
filter_rr(Cache, [RR | RRs], Time) when RR#dns_rr.tm + RR#dns_rr.ttl < Time ->
    ets:match_delete(Cache, RR),
%    self() ! {ets_commands, [{ets_match_delete, RR}]},
    filter_rr(Cache, RRs, Time);
filter_rr(Cache, [RR | RRs], Time) when RR#dns_rr.tm > Time ->
    [RR | filter_rr(Cache, RRs, Time)];
filter_rr(Cache, [RR | RRs], Time) ->
    #dns_rr{tm = TM, ttl = TTL} = RR,
    [RR#dns_rr{ttl = (TM + TTL) - Time} | filter_rr(Cache, RRs, Time)];
filter_rr(_, [], _) ->  [].

%%
%% Cache a bunch of RRs.
%%
cache_rrs(_, [], Cache) ->
    ok;
cache_rrs(Auth, RRs, Cache) ->
    Sorted = sort_type(RRs),
    Time = times(),
    cache_rrs(Sorted, Auth, Time, Cache).

%%
%% cache_rrs(TypeL, Auth, Time, Cache) -> true
%%
%% If authoritative data (Auth == true) old cached data
%% is always removed before caching the new.
%% If not authoritative data (Auth == false) nothing is
%% cached if there already exist cached data.
%%
cache_rrs([{Type, RRs}|T], true, Time, Cache) ->
    RR = hd(RRs),
    Domain = RR#dns_rr.domain,
    ets:match_delete(Cache, RR#dns_rr{domain = tolower(Domain),
				       cnt = '_',
				       tm = '_',
				       ttl = '_',
				       bm = '_',
				       data = '_'}),
%    self() ! {ets_commands, [{ets_match_delete,
%			      RR#dns_rr{domain = tolower(Domain),
%					cnt  = '_',
%					tm   = '_',
%					ttl  = '_',
%					bm   = '_',
%					data = '_'}}]},
    F = fun(R) -> R1 = lower_rr(R),
		  cache_rr(Cache, R1#dns_rr{tm = Time})
	end,
    lists:foreach(F, RRs),
    cache_rrs(T, true, Time, Cache);
cache_rrs([{Type, RRs}|T], _, Time, Cache) ->
    RR = hd(RRs),
    Domain = RR#dns_rr.domain,
    case ets:match_object(Cache, RR#dns_rr{domain = tolower(Domain),
					    cnt = '_',
					    tm = '_',
					    ttl = '_',
					    bm = '_',
					    data = '_'}) of
	[] ->
	    F = fun(R) ->
			R1 = lower_rr(R),
			cache_rr(Cache, R1#dns_rr{tm = Time})
		end,
	    lists:foreach(F, RRs),
	    cache_rrs(T, false, Time, Cache);
	_ ->
	    cache_rrs(T, false, Time, Cache)
    end;
cache_rrs([], _, _, _) ->
    true.

sort_type(RRs) ->
    SRRs = lists:keysort(#dns_rr.type, RRs),
    Type = (hd(SRRs))#dns_rr.type,
    cr_type_list(SRRs, Type, []).

cr_type_list([RR|RRs], Type, TypeAck) when RR#dns_rr.type == Type ->
    cr_type_list(RRs, Type, [RR|TypeAck]);
cr_type_list([RR|RRs], Type, TypeAck) ->
    Type1 = RR#dns_rr.type,
    [{Type, TypeAck}|cr_type_list([RR|RRs], Type1, [])];
cr_type_list([], Type, TypeAck) ->
    [{Type, TypeAck}].

%%
%% Lower case the domain name before storage 
%%
lower_rr(RR) ->
    Dn = RR#dns_rr.domain,
    if list(Dn) ->
	    RR#dns_rr{domain = tolower(Dn),
		      bm = Dn};
       true -> RR
    end.

%%
%% Map upper-case to lower-case 
%%
tolower([H|T]) when H >= $A, H =< $Z -> [H+32 | tolower(T)];
tolower([H|T]) -> [H | tolower(T)];
tolower([]) -> [].

