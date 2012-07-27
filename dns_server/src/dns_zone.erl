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
%%% File    : dns_zone.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : A DNS zone administrator.
%%% Created : 10 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_zone).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/0, lookup/3,
	 traverse_init/1, traverse/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {domain_db,
		zone_db,
		lb_ttl,
		pending_del = [],
	        traversing = []}). % [#trav{}]

-record(trav, {pid,
	       db}).

-record(zone, {name,
	       type,   % primary | secondary | stub | ??
	       status = active,
	       lbdomains = [],
	       slave}).

-define(MATCH_ZONE(N,S), #zone{name = N,
			  type = '_',
			  status = S,
			  lbdomains = '_',
			  slave = '_'}).

%% Secondary (or stub) zone information.
-record(slave, {addrs,
		soa,
		timer,
	        backup,
	        xfr}).

%% Current zone transfer information
-record(xfr, {pid,
	      db,
	      lbdomains = []}).

-record(soa, {mname,
	      rname,
	      serial,
	      refresh,
	      retry,
	      expire,
	      ttl}).

-include("dns.hrl").
-include_lib("misc/include/logger.hrl").

%% TBD, should be in dns_rr.hrl
-define(MATCH_ARR(Key),
	#dns_rr{domain = Key,
		class = '_',
		type = ?S_A,
		cnt = '_',
		tm = '_',
		ttl = '_',
		bm = '_',
		data = '_',
	        func = '_'}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(undefined, Zone, Domain) ->
    %% TBD, check if zone is really pending or if other error.
    {error, pending};    
lookup(ZoneDb, Zone, Domain) ->
    D = tolower(Domain),
    case ets:lookup(ZoneDb, {Zone, D}) of
	[]  ->
	    {error, nxdomain};
	RRs ->
	    F = fun(RR) -> RR#dns_rr{domain = D} end,
	    {ok, lists:map(F, RRs)}
    end.

traverse_init(Zone) ->
    {ok, Db} = gen_server:call(?MODULE, {traverse, self(), Zone}),
    Key = ets:first(Db),
    {ok, Db, Key}.

traverse('$end_of_table', _, _) ->
    ok;
traverse(Key, Db, Zone) ->
    RRs = ets:lookup(Db, Key),
    NKey = ets:next(Db, Key),
    {ok, strip_but_zone(RRs, Zone), NKey}.

strip_but_zone([RR|RRs], Zone) when element(1, RR#dns_rr.domain) == Zone ->
    {_, D} = RR#dns_rr.domain,
    [RR#dns_rr{domain = D} | strip_but_zone(RRs, Zone)];
strip_but_zone([_|RRs], Zone) ->
    strip_but_zone(RRs, Zone);
strip_but_zone([], _) ->
    [].

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    DomainDb = ets:new(dns_zone_domain, [{keypos, #dns_rr.domain}, bag]),
    ZoneDb = ets:new(dns_zone, [{keypos, #zone.name}]),
    LB_TTL = get_ttl(),
    fill_zones(DomainDb, ZoneDb, LB_TTL),
    {ok, #state{domain_db = DomainDb,
		zone_db = ZoneDb,
		lb_ttl = LB_TTL}}.

fill_zones(DomainDb, ZoneDb, TTL) ->
    Key = dns_catalog:init_traverse_zones(),
    fill_zones(Key, DomainDb, ZoneDb, TTL).

fill_zones(Key, DomainDb, ZoneDb, TTL) ->
    case dns_catalog:traverse_zones(Key) of
	{NKey, Zone} ->
	    fill_zone(Zone, DomainDb, ZoneDb, TTL),
	    fill_zones(NKey, DomainDb, ZoneDb, TTL);
	false ->
	    ok
    end.

fill_zone({primary, Zone, File, BM, Dir}, DomainDb, ZoneDb, TTL) ->
    DF = filename(Dir,File),
    case store_master(DF, Zone, BM, DomainDb, Dir) of
	{ok, LBDomains} ->
	    get_soa(Zone, DomainDb), % Validity check only
	    store_lb_domains(LBDomains, Zone, DomainDb, TTL),
            dns_catalog:zone_db(Zone, DomainDb),
	    ets:insert(ZoneDb, #zone{name = Zone,
				     lbdomains = strip_fn(LBDomains),
				     type = primary}),
	    ok;
	Error ->
	    ?FATAL(?F("~s: Couldn't store zone ~p - ~p", [DF, Zone, Error])),
	    exit({primary, DF, Error})
    end;
fill_zone({secondary, Zone, {Addrs, Backup}, BM, Dir}, DomainDb,
	  ZoneDb, TTL) ->
    case init_from_backup(Backup, Dir, DomainDb, Zone, BM) of
	{ok, LBDomains} ->
	    SOA = get_soa(Zone, DomainDb),
	    store_lb_domains(LBDomains, Zone, DomainDb, TTL),
	    dns_catalog:zone_db(Zone, DomainDb),
	    TimeRef = erlang:send_after(SOA#soa.refresh * 1000, self(),
					{timeout, Zone, refresh}),
	    Sec = #slave{addrs = Addrs,
			 soa = SOA,
			 timer = TimeRef,
			 backup = {Dir, Backup}},
	    ets:insert(ZoneDb, #zone{name = Zone,
				     type = secondary,
				     lbdomains = strip_fn(LBDomains),
				     slave = Sec}),
	    ok;
	_ ->
	    %% Set refresh timeout = 0 and perform a zone transfer !
	    TimeRef = erlang:send_after(0, self(), {timeout, Zone, refresh}),
	    Sec = #slave{addrs = Addrs,
			 soa = #soa{retry = 10,
                                    expire = 600},
			 timer = TimeRef,
			 backup = {Dir, Backup}},
	    ets:insert(ZoneDb, #zone{name = Zone,
				     type = secondary,
				     status = pending,
				     slave = Sec}),
	    ok
    end.

uppercase(false, Zone) -> Zone;
uppercase(Zone, _)     -> Zone.

init_from_backup(false, _, _, _, _) ->
    ignore;
init_from_backup(Backup, Dir, Db, Zone, BM) ->
    %% TBD, Backup files are not implemented yet !
%    case store_master(filename(Dir,Backup), Zone, BM, Db, Dir) of
%	{ok, LBDomains} ->
%	    {ok, LBDomains};
%	Error ->
%           %% TBD, report error here when implemented.
%	    error
%    end.
    ignore.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

handle_call({traverse, Pid, Zone}, From, State) ->
    #state{zone_db = ZoneDb} = State,
    case ets:lookup(ZoneDb, Zone) of
	[ZZ] when ZZ#zone.status == pending ->
	    {reply, {error, pending}, State};
	[ZZ] ->
	    #state{domain_db = Db, traversing = Tr} = State,
	    link(Pid), % Remove from traversing upon termination.
	    {reply,
	     {ok, Db},
	     State#state{traversing = [#trav{pid = Pid,
					     db = Db} | Tr]}};
	_ ->
	    {reply, {error, undefined}, State}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({timeout, Zone, refresh}, State) ->
    #state{zone_db = ZoneDb} = State,
    [ZZ] = ets:lookup(ZoneDb, Zone),
    #zone{slave = Slave} = ZZ,
    #slave{soa = SOA, addrs = Addrs} = Slave,
    #soa{serial = Serial, retry = Retry, expire = Expire} = SOA,
    {ok, Pid} = dns_xfr:start_link(Zone, Addrs, Serial, Retry, Expire),
    XfrDb = ets:new(dns_zone_domain, [{keypos, #dns_rr.domain}, bag]),
    Slave1 = Slave#slave{timer = undefined,
			 xfr = #xfr{pid = Pid, db = XfrDb}},
    ets:insert(ZoneDb, ZZ#zone{slave = Slave1}),
    {noreply, State};

handle_info({Pid, Zone, uptodate}, State) ->
    %% No zone transfer needed
    #state{zone_db = ZoneDb} = State,
    [ZZ] = ets:lookup(ZoneDb, Zone),
    #zone{slave = Slave} = ZZ,
    #slave{soa = SOA, xfr = Xfr} = Slave,
    ets:delete(Xfr#xfr.db),
    Timer = erlang:send_after(SOA#soa.refresh * 1000, self(),
			      {timeout, Zone, refresh}),
    ets:insert(ZoneDb, ZZ#zone{slave = Slave#slave{timer = Timer,
						   xfr = undefined}}),
    {noreply, State};

handle_info({Pid, Zone, {insert, RR}}, State) ->
    #state{zone_db = ZoneDb} = State,
    [ZZ] = ets:lookup(ZoneDb, Zone),
    #zone{slave = Slave} = ZZ,
    #slave{xfr = Xfr} = Slave,
    #xfr{db = XfrDb, lbdomains = LBD} = Xfr,
    LBDomains = insert_xfr_rr(XfrDb, RR, Zone, LBD),
    Xfr1 = Xfr#xfr{lbdomains = LBDomains},
    ets:insert(ZoneDb, 
	       ZZ#zone{slave = Slave#slave{xfr = Xfr1}}),
    {noreply, State};

handle_info({Pid, Zone, expired}, State) ->
    %% TBD, this zone should be deleted from our DNS server !
    ?INFO(?F("Zone (~p) transfer expired", [Zone])),
    #state{zone_db = ZoneDb} = State,
    [ZZ] = ets:lookup(ZoneDb, Zone),
    #zone{slave = Slave} = ZZ,
    #slave{xfr = Xfr} = Slave,
    ets:delete(Xfr#xfr.db),
    ets:insert(ZoneDb, ZZ#zone{status = pending,
			       slave = Slave#slave{xfr = undefined}}),
    {noreply, State};

handle_info({Pid, Zone, done}, State) ->
    #state{zone_db = ZoneDb,
	   domain_db = OldDb,
	   lb_ttl = TTL,
	   pending_del = Pend,
	   traversing = Tr} = State,
    [ZZ] = ets:lookup(ZoneDb, Zone),
    #zone{slave = Slave, lbdomains = OldLBD} = ZZ,
    #slave{xfr = Xfr} = Slave,
    #xfr{db = NewDb, lbdomains = LBD} = Xfr,
    copy_old_except_zone(Zone, OldDb, NewDb),
    store_lb_domains(LBD, Zone, NewDb, TTL),
    SOA = get_soa(Zone, NewDb),
    Timer = erlang:send_after(SOA#soa.refresh * 1000, self(),
			      {timeout, Zone, refresh}),
    %% TBD, store zone on backup file !!
    ets:insert(ZoneDb, ZZ#zone{status = active,
			       lbdomains = strip_fn(LBD),
			       slave = Slave#slave{xfr = undefined,
						   soa = SOA,
						   timer = Timer}}),
    set_new_db(ZoneDb, NewDb),
    Pend1 = try_delete(OldDb, Zone, OldLBD, Pend, Tr),
    {noreply, State#state{domain_db = NewDb,
			  pending_del = Pend1}};

handle_info({Pid, Zone, retry}, State) ->
    %% Undo all previous DB insertions.
    #state{zone_db = ZoneDb} = State,
    [ZZ] = ets:lookup(ZoneDb, Zone),
    #zone{slave = Slave} = ZZ,
    #slave{xfr = Xfr} = Slave,
    ets:delete(Xfr#xfr.db),
    Db = ets:new(dns_zone, [{keypos, #dns_rr.domain}, bag]),
    Xfr1 = Xfr#xfr{db = Db, lbdomains = []},
    ets:insert(ZoneDb, ZZ#zone{slave = Slave#slave{xfr = Xfr1}}),
    {noreply, State};

handle_info({'EXIT', Pid, _}, State) ->
    #state{zone_db = ZoneDb,
	   pending_del = Pend,
	   traversing = Tr} = State,
    case exit_traversing(Pid, Pend, Tr, State) of
	{ok, State1} ->
	    {noreply, State1};
	_ ->
	    case find_slave(Pid, ZoneDb) of
		false ->
		    {noreply, State};
		{ZZ, Slave, Zone} ->
		    %% This case should (could) never occur;
		    %% handled in expired.
		    ?INFO(?F("Zone (~p) transfer failed - "
			     "retry after refresh timeout", [Zone])),
		    #slave{xfr = Xfr, soa = SOA} = Slave,
		    ets:delete(Xfr#xfr.db),
		    TimeRef = erlang:send_after(SOA#soa.refresh * 1000, self(),
						{timeout, Zone, refresh}),
		    ets:insert(ZoneDb,
			       ZZ#zone{slave = Slave#slave{timer = TimeRef,
							   xfr = undefined}}),
		    {noreply, State}
	    end
    end;
    
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    cancel_timers(State#state.zone_db),
    ok;
terminate(_, _) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% The load balanced domain names TTL is configurable.
%%
get_ttl() ->
    {ok, TTL} = dns_catalog:lb_ttl(),
    TTL.

%%
%%
%%
filename(Dir, File) ->
    case filename:pathtype(File) of
	absolute -> File;
	_        -> filename:join(Dir, File)
    end.

%%
%% store_master(File, Zone, BitMap, Db, Dir [, Default]) ->
%%
store_master(File, Zone, BM, Db, Dir) ->
    ZZone = uppercase(BM, Zone),
    store_master(File, Zone, BM, Db, Dir, dns_rr:cr_default(ZZone)).

store_master(File, Zone, _, Db, Dir, Default) ->
    case dns_parse:master(File) of
	{ok, Pid} ->
	    ?INFO(?F("master zone ~p loaded",[Zone])),
	    fill_db(Pid, Zone, Db, Dir, Default, []);
	Error ->
	    Error
    end.

%%
%%
%%
fill_db(Pid, Zone, Db, Dir, Default, LBD) ->
    case dns_parse:parse_next(Pid) of
	{ok, eof} ->
	    {ok, LBD};
	{ok, Item} ->
	    {NewDefault, LBD1} = insert(Item, Zone, Db, Dir, Default, LBD),
        ?INFO(?F("Loading Zone File: ~p", [Zone])),
	    fill_db(Pid, Zone, Db, Dir, NewDefault, LBD1);
	Error ->
	    Error
    end.

insert({rr, RR}, Zone, Db, _, Default, LBD) ->
    {ok, RRrec, NDef} = dns_rr:decode(RR, Default, Zone),
    LBD1 = insert_rr(Db, RRrec, Zone, LBD),
    {NDef, LBD1};
insert({include, File}, Zone, Db, Dir, Default, LBD) ->
    DF = filename(Dir,File),
    case store_master(DF, Zone, false, Db, Dir, Default) of
	{ok, LBDomains} ->
	    {Default, LBDomains ++ LBD};
	Error ->
	    ?FATAL(?F("~s: Include file error ~p - ~p", [DF, Zone, Error])),
	    exit({include, DF, Error})
    end;
insert({include, File, Origin}, Zone, Db, Dir, Default, LBD) ->
    DF = filename(Dir,File),
    case store_master(DF, Zone, false, Db, Dir,
		      dns_rr:origin(Default, Origin)) of
	{ok, LBDomains} ->
	    {Default, LBDomains ++ LBD};
	Error ->
	    ?FATAL(?F("~s: Include file error ~p - ~p", [DF, Zone, Error])),
	    exit({include, DF, Error})
    end;
insert({origin, Origin}, Zone, Db, Dir, Default, LBD) ->
    {dns_rr:origin(Default, Origin), LBD};
insert(Other, _, _, _, Default, LBD) ->
    io:format("dns_zone should insert ~p~n",[Other]),
    {Default, LBD}.

insert_rr(Db, RR, Zone, LBD) ->
    #dns_rr{domain = Domain, type = Type} = RR,
    ets:insert(Db, RR#dns_rr{domain = {Zone, Domain}}),
    LBD1 = lbd(Type, Domain, LBD).

%%
%% If Domain is a load balanced domain store it in LBD
%% for treatment later on.
%%
lbd(?S_A, Domain, LBD) ->
    case dns_catalog:load_function(Domain) of
	{ok, Func} ->
	    case lists:keymember(Domain, 1, LBD) of
		true -> LBD;
		_    -> [{Domain, Func}|LBD]
	    end;
	_ ->
	    LBD
    end;
lbd(_, _, LBD) ->
    LBD.

%%
%% Get the SOA of our zone.
%%
get_soa(Zone, Db) ->
    F = fun(#dns_rr{type = Type}) when Type /= ?S_SOA -> false;
	   (RR)                                       -> true
	end,
    case lists:filter(F, ets:lookup(Db, {Zone, Zone})) of
	[SOA] ->
	    {O,M,S,Re,Ret,E,T} = SOA#dns_rr.data,
	    #soa{mname = O,
		 rname = M,
		 serial = S,
		 refresh = Re,
		 retry = Ret,
		 expire = E,
		 ttl = T};
	_ ->
	    ?FATAL(?F("No SOA record in zone ~p", [Zone])),
	    exit({no_zone_soa, Zone})
    end.

%%
%% Insert a transferred RR into our DB.
%% Save original letters before lower case all.
%%
insert_xfr_rr(Db, RR0, Zone, LBD) ->
    RR = fix_name(RR0, Zone),
    insert_rr(Db, RR, Zone, LBD).

fix_name(RR, Zone) ->
    Domain = RR#dns_rr.domain,
    BM = cr_bitmap(Domain, Zone),
    RR#dns_rr{domain = tolower(Domain),
	      bm = BM}.

cr_bitmap(Domain, Zone) ->
    %% Zone suffix of Domain ?
    case string:rstr(Domain, Zone) of
	N when N < 2 ->
	    %% Zone == Domain or Zone not a suffix
	    Domain ++ ".";
	N ->
	    string:substr(Domain, 1, N - 2)
    end.

%%
%% Map upper-case to lower-case 
%%
tolower(L) ->
    case tolower_p(L) of
	true -> do_tolower(L);
	_    -> L
    end.

tolower_p([H|_]) when H >= $A, H =< $Z -> true;
tolower_p([_|T])                       -> tolower_p(T);
tolower_p([])                          -> false.

do_tolower([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | do_tolower(Cs)];
do_tolower([C|Cs]) -> [C | do_tolower(Cs)];
do_tolower([]) -> [].

%%
%% Modify the load_balance function per domain name
%% in order to find the corresponding cluster.
%%

store_lb_domains([{Domain, {M, Fn}}|LBDs], Zone, DomainDb, TTL) ->
    case ets:match_object(DomainDb, ?MATCH_ARR({Zone, Domain})) of
	[] ->
	    store_lb_domains(LBDs, Zone, DomainDb, TTL);
	RRs ->
	    IPs = lists:map(fun(RR) -> RR#dns_rr.data end, RRs),
	    Cid = dns_load:add_ip_cluster(IPs),
	    F = fun(RR) ->
			?INFO(?F("load balancing domain ~p",[RR#dns_rr.domain])),
			ets:match_delete(DomainDb, RR),
			ets:insert(DomainDb,
				   RR#dns_rr{func = {M, Fn, [Cid, TTL]}})
		end,
	    lists:foreach(F, RRs),
	    store_lb_domains(LBDs, Zone, DomainDb, TTL)
    end;
store_lb_domains([], _, _, _) ->
    ok.

%%
%% Keep only the Domain names.
%%
strip_fn(LBDomains) ->
    F = fun({Domain, _}) -> Domain end,
    lists:map(F, LBDomains).

%%
%% Cancel all active slave timers.
%%
cancel_timers(Db) ->
    Key = ets:first(Db),
    cancel_timers(Key, Db).

cancel_timers('$end_of_table', _) ->
    ok;
cancel_timers(Key, Db) ->
    NKey = ets:next(Db, Key),
    case ets:lookup(Db, Key) of
	[#zone{type = secondary,
	       slave = #slave{timer = Timer}}] ->
	    cancel_timer(Timer),
	    cancel_timers(NKey, Db);
	_ ->
	    cancel_timers(NKey, Db)
    end.

%%
%% Cancel a timer (if active).
%%
cancel_timer(undefined) -> ok;
cancel_timer(Ref)       -> erlang:cancel_timer(Ref).


%%
%% Copy the old DB except the currently updated zone.
%%
copy_old_except_zone(Zone, OldDb, NewDb) ->
    Key = ets:first(OldDb),
    copy_old_except_zone(Key, Zone, OldDb, NewDb).
    
copy_old_except_zone('$end_of_table', _, _, _) ->
    ok;
copy_old_except_zone(Key, Zone, OldDb, NewDb) ->
    RRs = ets:lookup(OldDb, Key),
    NKey = ets:next(OldDb, Key),
    copy_but_zone(RRs, Zone, NewDb),
    copy_old_except_zone(NKey, Zone, OldDb, NewDb).

copy_but_zone([RR|RRs], Zone, NewDb) when element(1, RR#dns_rr.domain) == Zone ->
    copy_but_zone(RRs, Zone, NewDb);
copy_but_zone([RR|RRs], Zone, NewDb) ->
    ets:insert(NewDb, RR),
    copy_but_zone(RRs, Zone, NewDb);
copy_but_zone([], _, _) ->
    ok.

%%
%% Set the new DB as current for all active zones.
%%
set_new_db(ZoneDb, NewDb) ->
    F = fun([Zone]) -> dns_catalog:zone_db(Zone, NewDb) end,
    lists:foreach(F, ets:match(ZoneDb, ?MATCH_ZONE('$1',active))).

%%
%% Delete the old ets if no process currently is traversing
%% the DB (for a zone transfer).
%% (normal queries accessing an old db should crash and
%% let the querying party resend the question.)
%% If the ets table couldn't be removed put it in the pending_del
%% list and delete it when the last traversing process terminates.
%%
try_delete(Db, Zone, LBD, Pend, Tr) ->
    case delete_p(Db, Tr) of
	false ->
	    [{Db, Zone, LBD}|Pend];
	_ ->
	    delete_db(Db, Zone, LBD),
	    Pend
    end.

delete_p(Db, Tr) ->
    F = fun(T) when T#trav.db == Db -> true;
	   (_)                      -> false
	end,
    not lists:any(F, Tr).

%%
%% Delete an old db.
%% First delete all cluster IPs (related to LB domains).
%%
delete_db(Db, Zone, LBD) ->
    F = fun(Domain) ->
		case ets:match_object(Db, ?MATCH_ARR({Zone, Domain})) of
		    [] ->
			ok;
		    [#dns_rr{func = {_, _, [Cid|_]}}|_] ->
			%% All RRs have the same Cid.
			dns_load:delete_ip_cluster(Cid)
		end
	end,
    lists:foreach(F, LBD),
    ets:delete(Db).

%%
%% Delete a pending DB if Pid was the last user of that table.
%%
exit_traversing(Pid, Pend, Tr, State) ->
    case del_pid(Tr, Pid) of
	false ->
	    false;
	{true, Dbs, Tr1} ->
	    Pend1 = del_pend(Dbs, Pend, Tr1),
	    {ok, State#state{pending_del = Pend1,
			     traversing = Tr1}}
    end.

del_pid(Tr, Pid) -> del_pid(Tr, Pid, [], [], false).

del_pid([#trav{pid = Pid, db = Db}|Tr], Pid, Dbs, TrAck, _) ->
    del_pid(Tr, Pid, [Db|Dbs], TrAck, true);
del_pid([T|Tr], Pid, Dbs, TrAck, Flag) ->
    del_pid(Tr, Pid, Dbs, [T|TrAck], Flag);
del_pid([], _, Dbs, TrAck, true) ->
    {true, Dbs, TrAck};
del_pid([], _, _, _, _) ->
    false.

del_pend([Db|Dbs], Pend, Tr) ->
    case lists:keymember(Db, 1, Pend) of
	true ->
	    case delete_p(Db, Tr) of
		true ->
		    case lists:keysearch(Db, 1, Pend) of
			{value, {_, Zone, LBD}} ->
			    delete_db(Db, Zone, LBD),
			    del_pend(Dbs, lists:keydelete(Db, 1, Pend), Tr);
			_ ->
			    del_pend(Dbs, Pend, Tr)
		    end;
		_ ->
		    del_pend(Dbs, Pend, Tr)
	    end;
	_ ->
	    del_pend(Dbs, Pend, Tr)
    end;
del_pend([], Pend, _) ->
    Pend.
    
%% {ZZ, Slave, Zone} = find_slave(Pid, ZoneDb)
find_slave(Pid, ZoneDb) ->
    P = #zone{name = '_',
	      type = '_',
	      status = '_',
	      lbdomains = '_',
	      slave = #slave{addrs = '_',
			     soa = '_',
			     timer = '_',
			     backup = '_',
			     xfr = #xfr{pid = Pid,
					db = '_',
				        lbdomains = '_'}}},
    case ets:match_object(ZoneDb, P) of
	[] ->
	    false;
	[ZZ] ->
	    {ZZ, ZZ#zone.slave, ZZ#zone.name}
    end.

