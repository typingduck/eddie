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
%%% File    : dns_query.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : A DNS query evaluator.
%%% Created :  6 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Modified: 21 Feb 2000 by Pekka Hedqvist <pekka@eddieware.org>
%%% Done: -Added timeout on recursive queries, tolower updated.
%%% 
%%%----------------------------------------------------------------------

-module(dns_query).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-behaviour(gen_fsm).

%% External exports
-export([start_link/5, resent/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_info/3, terminate/3]).

%% gen_fsm state functions
-export([decode/2, eval_query/2, get_zone/2, match_cache/2,
	 match_zone/2, additional_info/2, respond/2,
	 eval_recursive_query/2, wait_recursive/2]).

%% exported for dns_recurse.erl
-export([find_zone/1, do_match_zone/2, matching_rrs/2,
	 get_additional_addrs/3]).

-record(state, {ll_query,     %% Low level query, list of bytes.
		address,      %% The address of the peer.
		dns_query,    %% Decoded question.
		dns_resp,     %% Built answer.
		zone_answ,    %% Found an answer in a zone, true | false.
		current_q,    %% Current question.
		orig_q   ,    %% Original question, if CNAME encountered.
	        cnt = 0,      %% Number of recursions through SLIST.
		zone,         %% Zone nearest QNAME. Zone = #zone{}
		recurse_pid,  %% Pid of recursive evaluator.
		options,      %% options directive in boot file.
	        dns_server}). %% PID

-record(zone, {domain, db}).

-include("dns.hrl").


%% terminate query after this time if no reply from 
%% a recursive query has appeared. 
%%
-define(R_TIMELIMIT, 35 * 1000).  %% time to wait on recursive query
-define(EPSILON,             e).  %% Internal state transitions.
-define(RECURSION_DESIRED,   1).  %% Recursion desired by the client.
-define(RESPONSE,            1).  %% The QR header field.

%% The aa entry in dns_rec.header record.
-define(AUTHORITY,           1).
-define(NO_AUTHORITY,        0).

-define(tolower(C),
	if (C) >= $A, (C) =< $Z -> ((C)-$A)+$a;
	   true -> C
	end).

-define(SCNT, 6). %% Number of recursive questions allowed.
                  %% really not true /pekka

-include_lib("../../misc/include/logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Query, IP, Port, Proto, Opts) ->
    gen_fsm:start_link(dns_query, [Query, IP, Port, Proto, Opts, self()],
		       []).

resent(Pid, Query) ->
    gen_fsm:send_all_state_event(Pid, {resent, Query}),
    ok.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%% Decode the Query after the 0 ms timeout, in the decode state.
init([[H0,H1,H2,H3|BinQuery], IP, Port, Proto, Opts, DNS_server]) ->
    process_flag(trap_exit, true),
    Query = [H0,H1,H2,H3|binary_to_list(BinQuery)],
    {ok, decode, #state{ll_query = Query,
			address = {IP, Port, Proto},
			options = Opts,
			dns_server = DNS_server},
     0}.

%%----------------------------------------------------------------------
%% Func: decode/2
%%       The decode state.
%%----------------------------------------------------------------------

decode(timeout, State) ->
    #state{ll_query = LLQ, options = Opts} = State,
    case inet_dns:decode(LLQ) of
	{ok, Q} ->
	    Resp0 = Q,
	    Header0 = Q#dns_rec.header,
	    Header = Header0#dns_header{ra = ?REC_AVAIL(Opts),
				        qr = ?RESPONSE,
				        aa = ?AUTHORITY},
	    %% Reset the aa bit thenever answer is not our authority.
	    R = Resp0#dns_rec{header = Header},
	    eval_query(?EPSILON, State#state{dns_query = Q,
					     dns_resp = R,
					     ll_query = []});
	Error ->
	    ?INFO(?F("Invalid query from ~p - ~p",
		     [State#state.address, Error])),
	    {stop, shutdown, State}
    end.

%%----------------------------------------------------------------------
%% Func: eval_query/2
%%       The eval_query state.
%%       (RFC1034, 4.3.2 - step 1)
%%       ra = ?RECURSION_AVAIL in decode !
%%----------------------------------------------------------------------

eval_query(?EPSILON, State) ->
    Query = State#state.dns_query,
    Header = Query#dns_rec.header,
    case Query#dns_rec.qdlist of
	[Q] when Header#dns_header.opcode == ?QUERY,
		 Q#dns_query.class == in ->
	    S = State#state{current_q = Q,
			    orig_q = Q,
			    zone_answ = false},
	    get_zone(?EPSILON, S);
	[Q] when Header#dns_header.opcode == ?QUERY ->
	    %% We do only handle CLASS = IN.
	    Resp0 = State#state.dns_resp,
	    H0 = Resp0#dns_rec.header,
	    H = H0#dns_header{rcode = ?SERVFAIL},
	    Resp = Resp0#dns_rec{header = H},
	    respond(?EPSILON, State#state{dns_resp = Resp});
	_ ->
	    %% We do not handle multiple queries as it is impossible
	    %% to set AUTHORITY and/or rcode for several answers.
	    %% But, if we should handle this case [Q|Qs] - set Q as 
	    %% current_q and qdlist = Qs and recurse over eval_query/2.
	    Rcode = if
			Header#dns_header.opcode == ?QUERY -> ?FORMERR;
			true                               -> ?NOTIMP
		    end,
	    Resp0 = State#state.dns_resp,
	    H0 = Resp0#dns_rec.header,
	    H = H0#dns_header{rcode = Rcode},
	    Resp = Resp0#dns_rec{header = H},
	    respond(?EPSILON, State#state{dns_resp = Resp})
    end.

%%----------------------------------------------------------------------
%% Func: eval_recursive_query/2
%%       The eval_recursive_query state.
%%       (RFC1034, 4.3.2 - step 5)
%%       Note: we have already done the local part here !
%%       If we have found local NS info use that info as initial
%%       hints (SLIST).
%%----------------------------------------------------------------------

eval_recursive_query(?EPSILON, State) ->
    #state{dns_resp = Resp,
	   current_q = Query,
	   options = Options,
	   address = ADDRESS} = State,
    case start_recurse(Resp, Query, Options, element(1,ADDRESS)) of
	{ok, Pid} ->
	    tell_recursive_q(State),
	    {next_state, wait_recursive, State#state{recurse_pid = Pid}, ?R_TIMELIMIT};
	_ ->
	    respond(?EPSILON, State)
    end.

start_recurse(Resp, Query, Opts, PeerIP) when Resp#dns_rec.nslist == [] ->
    #dns_rec{nslist = NsL, arlist = ArL} = Resp,
    Domain = tolower(Query#dns_query.domain),
    dns_recurse:start_link(?SCNT, Query, Opts, PeerIP,[#pend{domain = Domain}]);

start_recurse(Resp, Query, Opts, PeerIP) ->
    #dns_rec{nslist = NsL, arlist = ArL} = Resp,
    Domain = tolower(Query#dns_query.domain),
    dns_recurse:start_link(?SCNT, Query, Opts, {NsL, ArL}, PeerIP,
			   [#pend{domain = Domain,
				  hints = {NsL, ArL}}]).

%% Tell the DNS server that this query was an
%% recursive query, i.e. the response will be delivered
%% later.
tell_recursive_q(State) ->
    #state{dns_server = DNS_server} = State,
    DNS_server ! {self(), recursive}.

%%----------------------------------------------------------------------
%% Func: wait_recursive/2
%%       Wait for the recursive answer.
%%----------------------------------------------------------------------

wait_recursive({Pid, Answ}, State) when pid(Pid) ->
    Resp0 = State#state.dns_resp,
    H0 = Resp0#dns_rec.header,
    AnL = Resp0#dns_rec.anlist,
    AuL = Resp0#dns_rec.nslist,
    ArL = Resp0#dns_rec.arlist,
    AHeader = Answ#dns_rec.header,
    H = H0#dns_header{rcode = AHeader#dns_header.rcode,
		      aa = which_aa(AHeader#dns_header.aa, H0, State),
		      tc = AHeader#dns_header.tc},
    Resp = Resp0#dns_rec{header = H,
			 anlist = merge(AnL, Answ#dns_rec.anlist),
			 nslist = merge(AuL, Answ#dns_rec.nslist),
			 arlist = merge(ArL, Answ#dns_rec.arlist)},
    respond(?EPSILON, State#state{dns_resp = Resp});

wait_recursive(R, State) ->
    %% Recursive Pid decided to terminate !
    %% or no answer in set timeout timeframe
    respond(?EPSILON, State).

%%----------------------------------------------------------------------
%% Func: additional_info/2
%%       The additional_info state.
%%       (RFC1034, 4.3.2 - step 6)
%%----------------------------------------------------------------------

additional_info(?EPSILON, State) ->
    #state{zone = Zone,
	   address = PeerAddr,
	   dns_resp = Resp,
	   current_q = Query} = State,
    PeerIP = element(1, PeerAddr),
    #dns_rec{anlist = ANl, nslist = NSl, arlist = ARl0} = Resp,
    ARl = get_additional_a(Query#dns_query.type, ARl0, ANl, Zone, element(1,PeerAddr)),
    case State#state.zone_answ of
	true ->	 
   {NSs, Addrs} = get_zone_ns_info(Zone,PeerIP),
	    R = Resp#dns_rec{nslist = merge(NSl, NSs),
			     arlist = merge(ARl, Addrs)},
	    respond(?EPSILON, State#state{dns_resp = R});
	false when Zone /= undefined ->
	    SOA = get_zone_soa(Zone),
	    R = Resp#dns_rec{nslist = merge(NSl, SOA)},
	    respond(?EPSILON, State#state{dns_resp = R});
	false ->
	    QNAME = Query#dns_query.domain,
	    {NSs, Addrs} = get_cache_ns_info(QNAME,element(1,PeerAddr)),
	    R = Resp#dns_rec{nslist = merge(NSl, NSs),
			     arlist = merge(ARl, Addrs)},
	    respond(?EPSILON, State#state{dns_resp = R})
    end.

%%
%% Get additional A RRs for requested MX and SRV types.
%%
get_additional_a(Type, ARl, _, _, _PeerIP) 
  when Type /= ?S_MX, Type /= ?S_SRV ->
    ARl;
get_additional_a(Type, ARl, ANl, Zone, PeerIP) ->
    case get_additional_a(Type, Zone, ANl,PeerIP) of
	[]    -> ARl;
	Addrs -> merge(ARl, Addrs)
    end.
    
get_additional_a(?S_MX, Zone,
		 [#dns_rr{type = ?S_MX, data = {_, Domain}}|ANl],PeerIP) ->
    case get_a_addrs(Domain, Zone, PeerIP) of
	[] -> get_additional_a(?S_MX, Zone, ANl, PeerIP);
	As -> As ++ get_additional_a(?S_MX, Zone, ANl, PeerIP)
    end;
get_additional_a(?S_SRV, Zone,
		 [#dns_rr{type = ?S_SRV, data = {_, _, _, Domain}}|ANl],PeerIP) ->
    case get_a_addrs(Domain, Zone,PeerIP) of
	[] -> get_additional_a(?S_SRV, Zone, ANl,PeerIP);
	As -> As ++ get_additional_a(?S_SRV, Zone, ANl,PeerIP)
    end;
get_additional_a(Type, Zone, [_|ANl],PeerIP) ->
    get_additional_a(Type, Zone, ANl,PeerIP);
get_additional_a(_, _, [],_PeerIP) ->
    [].

%%
%% Get additional A RRs for NS records for Zone.
%%
get_zone_ns_info(Zone, PeerIP) ->
    Zdb = Zone#zone.db,
    Domain = Zone#zone.domain,
    case dns_zone:lookup(Zdb, Domain, Domain) of
	{ok, RRs} ->
	    NSRRs = matching_rrs(RRs, ?S_NS),
	    Addrs = get_additional_addrs(NSRRs, Zone,PeerIP),
	    {NSRRs, Addrs};
	_ ->
	    {[],[]}
    end.

%%
%% Get the Zone SOA RR.
%%
get_zone_soa(Zone) ->
    Zdb = Zone#zone.db,
    Domain = Zone#zone.domain,
    case dns_zone:lookup(Zdb, Domain, Domain) of
	{ok, RRs} ->
	    matching_rrs(RRs, ?S_SOA);
	_ ->
	    []
    end.

%%
%% Get additional A RRs for NS records from the cache.
%%
get_cache_ns_info(QNAME, PeerAddr) ->
    case do_match_cached_zone(QNAME) of
	{ok, NSs} ->
	    F=fun(#dns_rr{data = D})-> additional_lookup(cache,D,PeerAddr) end,
	    Addrs = lists:append(lists:map(F, NSs)),
	    {NSs, Addrs};
	_ ->
	    {[], []}
    end.

do_match_cached_zone("") ->
    false;
do_match_cached_zone(Domain) ->
    case dns_cache:lookup(Domain, in, ?S_NS) of
	{ok, NSs} ->
	    {ok, NSs};
	_ ->
	    SubD = sub_label(Domain),
	    do_match_cached_zone(SubD)
    end.
	    
%%----------------------------------------------------------------------
%% Func: respond/2
%%       The respond state.
%%       (RFC1034, 4.3.2 - finished)
%%----------------------------------------------------------------------

respond(?EPSILON, State) ->
    Resp = State#state.dns_resp,
    case inet_dns:encode(Resp) of
	{ok, Buffer} ->
	    Header = Resp#dns_rec.header,
	    #state{dns_server = DNS_server} = State,
	    reply(DNS_server, Buffer, Header#dns_header.id),
	    unlink(DNS_server),
	    {stop, normal, State};
	Error ->
	    ?INFO(?F("Can't encode response to ~p - ~p",
		     [State#state.address, Error])),
	    {stop, Error, State}
    end.

%%----------------------------------------------------------------------
%% Func: get_zone/2
%%       The get_zone state.
%%       (RFC1034, 4.3.2 - step 2)
%%----------------------------------------------------------------------

get_zone(?EPSILON, State) ->
    #dns_query{type = Type,
	       domain = QNAME} = State#state.current_q,
    case find_zone(QNAME) of
	{ok, Zone} when Type /= ?S_AXFR ->
	    match_zone(?EPSILON, State#state{zone = Zone,
					     zone_answ = true});
	{ok, Zone} when Type == ?S_AXFR,
			Zone#zone.domain == QNAME ->
	    zone_transfer(?EPSILON, State#state{zone = Zone});
	_ when Type == ?S_AXFR ->
	    {stop, normal, State};
	_ ->
	    match_cache(?EPSILON, State)
    end.

%%----------------------------------------------------------------------
%% Func: zone_transfer/2
%%       The zone_transfer state.
%%       (RFC1034, 4.3.5)
%%----------------------------------------------------------------------

zone_transfer(?EPSILON, State) ->
    #state{zone = Zone,
	   address = Address,
	   dns_resp = Resp,
	   dns_server = DNS_server} = State,
    case xfr_allowed_p(Address) of
	true ->
	    SOA = get_zone_soa(Zone),
	    SOA_R = Resp#dns_rec{anlist = SOA},
	    ID = (Resp#dns_rec.header)#dns_header.id,
	    send_zone_info(SOA_R, ID, DNS_server),
	    send_zone_rrs(Zone, ID, DNS_server, Resp),
	    send_zone_info(SOA_R, ID, DNS_server),
	    {stop, normal, State};
	_ ->
	    ?INFO(?F("Not allowed zone transfer request: ~p", [Address])),
	    {stop, normal, State}
    end.

xfr_allowed_p({IP, _, _}) ->
    case dns_catalog:xfrnets() of
	[] ->
	    true;
	XfrNets ->
	    xfr_allowed_p(IP, XfrNets)
    end.

xfr_allowed_p(IP, [{NetIP, NetM}|Xfrs]) ->
    case do_band(NetM, IP) of
	NetIP -> true;
	_     -> xfr_allowed_p(IP, Xfrs)
    end;
xfr_allowed_p(_, _) ->
    false.

do_band({M1,M2,M3,M4}, {IP1,IP2,IP3,IP4}) ->
    %% IPv4
    {M1 band IP1,
     M2 band IP2,
     M3 band IP3,
     M4 band IP4};
do_band({M1,M2,M3,M4,M5,M6,M7,M8},
	{IP1,IP2,IP3,IP4,IP5,IP6,IP7,IP8}) ->
    %% IPv6
    {M1 band IP1,
     M2 band IP2,
     M3 band IP3,
     M4 band IP4,
     M5 band IP5,
     M6 band IP6,
     M7 band IP7,
     M8 band IP8}.

send_zone_rrs(Zone, ID, DNS_server, Resp) ->
    Domain = Zone#zone.domain,
    {ok, Db, Key} = dns_zone:traverse_init(Domain),
    send_zone_rrs(Key, Db, ID, DNS_server, Resp, Domain).

send_zone_rrs(Key, Db, ID, DNS_server, Resp, Domain) ->
    case dns_zone:traverse(Key, Db, Domain) of
	{ok, RRs0, NKey} ->
	    case original_name_strip_soa(RRs0, Domain) of
		[] ->
		    send_zone_rrs(NKey, Db, ID, DNS_server, Resp, Domain);
		RRs ->
		    %% Only allowed to send one RR at a time according to
		    %% the BIND implementation.
		    F = fun(RR) -> 
				RR_Resp = Resp#dns_rec{anlist = [RR]},
				send_zone_info(RR_Resp, ID, DNS_server)
			end,
		    lists:foreach(F, RRs),
		    send_zone_rrs(NKey, Db, ID, DNS_server, Resp, Domain)
	    end;
	ok ->
	    ok
    end.

original_name_strip_soa([RR|RRs], Domain) when RR#dns_rr.domain == Domain,
					       RR#dns_rr.type == ?S_SOA ->
    original_name_strip_soa(RRs, Domain);
original_name_strip_soa([RR|RRs], Domain) ->
    [RR#dns_rr{domain = orig_name(RR#dns_rr.bm, Domain)} |
     original_name_strip_soa(RRs, Domain)];
original_name_strip_soa([], _) ->
    [].

orig_name([$.], _)       -> [];
orig_name([H|T], Domain) -> [H|orig_name(T, Domain)];
orig_name([], Domain)    -> [$.|Domain].
	
send_zone_info(Resp, ID, DNS_server) ->
    case inet_dns:encode(Resp) of
	{ok, Buffer} ->
	    reply(DNS_server, Buffer, ID),
	    ok;
	Error ->
	    ?INFO(?F("Can't encode zone info - ~p",
		     [Error])),
	    Error
    end.
	
%%----------------------------------------------------------------------
%% Func: match_zone/2
%%       The match_zone state.
%%       (RFC1034, 4.3.2 - step 3)
%%----------------------------------------------------------------------

match_zone(?EPSILON, State) ->
    Query = State#state.current_q,
    QNAME = Query#dns_query.domain,
    Resp = State#state.dns_resp,
    Zone = State#state.zone,
    PeerIP = element(1,State#state.address),
    case do_match_zone(Query, Zone) of
	{match, [RR]} when RR#dns_rr.type == ?S_CNAME ->
	    Answ = Resp#dns_rec.anlist,
	    MRRs = patch_owner([RR], QNAME),
	    R = Resp#dns_rec{anlist = merge(Answ, MRRs)},
	    if
		Query#dns_query.type == ?S_CNAME ->
		    additional_info(?EPSILON, State#state{dns_resp = R});
		true ->
		    Q = Query#dns_query{domain = RR#dns_rr.data},
		    S = State#state{current_q = Q,
				    dns_resp = R},
		    get_zone(?EPSILON, S)
	    end;
	{match, RRs} ->
	    Type = Query#dns_query.type,
	    case matching_rrs(RRs, Type) of
		[] ->
		    additional_info(?EPSILON, State#state{zone_answ = false});
		MRRs0 ->
		    MRRs1 = load_balance(MRRs0, Type, PeerIP),
		    MRRs = patch_owner(MRRs1, QNAME),
		    Answ = Resp#dns_rec.anlist,
		    R = Resp#dns_rec{anlist = merge(Answ, MRRs)},
		    additional_info(?EPSILON, State#state{dns_resp = R})
	    end;
	{cut, _, _, NSRRs} ->
	    %% This is not part of our authoritative data.
	    Addrs = get_additional_addrs(NSRRs, Zone,PeerIP),
	    NSl = Resp#dns_rec.nslist,
	    ARl = Resp#dns_rec.arlist,
	    match_cache(?EPSILON,
			State#state{zone_answ = false,
				    zone = undefined,
				    dns_resp =
				      Resp#dns_rec{nslist = merge(NSl, NSRRs),
						   arlist = merge(ARl, Addrs)}});
	{wildcard, RRs} ->
	    MRRs0 = matching_rrs(RRs, Query#dns_query.type),
	    MRRs = patch_owner(MRRs0, QNAME),
	    Answ = Resp#dns_rec.anlist,
	    R = Resp#dns_rec{anlist = merge(Answ, MRRs)},
	    additional_info(?EPSILON, State#state{dns_resp = R});
	false ->
	    Orig = State#state.orig_q,
	    if
		Query#dns_query.domain == Orig#dns_query.domain ->
		    Header0 = Resp#dns_rec.header,
		    Header = Header0#dns_header{rcode = ?NXDOMAIN},
		    R = Resp#dns_rec{header = Header},
		    additional_info(?EPSILON, State#state{dns_resp = R,
							  zone_answ = false});
		true ->
		    additional_info(?EPSILON, State)
	    end
    end.

%%
%% Load balance RRs.
%%
load_balance([RR|RRs], Type, PeerIP) when RR#dns_rr.func == false, length(RRs) > 0 ->
    RRs1 = [RR|RRs],
    do_round_robin(RR#dns_rr.domain, Type, RRs1);
load_balance([RR|RRs], ?S_A, PeerIP) when RR#dns_rr.func /= false ->
    #dns_rr{func = {M,F,Args}, domain = Domain} = RR,
    case apply(M, F, Args ++ [Domain, ?S_A, PeerIP]) of
	{ok, BalanceIPs, TTL} ->
	    SetIPTTL = fun(BIP) -> RR#dns_rr{data = BIP,
					     ttl = TTL} end,
	    lists:map(SetIPTTL, BalanceIPs);
	{false, TTL} ->
	    %% All IP's are down.
	    %% Return all round robin !
	    RRs1 = [RR|RRs],
	    RR_RRs = do_round_robin(Domain, ?S_A, RRs1),
	    SetRRTTL = fun(R) -> R#dns_rr{ttl = TTL} end,
	    lists:map(SetRRTTL, RR_RRs)
    end;
load_balance(RRs, _, _PeerIP) -> RRs.

do_round_robin(Domain, Type, RRs) ->
    case dns_load:round_robin(Domain, Type, RRs) of
	{ok, OrderedRRs} -> OrderedRRs;
	_                -> RRs
    end.
	    
%%----------------------------------------------------------------------
%% Func: match_cache/2
%%       The match_cache state.
%%       (RFC1034, 4.3.2 - step 4)
%%----------------------------------------------------------------------

match_cache(?EPSILON, State) ->
    #state{current_q = Query,
	   dns_resp = Resp,
	   options = Opts} = State,
    QNAME = Query#dns_query.domain,
    Header0 = Resp#dns_rec.header,
    Header = Header0#dns_header{aa = which_aa(?NO_AUTHORITY, Header0, State)},
    case cache_lookup(QNAME) of
	{ok, [RR]} when RR#dns_rr.type == ?S_CNAME ->
	    Answ = Resp#dns_rec.anlist,
	    MRRs = patch_owner([RR], QNAME),
	    R = Resp#dns_rec{anlist = merge(Answ, MRRs),
			     header = Header},
	    if
		Query#dns_query.type == ?S_CNAME ->
		    additional_info(?EPSILON, State#state{dns_resp = R});
		true ->
		    Q = Query#dns_query{domain = RR#dns_rr.data},
		    S = State#state{current_q = Q,
				    dns_resp = R},
		    get_zone(?EPSILON, S)
	    end;
	{ok, RRs} ->
	    case matching_rrs(RRs, Query#dns_query.type) of
		[] when Header#dns_header.rd == ?RECURSION_DESIRED,
			?RECURSION_AVAIL(Opts) ->
		    S = State#state{dns_resp = Resp#dns_rec{header = Header}},
		    eval_recursive_query(?EPSILON, S);
		MRRs0 ->
		    MRRs = patch_owner(MRRs0, QNAME),
		    Answ = Resp#dns_rec.anlist,
		    R = Resp#dns_rec{anlist = merge(Answ, MRRs),
				     header = Header},
		    additional_info(?EPSILON, State#state{dns_resp = R})
	    end;
	_ ->
	    S = State#state{dns_resp = Resp#dns_rec{header = Header}},
	    case Header#dns_header.rd of
		?RECURSION_DESIRED when ?RECURSION_AVAIL(Opts) ->
		    eval_recursive_query(?EPSILON, S);
		_ ->
		    additional_info(?EPSILON, S)
	    end
    end.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%%
%%----------------------------------------------------------------------
handle_event({resent, _Query}, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%%
%%----------------------------------------------------------------------

handle_info({'EXIT', Pid, Reason}, wait_recursive, State)
  when Pid == State#state.recurse_pid ->
    wait_recursive({'EXIT', Pid, Reason}, State);
    
handle_info({Pid, Answ}, wait_recursive, State)
  when Pid == State#state.recurse_pid ->
    wait_recursive({Pid, Answ}, State);
    
handle_info({'EXIT', Pid, Reason}, StateName, StateData) ->
    {stop, Reason, StateData};
    
handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% 
%%----------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Reply to the querier.
%% Data is an encoded DNS message and Id is the
%% query identifier.
%%
reply(To, Data, Id) ->
    To ! {self(), response, list_to_binary(Data), Id},
    ok.

%% 
%% Find the nearest ancestor zone of QNAME.
%% Returns: {ok, #zone{}} | false
%% 
find_zone(QNAME) -> do_find_zone(tolower(QNAME)).

do_find_zone(Zone) ->
    case dns_catalog:zone(Zone) of
	{ok, Db} ->
	    Z = #zone{domain = Zone,
                      db = Db},
	    {ok, Z};
	_ when Zone /= "" ->
	    do_find_zone(next_zone_level(Zone));
	_ ->
	    false
    end.

next_zone_level([$.|Zone]) -> Zone;
next_zone_level([_|T])     -> next_zone_level(T);
next_zone_level([])        -> [].

%%
%% Make lower case only chars in a name.
%%
tolower([H|T]) when H >= $A, H =< $Z -> [H+32 | tolower(T)];
tolower([H|T]) -> [H | tolower(T)];
tolower([]) -> [].

%% 
%% Match down the zone label by label in QNAME.
%% Returns: {match, RRs} | {cut, Domain, RRs, NRRs} | {wildcard, RRs} | false
%% 
do_match_zone(Query, Zone) ->
    QNAME = tolower(Query#dns_query.domain),
    Zdb = Zone#zone.db,
    ZoneD = Zone#zone.domain,
    do_match_zone(QNAME, Zdb, ZoneD, top).

do_match_zone("", _, _, _) ->
    false;
do_match_zone(Domain, Zdb, Zdom, Level) ->
    case dns_zone:lookup(Zdb, Zdom, Domain) of
	{ok, RRs} when Level == top ->
	    {match, RRs};
	{ok, RRs} ->
	    case matching_rrs(RRs, ?S_NS) of
		[]                       -> false;
		NRRs when Domain == Zdom -> false;
		NRRs                     -> {cut, Domain, RRs, NRRs}
	    end;
	_ ->
	    Subdomain = sub_label(Domain),
	    case dns_zone:lookup(Zdb, Zdom, "*." ++ Subdomain) of
		{ok, RRs} ->
		    {wildcard, RRs};
		_ ->
		    do_match_zone(Subdomain, Zdb, Zdom, false)
	    end
    end.

sub_label(L) ->
    case string:chr(L, $.) of
	0 -> "";
	I -> string:substr(L, I + 1)
    end.

%%
%% Get address (A) information if existing for all RRs.
%% Check the Zone (first the current zone for glue data then
%% a matching zone) before the cache.
%%
get_additional_addrs([RR|RRs], Zone, PeerIP) ->
    Domain = RR#dns_rr.data,
    case get_a_addrs(Domain, Zone,PeerIP) of
	[] ->
	    get_additional_addrs(RRs, Zone,PeerIP);
	ARRs ->
	    ARRs ++ get_additional_addrs(RRs, Zone,PeerIP)
    end;
get_additional_addrs([], _,_PeerIP) ->
    [].

%%
%% Get additional address (A) information if existing for Domain.
%%
get_a_addrs(Domain, Zone,PeerIP) ->
    case get_additional_from_zone(Zone, Domain, PeerIP) of
	[] ->
	    case additional_lookup(cache, Domain,PeerIP) of
		[] ->
		    [];
		MRRs ->
		    MRRs
	    end;
	MRRs ->
	    MRRs
    end.

get_additional_from_zone(Zone, Domain, PeerIP) ->
    case additional_lookup(Zone, Domain, PeerIP) of
	[] -> case find_zone(Domain) of
		  {ok, Z} -> additional_lookup(Z, Domain, PeerIP);
		  _       -> []
	      end;
	RRs -> RRs
    end.

additional_lookup(cache, Domain, PeerIP) ->
    case dns_cache:lookup(Domain) of
	{ok, RRs} -> matching_rrs(RRs, ?S_A); 
	_         -> []
    end;
additional_lookup(Zone, Domain, PeerIP) when record(Zone, zone) ->
    #zone{db = Zdb, domain = Zdom} = Zone,
    case dns_zone:lookup(Zdb, Zdom, Domain) of
	{ok, RRs} ->
	    RRs1 = matching_rrs(RRs, ?S_A),
	    load_balance(RRs1, ?S_A,PeerIP);
	_ ->
	    []
    end;
additional_lookup(_, _,_PeerIP) ->
    [].

%%
%% Lookup a domain in the cache.
%% Return {ok, RRs} | {error, Error}
%%
cache_lookup(QNAME) ->
    dns_cache:lookup(QNAME).

%%
%% Keep only RRs of type, or all if type = ANY.
%%
matching_rrs([RR|RRs], Type) when RR#dns_rr.type == Type ->
    [RR|matching_rrs(RRs, Type)];
matching_rrs([RR|RRs], ?S_ANY) ->
    [RR|matching_rrs(RRs, ?S_ANY)];
matching_rrs([_|RRs], Type) ->
    matching_rrs(RRs, Type);
matching_rrs([], _) ->
    [].

%%
%% Set the domain in all RRs.
%% This is to ensure that (low/upper) case is preserved from the query.
%% Also sets the correct domain name for wildcard matches.
%%
patch_owner([RR|RRs], Domain) ->
    [RR#dns_rr{domain = Domain}|patch_owner(RRs, Domain)];
patch_owner([], _) ->
    [].

%%
%% Choose the NewAA only if this was the original question.
%% Example: If we followed an authoritative CNAME to non-authorita
%% we should mark this as authoritative anyhow.
%%
which_aa(NewAA, #dns_header{aa = OldAA}, State) ->
    #state{current_q = Query,
	   orig_q = Orig} = State,
    if
	Query#dns_query.domain == Orig#dns_query.domain ->
	    NewAA;
	true ->
	    OldAA
    end.

%%
%% Merge MRRs if nonexisting in RRs.
%%
merge(RRs, MRRs) ->
    KeepMRRs = keep(MRRs, RRs),
    RRs ++ KeepMRRs.

keep([MRR|MRRs], RRs) ->
    case merge_p(MRR, RRs) of
	true -> [MRR|keep(MRRs, RRs)];
	_    -> keep(MRRs, RRs)
    end;
keep([], _) ->
    [].
    
merge_p(#dns_rr{domain = Domain, data = Data}, MRRs) ->
    F = fun(RR) when RR#dns_rr.domain == Domain,
		     RR#dns_rr.data == Data ->
		true;
	   (_) ->
		false
	end,
    not lists:any(F, MRRs).

