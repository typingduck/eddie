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
%%% File    : dns_load.erl
%%% Author  : Magnus Fr|berg
%%% Author2 : Pekka Hedqvist <pekka@eddieware.org>
%%% Purpose : The load information handling part of the DNS server.
%%% Created :  4 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Modified:  13 Sep 1999 by Pekka Hedqvist <pekka@eddieware.org>
%%% Done:     -Added md5 signed load messages, bit more secure now..
%%%           -Added registration key checks
%%%           -Added N cached random numbers.
%%%           -Added preferential load balancing per lan with congestion levels
%%%           -Fixed bug, load balance only returned the IPs in the LAN
%%%           the IP were selected from, not all IPs in the domain.
%%%           -Fixed bug, lan fractions only updated for the last defined   
%%%           lan in a domain!!
%%%           -Fixed bugg with initial fraction value for LAN. Previosuly 
%%%           they received assymetrical decreasing start values. After 
%%%           a while that evenes out but with many lans the last lan 
%%%           could have a very small fraction..
%%%           -Fixed some other stuffs I cannot remember..
%%%----------------------------------------------------------------------

-module(dns_load).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-vsn('$Revision: /main/eddie/eddie-1.0/eddie-0.83/1').
-author('pekka@eddieware.org').

-behaviour(gen_server).

%% External exports
-export([start_link/0, round_robin/3, delete_round_robin/2,
	 load_balance/5,
	 add_ip_cluster/1, delete_ip_cluster/1,
	 random/0, sync_random/0, random/1, sync_random/1]).

-export([lan_divide/2,one_per_host/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([get_all_but_sign/1,get_p_opcode/1, get_p_ip/1, get_p_host/1, get_p_fe/1,
	get_p_be/1]).
-record(state, {udp,
		expired,
		port,
	        db,
		cookie,
		pdb,
		ip_db,
		cid = 0,       % IP cluster id conter
		netmasks,
		load_timer,
	        flush_timer}).

-define(LOADINFO_PORT, 4567). % Default

-record(round_robin, {domain_type,
		      data}).

-define(FLUSH_TIMEOUT, 40000).

-define(TIMES_PREF, 2). % times to iterate searching for pref values
			% after going through the list of prefs this
			% many times it resorts to plain lb.
			% also determines how many chached randomvalues

-define(MAX_CONGESTION_LEVEL, 500). 

-define(MIN(X,Y), (if X < Y -> X;  true  -> Y end)). % smallest of two values
-define(MAX(X,Y), (if X > Y -> X;  true  -> Y end)). % largest of two values

-define(REG_TIMEOUT, 1000 * 60 * 60 * 24). %% how often to check: 24 hour now

-define(LOAD_TIMEOUT_SEC,
	trunc(?LOAD_TIMEOUT / 5)). %% how often to update the load in seconds 
-define(CL_up,
	?MAX(1, ?LOAD_TIMEOUT_SEC * (?MAX_CONGESTION_LEVEL div 100))).
-define(CL_down,
	?CL_up div ?LOAD_TIMEOUT_SEC).

-define(UP, up).
-define(DOWN, down).

-record(ip, {ip,
	     cid,             % cluster id
	     status = ?DOWN,  % ?UP | ?DOWN
	     host,            % At host
	     n,               % # of IP's in cid.
	     f     = 1,
	     lan,
	     lan_f = 1,
	     l_lan_f = 1,     % latest lan_f this round
	     time  = 0
	    }).

-define(MATCH_IP(IP, Cid, Status, Host),
	#ip{ip      = IP,
	    cid     = Cid,
	    status  = Status,
	    host    = Host,
	    n       = '_',
	    f       = '_',
	    lan     = '_',
	    lan_f   = '_',
	    l_lan_f = '_',
	    time    = '_'}).

-define(MIN_FRACTION, 0.01).  % == 1%
-define(lowest(F), if  F < ?MIN_FRACTION -> ?MIN_FRACTION;
		       true              -> F
		   end).

-record(cluster, {
	  cid,
	  ips       = [], % [IP] sorted set repr. the cluster.
	  ref_cnt   = 1,  % delete when ref_cnt == 0
	  fractions = []  % [#lan_fr]
	 }).


-record(random, {
	  k = random,
	  ulen,
	  u % list of numbers 0 =< r =< 1
	 }).         

-define(MATCH_CLUSTER(Cid, IPs),
	#cluster{
		       cid       = Cid,
		       ips       = IPs,
		       ref_cnt   = '_',
		       fractions = '_'
		      }).

-record(lan_fr,	{
	  lan,
	  f,
	  congestion_level = 0,   % int
	  time,                   % timestamp for lf
	  ips = []                % [#ip_fr]
	 }).  

-record(ip_fr, {
	  ip,
	  f
	 }).

-record(pref_lan, {
	  domain,      %% "www.foo.bar"
          no,          %% ordering integer from 1-N, how many domains there is,
	  o_netmask,   %% {{172,30,128,32},{255,255,255,248}}
	  lans = []    %% ordered pref list of lan:s for this ip 
	 }).           %% (this is the LAN not #lan_f)
%% in the form of [{{172,30,128,32},{255,255,255,248}}]

%% The maximum time load information is allowed to exist without
%% being updated. The load server sends info every 5 seconds, i.e.
%% 31 secs implies that 6 or at least 5 updates are missing.
%% This is only needed in case the last machine at a LAN crashes,
%% i.e. it couldn't tell us that it went down.
-define(MAX_INACTIVE, 31). 

-define(i16(X1,X0),
        (?u16(X1,X0) -  (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(u16(X1,X0), (((X1) bsl 8) bor (X0))).

-include_lib("../../misc/include/logger.hrl").
-include_lib("../../load_info/src/load_server.hrl").
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% LANList is a list of LAN id:s in the preferential order!
set_ip_lan_pref(IP, Domain, LANList) ->
    gen_server:call(?MODULE, {set_ip_lan_pref, IP, Domain, LANList}).

load_balance(Cid, TTL, Domain, Type, PeerIP) ->
    case get_pref_ips(PeerIP, Cid, TTL, Domain, Type) of
	no_pref -> get_lb_ips(Cid, TTL, Domain, Type);
	X -> X
    end.

%% Perform the rotation here, i.e. we don't have to copy all
%% RRs to the dns_load process.
round_robin(Domain, Type, List) ->
    case gen_server:call(?MODULE, {round_robin, {Domain, Type},
				   length(List)}) of
	{ok, N} -> {ok, rotate(List, N)};
	Error   -> Error
    end.

%% Delete the (eventually existing) entry in DB.
delete_round_robin(Domain, Type) ->
    gen_server:cast(?MODULE, {delete_round_robin, {Domain, Type}}).

%% Returns a cid.
add_ip_cluster(IPs) ->
    gen_server:call(?MODULE, {add_ip_cluster, IPs}).

delete_ip_cluster(Cid) ->
    gen_server:call(?MODULE, {delete_ip_cluster, Cid}).

random() ->
    [#random{u = U}|_] = ets:lookup(dns_load_ip, random),
    new_random(1),
    hd(U).

random(N) ->
    [#random{u = U}|_] = ets:lookup(dns_load_ip, random),
    new_random(N),
    lists:sublist(U,N).

%% Ensures that the next time the same process calls
%% (sync_)random/0 a fresh random value is returned.
sync_random() ->
    [#random{u = U}|_] = ets:lookup(dns_load_ip, random),
    sync_new_random(1),
    hd(U).
sync_random(N) ->
    [#random{u = U}|_] = ets:lookup(dns_load_ip, random),
    sync_new_random(N),
    lists:sublist(U,N).

new_random(N) -> gen_server:cast(?MODULE,{new_random,N}).
sync_new_random(N) -> gen_server:call(?MODULE, {sync_new_random,N}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    Expired = false, %% read regfile and setit 
    Port = get_port(),
    case open_udp(Port) of
	{ok, Udp} ->
	    ?INFO(?F("listening on load data on port ~p",[Port])),
	    Db = ets:new(dns_load, [{keypos, #round_robin.domain_type}]),
	    IPDb = ets:new(dns_load_ip, [named_table, {keypos, #ip.ip}, bag]),
	    PDB = ets:new(ip_pref,[named_table,{keypos,#pref_lan.domain},bag]),
	    LTimer   = init_timer(load),
	    FTimer   = init_timer(flush),
	    NetMasks = get_netmasks(),
	    Cookie = get_cookie(),
	    set_prefs(PDB), %% set preferentials
	    init_random(IPDb, ?TIMES_PREF),
%	    dns_test:init_test(),
 	    {ok, #state{udp         = Udp,
			expired     = Expired,
			cookie      = Cookie,
			port        = Port,
			db          = Db,
			ip_db       = IPDb,
			pdb         = PDB,
			netmasks    = NetMasks,
			load_timer  = LTimer,
		        flush_timer = FTimer}};
	Error ->
	    ?FATAL(?F("Can't open port ~p - ~p", [Port, Error])),
	    {stop, Error}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call({set_ip_lan_pref, Netmask, Domain, LANList}, _From, State) ->
    PDB = State#state.pdb, 
    ets:insert(PDB, #pref_lan{domain = Domain,
			      o_netmask = Netmask,
			      lans = LANList}),
    {reply, ok, State};


handle_call({round_robin, Key, Length}, _From, State) ->
    Db = State#state.db,
    case ets:lookup(Db, Key) of
	[Info] ->
	    N = rotate_times(Info#round_robin.data, Length),
	    ets:delete(Db, Key),
	    ets:insert(Db, Info#round_robin{data = N}),
	    {reply, {ok, N}, State};
	_ ->
	    ets:insert(Db, #round_robin{domain_type = Key,
					data = 1}),
	    {reply, {ok, 1}, State}
    end;

handle_call({add_ip_cluster, IPs}, _From, State) ->
    #state{cid = Cid, ip_db = IPDb} = State,
    {RCid, SCid} = do_add_ip_cluster(IPs, Cid, IPDb),
    {reply, RCid, State#state{cid = SCid}};

handle_call({delete_ip_cluster, Cid}, _From, State) ->
    #state{ip_db = IPDb} = State,
    try_delete_ip_cluster(Cid, IPDb),
    {reply, ok, State};

handle_call({sync_new_random, N}, _From, State) ->
    new_random(State#state.ip_db,N),
    {reply, ok, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast({delete_round_robin, Key}, State) ->
    #state{db = Db} = State,
    ets:delete(Db, Key),
    {noreply, State};

handle_cast({new_random, N}, State) ->
    new_random(State#state.ip_db, N),
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info({udp, Udp, IP, _, RawPacket}, State) when State#state.udp == Udp,
						      State#state.expired /= true ->
    case validate_ip(IP, State#state.netmasks) of
	{true, LAN} ->
 	    case validate_sign(RawPacket, State#state.cookie) of
		{true, P} ->
		    OPCODE = get_p_opcode(P),
		    IPP = get_p_ip(P),
		    case OPCODE of
			?LOADINFO ->
			    load_info([?LOADINFO, IPP, get_p_host(P), get_p_fe(P),
				      get_p_be(P)], LAN, State, IP),
	    {noreply, State};
			?IP_UP ->
			    load_info([?IP_UP, IPP, get_p_host(P)], LAN, State, IP),
	    {noreply, State};
			?IP_DOWN ->
			    load_info([?IP_DOWN, IPP], LAN, State, IP),
			    {noreply, State}
		    end;
		false ->
	    ?INFO(?F("Bad signed load data!!",[])),
	    {noreply, State}
    end;
	false ->
 	    ?INFO(?F("Not allowed IP (~p) tried to send load info", [IP])),
 	    {noreply, State}
    end;

handle_info({udp, Udp, _IP, _, _RawPacket}, State) when State#state.udp == Udp ->
    {noreply, State};			

handle_info({udp_closed, Udp}, State) when State#state.udp == Udp ->
    case open_udp(State#state.port) of
	{ok, NewUdp} ->
	    ?INFO(?F("UDP port went down, successfully reopened it", [])),
	    {noreply, State#state{udp = NewUdp}};
	Error ->
	    ?FATAL(?F("Can't (re)open port ~p - ~p",
		      [State#state.port, Error])),
	    {stop, Error, State}
    end;

handle_info(load_timeout, State) ->
    #state{cid = Cid, ip_db = IPDb} = State,
    update_fractions(Cid, IPDb),
    Timer = init_timer(load),
    {noreply, State#state{load_timer = Timer}};
handle_info(flush_timeout, State) ->
    #state{ip_db = IPDb} = State,
    flush_db(times(), IPDb),
    Timer = init_timer(flush),
    {noreply, State#state{flush_timer = Timer}};

handle_info(Info, State) ->  {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    erlang:cancel_timer(State#state.flush_timer),
    erlang:cancel_timer(State#state.load_timer),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

times() ->
    {Mega,Secs,_} = erlang:now(),
    Mega*1000000 + Secs.

get_cookie() ->
    case dns_catalog:cookie() of
	{ok, C} -> C;
	_       -> []
    end.
    

get_port() ->
    case dns_catalog:lb_port() of
	{ok, Port} -> Port;
	_          -> ?LOADINFO_PORT
    end.

%%
%% Start timer for cleanup (flush) of not valid load information stuff
%% or load timer for update up fraction lists.
%%
init_timer(flush) ->
    erlang:send_after(?FLUSH_TIMEOUT, self(), flush_timeout);
init_timer(load) ->
    erlang:send_after(?LOAD_TIMEOUT, self(), load_timeout).

%%
%% Get the configured netmasks that validates and identifies
%% IP addresses.
%%
get_netmasks() ->
    {ok, NetMasks} = dns_catalog:netmasks(),
    NetMasks.

set_prefs(PDB) ->
%% read the prefs, collect them into a list of lists where 
%% each list is all the with the same domain
%% create a new list with which numbers the ones with the same
%% domain from 1 to N, where 1 is the first one originally defined 
%% first in the config file. Then insert them in the table so we 
%% know the original ordering.
    {ok, PrefNetMasks} = dns_catalog:prefs(),
    CPrefNetMasks = collect(PrefNetMasks,
			    fun(X, I) -> A = element(#pref_lan.domain, X),
					 B = element(#pref_lan.domain, I),
					 A == B
			    end),
    DNCPrefNetMasks =
	lists:map(fun(SameDomainL) ->
			  lists:foldl(fun(X,{L,N}) -> {[{N,X}|L],N-1} end,
				      {[],length(SameDomainL)},
				      SameDomainL)
		  end,
		  CPrefNetMasks),
    lists:map(fun({SameDomainL,_}) ->
		      lists:foreach(fun({N, {OIPMask,Domain,PrefLanMaskList}})->
					    ets:insert(PDB,
						       #pref_lan{
							 no        = N, %% ordering no!
							 domain    = Domain,
							 o_netmask = OIPMask,
							 lans = PrefLanMaskList})
				    end,
				    SameDomainL)
	      end,
	      DNCPrefNetMasks). 

%% from a list A it returns a list of lists where 
%% each sublist is a collection of all all elements
%% in the list A that which are true according to the
%% fun Fun.
collect([F|R], Fun) ->
    Items = [I || I <- R, Fun(F, I)],
    case Items of
	[]    -> [[F] | collect(R, Fun)];
	_Else -> [[F|Items] | collect(R -- Items,Fun)]
    end;
collect([],_) -> [].

%%
%% check that Packet signature is valid ackording to cookie
%% 
%%

%% don't check sign if no cookie is configured
validate_sign(Packet, []) -> {true, Packet};
validate_sign(Packet, Cookie) ->
    case get_all_but_sign(Packet) of
	{VPacket, [SLen|Sign1]} ->
	    Sign2 =
		binary_to_list(crypto:md5_final(
				 crypto:md5_update(crypto:md5_init(),
						   Cookie ++ VPacket))),
	    if Sign1 == Sign2 ->
		    {true, VPacket};
       true ->
	    false
	    end;
	_ -> false
    end.
	    
%%
%% Check that IP matches a netmask and return the matching
%% netmasks NetIP as the id of the LAN.
%%
validate_ip(IP, NetMasks) ->
    do_validate(NetMasks, IP).

do_validate([{NetIP, NM}|NMs], IP) ->
    case do_band(NM, IP) of
	NetIP -> {true, NetIP};
	_     -> do_validate(NMs, IP)
    end;
do_validate([], _) ->
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

%%
%% Decode UDP packet and store information about IP load.
%%
load_info(Packet, LAN, State, SIP) ->
    ?INFO(?F("dns server received ~w",[Packet])),
    #state{ip_db = IPDb} = State,
    store_load_info(Packet, LAN, IPDb, SIP).

store_load_info([?LOADINFO, IP, Host, FE_f, BE_f], LAN, IPDb, SIP)
  when number(FE_f), number(BE_f) ->
    case ets:lookup(IPDb, IP) of
	[] ->
	    ?INFO(?F("Warning IP ~p not member of load balanced domain (~p)", [SIP, IP])),
	    ok;
	IPs ->
	    F = fun(IPr) when IPr#ip.status == ?DOWN ->
			?INFO(?F("IP ~p previously DOWN, received loadinfo, now UP",
				 [IPr#ip.ip])),
			ip_up(IPr, LAN, Host, IPDb);
		   (IPr) ->
			ip_info(IPr, LAN, FE_f, BE_f, IPDb)
		end,
	    lists:foreach(F, IPs)
    end;

store_load_info([?IP_UP, IP, Host], LAN, IPDb, SIP) ->
    case ets:lookup(IPDb, IP) of
	[] ->
	    ?INFO(?F("~p sent bad load info (up): invalid IP (~p)",
		     [SIP, IP])),
	    ok;
	IPs ->
	    F = fun(IPr) ->
			if IPr#ip.status == ?DOWN ->
				?INFO(?F("IP ~p previously DOWN, received "
					 "loadinfo, now UP", [IPr#ip.ip]));
			   true -> true
			end,
			ip_up(IPr, LAN, Host, IPDb) 
		end,
	    lists:foreach(F, IPs)
    end;

store_load_info([?IP_DOWN, IP], LAN, IPDb, SIP) ->
    case ets:lookup(IPDb, IP) of
	[] -> 
	    ?INFO(?F("~p sent bad load info (down): invalid IP (~p)",
		     [SIP, IP])),
	    ok;
	IPs ->
	    F = fun(IPr) ->
			if IPr#ip.status == ?UP ->
				?INFO(?F("IP ~p previously UP, received "
					 "loadinfo, now DOWN",[IPr#ip.ip]));
			   true -> true
			end,
			ip_down(IPr, LAN, IPDb) 
		end,
	    lists:foreach(F, IPs)
    end;

store_load_info(_, _, _, SIP) ->
    ?INFO(?F("~p sent invalid load info packet", [SIP])),
    ok.

%%
%% Store new information about IPr#ip.ip
%%
ip_info(IPr, LAN, FE_f, BE_f, IPDb) ->
    #ip{f = F0, lan_f = LF0, l_lan_f = LLF0} = IPr,
    ets:match_delete(IPDb, IPr),
    Times = times(), 
    F   = ?lowest(F0),
    LF  = ?lowest(LF0),
    LLF = ?lowest(LLF0),
    ets:insert(IPDb, IPr#ip{f       = ?lowest(F * FE_f),
			    lan_f   = ?lowest(LF * BE_f),
			    l_lan_f = ?lowest(LLF * BE_f),
			    lan     = LAN,
			    time    = Times}).

%%
%% Add information about IPr#ip.ip in its cluster
%% and store new IPr info.
%%
ip_up(IPr, LAN, Host, IPDb) ->
    #ip{cid = Cid, lan_f = LF, l_lan_f = LLF} = IPr,
    ets:match_delete(IPDb, IPr),
    Times = times(),
    case ets:match_object(IPDb, ?MATCH_IP('_', Cid, ?UP, Host)) of
	[] ->
	    ets:insert(IPDb, IPr#ip{f = initial_f(IPDb, Cid, LAN),
				    lan     = LAN,
				    status  = ?UP,
				    host    = Host,
				    lan_f   = ?lowest(LF),
				    l_lan_f = ?lowest(LLF),
				    time    = Times}),
	    update_cid_fractions(Cid, IPDb);
	IPrs ->
	    F = split_share_fraction(IPDb, IPrs),
	    ets:insert(IPDb, IPr#ip{f       = F,
				    lan     = LAN,
				    status  = ?UP,
				    host    = Host,
				    lan_f   = ?lowest(LF),
				    l_lan_f = ?lowest(LLF),
				    time    = Times}),
	    update_cid_fractions(Cid, IPDb)
    end.

%% Several IP's located at the same host !
%% Shares the same fraction.
%% This IP's will be updated using the same factor, i.e.
%% the same factor is sent in the corresponding ?LOAD_INFO
%% package for every IP.
split_share_fraction(IPDb, [IPr|IPrs]) ->
    L = length(IPrs) + 1,
    F = (IPr#ip.f * L) / (L + 1), %% +1 really right..? /pekka
    Fun = fun(Ir) -> ets:match_delete(IPDb, Ir),
		     ets:insert(IPDb, Ir#ip{f = F})
	  end,
    lists:foreach(Fun, [IPr|IPrs]),
    F.

%% Set the initial fraction relative the current number of
%% active IP addresses (only one IP per host counts).
initial_f(IPDb, Cid, LAN) ->
    case ets:lookup(IPDb, Cid) of
	[] ->
	    1;
	[C] ->
	    IPRs = one_per_host(get_active_ips(C#cluster.ips, Cid, IPDb), []),
	    IPRs2 = [IPR || IPR <- IPRs, IPR#ip.lan == LAN],
	    L = length(IPRs2),
	    if L == 0 -> 1;
	       true -> ?lowest(1 / (L)) 
	    end
    end.

one_per_host([IPr|IPRs], Hosts) ->
    Host = IPr#ip.host,
    case lists:member(Host, Hosts) of
	true -> one_per_host(IPRs, Hosts);
	_    -> [IPr|one_per_host(IPRs, [Host|Hosts])]
    end;
one_per_host([], _) ->
    [].

%%
%% Delete information about IPr#ip.ip in its cluster
%% and store new IPr info.
%%
%% Needs ?MATCH_IP/4 due to flush_db which can call ip_down/3 with
%% old IPr as update_cid_fractions/2 updates all other IPr's.

ip_down(IPr, LAN, IPDb) ->
    #ip{ip = IP, cid = Cid, n = N, host = Host} = IPr,
    ets:match_delete(IPDb, ?MATCH_IP(IP, Cid, '_', '_')),
    ets:insert(IPDb, IPr#ip{f = 1 / N, status = ?DOWN,
			    lan = LAN, lan_f = 1, l_lan_f = 1}),
    case ets:match_object(IPDb, ?MATCH_IP('_', Cid, ?UP, Host)) of
	[] ->	    update_cid_fractions(Cid, IPDb);
	IPrs ->	    reuse_share_fraction(IPDb, IPrs),
		    update_cid_fractions(Cid, IPDb)
    end.

%% Several IP's located at the same host !
%% Shares the same fraction.
%% Reuse fraction part shared by this IP's.
reuse_share_fraction(IPDb, [IPr|IPrs]) ->
    L = length(IPrs) + 1,
    F = (IPr#ip.f * (L + 1)) / L,
    Fun = fun(Ir) ->
		  ets:match_delete(IPDb, Ir),
		  ets:insert(IPDb, Ir#ip{f = F})
	  end,
    lists:foreach(Fun, [IPr|IPrs]).

%% see GeoLB paper
calc_cl(BE_f, CL) when BE_f < 1.0 ->
    NewCL = CL + ?CL_up,
    if	 NewCL > ?MAX_CONGESTION_LEVEL ->  ?MAX_CONGESTION_LEVEL;
	 true                          ->  NewCL
    end;
calc_cl(_BE_f, CL) ->
    NewCL = CL - ?CL_down,
    if	NewCL < 0 -> 0;
	true      -> NewCL
    end.

%%
%% Update all cluster fraction lists.
%%
update_fractions(MaxCid, IPDb) ->
    update_fractions(1, MaxCid, IPDb).

update_fractions(Cid, MaxCid, IPDb) when Cid =< MaxCid ->
    update_cid_fractions(Cid, IPDb),
    update_fractions(Cid + 1, MaxCid, IPDb);
update_fractions(_, _, _) ->
    ok.

%%
%% Update the Cid cluster fraction lists.
%%
update_cid_fractions(Cid, IPDb) ->
    case ets:lookup(IPDb, Cid) of
	[] ->  ok;
	[C] -> ActiveIPRs = get_active_ips(C#cluster.ips, Cid, IPDb),
	       update_cid_fractions(Cid, IPDb, ActiveIPRs, C)
    end.

update_cid_fractions(Cid, IPDb, ActiveIPRs, C) ->
    FacLanFrL = create_fractions(ActiveIPRs, IPDb), %[{LatestF, #lan_fr}..],
    LanFrL = update_lan_cong_levels(FacLanFrL, C),
    case LanFrL of
	Fr when C#cluster.fractions /= Fr ->
	    ets:insert(IPDb, C#cluster{fractions = LanFrL}),
	    ets:match_delete(IPDb, C),
	    ok;
	_Else ->
	    ok
    end.

%% FacLanFrL = [{nonormF, #lan_fr}] 
%% returns #lan_fr
%%update_lan_cong_levels(FacLanFrL, C) when C#cluster.fractions /= [] ->
update_lan_cong_levels(FacLanFrL, C) ->
    OldFracs = C#cluster.fractions,
    F =	fun({LanF, Lan_Fr}) ->
		case lists:keysearch(Lan_Fr#lan_fr.lan,#lan_fr.lan, OldFracs) of
		    {value, LF} ->
			if Lan_Fr#lan_fr.time > LF#lan_fr.time -> %% update_cong
				NCL = calc_cl(LanF, LF#lan_fr.congestion_level),
				Lan_Fr#lan_fr{ congestion_level = NCL};
			   true -> % copy old values
				Lan_Fr#lan_fr{ 
				  congestion_level = LF#lan_fr.congestion_level}
				%% cluster_fraction = LF#lan_fr.cluster_fraction}
			end;
		    _ -> Lan_Fr %% really not possible..?
		end
	end,
    lists:map(F, FacLanFrL).

get_active_ips([IP|IPs], Cid, IPDb) ->
    case ets:match_object(IPDb, ?MATCH_IP(IP, Cid, ?UP, '_')) of
	[]    -> get_active_ips(IPs, Cid, IPDb);
	[IPr] -> [IPr|get_active_ips(IPs, Cid, IPDb)]
    end;
get_active_ips([], _, _) -> [].

%%
%% create_fractions(IPRs, IPDb) -> [#lan_fr]
%%
%% Create a new fraction list.
%% As a side effect of this function all IP#ip.f will be normalized
%% and all IP's be stored a new record.
%%
create_fractions([], _) ->
    [];
create_fractions(IPRs, IPDb) ->
    LIPRs0  = lan_divide(IPRs, []),  % [{LAN, [#ip]}]
    LIPRs1  = set_latest_f(LIPRs0),  % [{LAN, {T, F, LatestF}, [#ip]}],
    LIPRs   = normalize_lan(LIPRs1), % [{LAN, {T, LatestF, F/SUMF}, [#ip]}],
    create_ip_fractions(LIPRs, IPDb). % return {LatestF, #lan_fr}

lan_divide([IPr|IPRs], Acc) ->
    case lists:keysearch(IPr#ip.lan, 1, Acc) of
	{value, {LAN, LIPRs}} ->
	    lan_divide(IPRs,
		       lists:keyreplace(LAN, 1, Acc, {LAN, [IPr|LIPRs]}));
	_ ->
	    lan_divide(IPRs, [{IPr#ip.lan, [IPr]}|Acc])
    end;
lan_divide([], Acc) -> Acc.

%%
%% Set the latest f value per LAN according to
%% the IP records.
%%   [{LAN, [#ip]}] -> [{LAN, {T,F}, [#ip]}]
%%
set_latest_f([{LAN, IPRs}|LIPRs]) ->
    TF = get_latest_f(IPRs, 0, 0, 0),
    [{LAN, TF, IPRs}|set_latest_f(LIPRs)];
set_latest_f([]) -> [].

get_latest_f([#ip{time = T, lan_f = F, l_lan_f = LLF}|IPRs], Tm, _, _) when T > Tm ->
    get_latest_f(IPRs, T, F, LLF);
get_latest_f([_|IPRs], Tm, F, LLF) ->
    get_latest_f(IPRs, Tm, F, LLF);
get_latest_f([], T, F, LLF) -> {T, F, LLF}.

%%
%% Normalize the fraction value related to each LAN in 
%% respect to the sum of all LAN's fraction values.
%%
normalize_lan(LIPRs) ->
    Fn = fun({_LAN, {_T, F, _LF}, _IPS}, Acc) -> Acc + F end,
    SumF = lists:foldl(Fn, 0, LIPRs),
    Fn1 = fun({LAN, {T, F, LF}, IPRs}) -> {LAN, {T, LF, F/SumF}, IPRs} end,
    lists:map(Fn1, LIPRs).

%%
%% create_ip_fractions(LIPRs, IPDb) -> [{Latesf, #lan_fr}]
%%
%% Create the IP fractions per LAN and store new IP records
%% in db.
%%
create_ip_fractions([{LAN, {T, LatestF, F}, IPRs}|LIPRs], IPDb) ->
    FIPs = cr_ip_fractions(IPRs, F, IPDb),
    LF = {LatestF, #lan_fr{lan = LAN, time = T, f = F, ips = FIPs}},
    [LF | create_ip_fractions(LIPRs, IPDb)];
create_ip_fractions([], _) ->
    [].

%%
%% cr_ip_fractions/3 -> [#ip_fr]
%% and insert f and lanf
cr_ip_fractions(IPRs, LanF, IPDb) ->
    SumF = lists:foldl(fun(#ip{f = F}, Acc) -> Acc + F end, 0, IPRs),
    Fn = fun(IPr) ->
		 #ip{f = F, ip = IP} = IPr,
		 ets:match_delete(IPDb, IPr),
		 F1 = F / SumF,
		 ets:insert(IPDb, IPr#ip{f = F1, lan_f = LanF, l_lan_f = 1}),
		 %% l_lan_f = 1, reset value !
		 #ip_fr{ip = IP, f = F1}
	 end,
    lists:map(Fn, IPRs).

%%
%% Number of times to rotate a list of length L.
%% Should be one more than the last N.
%% Used for the round robin mechanism.
%%
rotate_times(N, N) -> 1;
rotate_times(N, L) -> N + 1.

%%
%% Move head of list to the tail N number of times.
%% Used for the round robin mechanism.
%%
rotate(List, N) -> rotate(List, N, []).
rotate(List, 1, Ack) -> List ++ lists:reverse(Ack);
rotate([H|T], N, Ack) -> rotate(T, N - 1, [H|Ack]).

%%
%% Mark all IP's not updatde during the last ?MAX_INACTIVE
%% period as ?DOWN.
%%
%% First search the db for all timed out IP's, when mark
%% them ?DOWN in the db.state
%% Each timed out IP results in an update of the associated
%% cluster.
%%
flush_db(Time, Db) ->
    DownIPs = flush_db(ets:first(Db), Time, Db, []),
    F = fun(IPr) -> ip_down(IPr, IPr#ip.lan, Db) end,
    lists:foreach(F, DownIPs).

flush_db('$end_of_table', _, _, Acc) -> Acc;
flush_db(IP, Time, Db, Acc) when tuple(IP) ->
    F = fun(#ip{time = TM}, IAcc) when Time - TM < ?MAX_INACTIVE ->
	        IAcc;
	   (IPr, IAcc) when IPr#ip.status == ?UP ->
		#ip{cid = Cid} = IPr,
		?INFO(?F("Flushes timedout load balanced IP ~p:~p",
			 [IP, Cid])),
		[IPr|IAcc];
	   (_, IAcc) ->
		IAcc
	end,
    DownIPs = lists:foldl(F, [], ets:lookup(Db, IP)),
    flush_db(ets:next(Db, IP), Time, Db, DownIPs ++ Acc);
flush_db(Key, Time, Db, Acc) ->
    flush_db(ets:next(Db, Key), Time, Db, Acc).

%%    
%% Add a new IP cluster.
%% A cluster is the set of IP addresses (A records) a domain
%% name has associated.
%%
%% A new Cid instance is always created before the old is
%% deleted in order to make sure that load_balance/1 always
%% has access to a Cid.
%%    
do_add_ip_cluster([], Cid, IPDb) ->
    {{error, no_ips}, Cid};
do_add_ip_cluster(IPs, Cid, IPDb) ->
    case get_create_cid(IPs, Cid, IPDb) of
	{true, RCid} ->
	    F = fun(IP) ->
			N = length(IPs),
			ets:insert(IPDb, #ip{ip = IP,
					     f = 1 / N,
					     n = N,
					     cid = RCid})
		end,
	    lists:foreach(F, IPs),
	    {RCid, RCid};
	{false, RCid} ->
	    {RCid, Cid}
    end.

get_create_cid(IPs0, Cid0, IPDb) ->
    IPs = lists:sort(make_set(IPs0)),
    case ets:match_object(IPDb, ?MATCH_CLUSTER('_', IPs)) of
	[] ->
	    Cid = Cid0 + 1,
	    ets:insert(IPDb, #cluster{cid = Cid,  ips = IPs}),
	    {true, Cid};
	[C] ->
	    #cluster{cid = Cid, ref_cnt = RC} = C,
	    ets:insert(IPDb, C#cluster{ref_cnt = RC + 1}),
	    ets:match_delete(IPDb, C),
	    {false, Cid}
    end.

make_set([]) -> [];
make_set([H|T]) -> [H | [ Y || Y <- make_set(T), Y =/= H]].
%%    
%% Delete an IP cluster if this was the last reference.
%% If the cluster should be deleted, all related IP addresses
%% also will be deleted.
%% (Every domain name with the same set of IP addresses (A records)
%% uses the same cluster).
%%    
%% A new Cid instance is always created before the old is
%% deleted in order to make sure that load_balance/1 always
%% has access to a Cid.
%%
try_delete_ip_cluster(Cid, IPDb) ->
    case ets:lookup(IPDb, Cid) of
	[C] when C#cluster.ref_cnt == 1 ->
	    F = fun(IP) ->
			ets:match_delete(IPDb, ?MATCH_IP(IP, Cid, '_', '_'))
		end,
	    lists:foreach(F, C#cluster.ips),
	    ets:delete(IPDb, C);
	[C] ->
	    RC = C#cluster.ref_cnt,
	    ets:insert(IPDb, C#cluster{ref_cnt = RC - 1}),
	    ets:match_delete(IPDb, C);
	_ ->
	    ok
    end.

%%
%% Get sorted IP's from cluster Cid.
%% First the LAN is selected (randomly related to LAN fractions)
%% and when a random sorted list of IP addresses is created related
%% to the current IP fraction list of that LAN.
%%
get_lb_ips(Cid, TTL, Domain, Type) ->
    case ets:lookup(dns_load_ip, Cid) of
	[#cluster{fractions = Fr}|_] when Fr /= [] ->
	    %% Can be several C:s as we insert new items before the
	    %% old is removed; i.e. we want let this function end
	    %% up without any cluster during updates in the server !!
	    delete_round_robin(Domain, Type),
	    {IPs, RestIPs} = select_lan(Fr),
	    RestIPs2 = lists:map(fun(X) -> X#ip_fr.ip end, RestIPs),
	    {ok, sort_ips(IPs) ++ RestIPs2, TTL};
	_ ->
	    %% No active IPs !!
	    {false, TTL}
    end.

get_pref_ips(OrigIP, CID, TTL, Domain, Type) ->
    %% 4 level case.. :-)
    case ets:lookup(ip_pref, Domain) of
	[] -> no_pref;
	PrefLanL ->
	    case get_pref_lan(PrefLanL, OrigIP) of
		{ok, PrefLan} ->
		    case ets:lookup(dns_load_ip, CID) of
			[#cluster{fractions = FrL}|_] when FrL /= [] ->
			    case select_pref_lan(PrefLan#pref_lan.lans, FrL) of
				{ok, {IPS, ResIPs}} ->
				    delete_round_robin(Domain, Type),
				    {ok, sort_ips(IPS) ++ ResIPs, TTL};
				X -> %% try to lb insead 
				    delete_round_robin(Domain, Type),
				    {IPs, RestIPs} = select_lan(FrL),
				    RestIPs2=lists:map(fun(Z) ->Z#ip_fr.ip end,
						       RestIPs),
				    {ok, sort_ips(IPs) ++ RestIPs2, TTL}
			    end;
			_ -> {false, TTL} %% No active IPs
		    end;
		_false -> no_pref
	    end
    end.

get_pref_lan(PL, OrigIP) -> get_pref_lan(PL, OrigIP, 1).
get_pref_lan(PrefLanL, OrigIP, N) ->
    case lists:keysearch(N, #pref_lan.no, PrefLanL) of
	{value, PrefLan} ->
	    case validate_ip(OrigIP, [PrefLan#pref_lan.o_netmask]) of
		{true, LAN} -> {ok, PrefLan};
		_false      -> get_pref_lan(PrefLanL, OrigIP, N+1)
	    end;
	false -> false
    end.

select_pref_lan(LANs, LFracL) ->
    N_C = length(LANs),
    RV = random(?TIMES_PREF),
    select_pref_lan(LANs, LFracL, N_C, N_C, -1, ?TIMES_PREF, hd(RV), tl(RV)).

select_pref_lan(_LANs, _LFracL, _N_C, _N_C_count, _J, 0, _R, _RV) -> congested;
select_pref_lan(LANs, LFracL, N_C, 0, J, TIMES_PREF, _R, RV) -> 
    select_pref_lan(LANs, LFracL, N_C, N_C, J, TIMES_PREF - 1, hd(RV), tl(RV));
select_pref_lan(LANs, LFracL, N_C, N_C_count, J, TIMES_PREF, R, RV) ->
    J1 = ((J+1) rem N_C),
    case lists:keysearch(element(1,lists:nth(J1+1, LANs)),#lan_fr.lan, LFracL) of
	{value, LANFr} ->
	    if R > (LANFr#lan_fr.congestion_level / ?MAX_CONGESTION_LEVEL) ->
		    F = fun(LAN, ACK) ->
				IPS = lists:map(fun(X) -> X#ip_fr.ip end,
						LAN#lan_fr.ips),
				IPS ++ ACK
			end,
		    ResIPs = lists:foldl(F, [], LFracL -- [LANFr]),
		    {ok, {LANFr#lan_fr.ips, ResIPs}};
	       true ->
		    select_pref_lan(LANs, LFracL, N_C,
				    N_C_count - 1, J1, TIMES_PREF -1, R, RV)
	    end;
	_Else ->
	    no_pref
    end.

%%
%% Select the Lan representing the Rand(om) fraction part (%).
%%
%% The last clause (select_lan/4) is there in order to cover for the
%% case Rand > N + F (with an empty list) which is in theory
%% impossible. But, the precision of the float may result
%% in this case. The choosen Lan is the correct one anyhow !
%%
select_lan([Lfr]) ->
    %% Only one LAN, i.e. we don't have to randomly choose.
    {Lfr#lan_fr.ips, []};
select_lan([]) ->
    %% No active LAN at all !2 + ((0 +1) rem 2).
    {[], []};
select_lan(Fr) ->
    [R] = sync_random(1),
    select_lan(Fr, 0, R, hd(Fr), []).

select_lan([Lfr|FRs], N, Rand, _, LanAck) when Rand =< N + Lfr#lan_fr.f ->
    F = fun(LAN, Ack) -> LAN#lan_fr.ips ++ Ack end,
    IPS = lists:foldl(F, [], LanAck ++ FRs), %% not cheap..improve later.
    {Lfr#lan_fr.ips, IPS};
select_lan([Lfr|FRs], N, Rand, _, LanAck) ->
    #lan_fr{f = F} = Lfr,
    select_lan(FRs, N+F, Rand, Lfr, [Lfr|LanAck]);
select_lan([], _, _, Lfr, LanAck) -> % can happen..!
    {Lfr#lan_fr.ips, LanAck}.    

%%
%% Sort a list of IP's using the fraction numbers related
%% to each IP and the generated random number.
%% The random number interval is decreased (each recursive call)
%% related to the fraction of choosen IP's.
%%
sort_ips([]) ->
    %% No active IP's !
    [];
sort_ips(IPs) ->
    [R] = random(1),
    sort(IPs, R, 1).

sort([#ip_fr{ip = IP}], _, _) ->
    [IP];
sort(IPs, Rand, Tot) ->
    {Part, IP, IPs1} = select(IPs, 0, Rand, []),
    Tot1 = Tot - Part,
    [IP|sort(IPs1, Rand * Tot1, Tot1)].

%%
%% Select the IP representing the Rand(om) part (%) of
%% the still existing cake.
%%
%% The last clause is there in order to cover for the
%% case Rand > N + P (with an empty list) which is in theory
%% impossible. But, the precision of the float may result
%% in this case. The choosen IP is the correct one anyhow !
%%
select([#ip_fr{ip = IP, f = P}|IPs], N, Rand, Ack) when Rand =< N + P ->
    {P, IP, Ack ++ IPs};
select([IPf|IPs], N, Rand, Ack) ->
    P = IPf#ip_fr.f,
    select(IPs, N + P, Rand, [IPf|Ack]);
select([], _, _, [#ip_fr{ip = IP, f = P}|Ack]) ->
    {P, IP, Ack}.

%%
%% Initiate the random value used to get IP from the cluster
%% fraction lists.
%%
init_random(IPDb, N) ->
    random:seed(),
    U = make_n_random(N), 
    ets:insert(IPDb, #random{u = U, ulen = N}).

new_random(IPDb,N) ->
    [OldR] = ets:lookup(IPDb, random),
    U      = OldR#random.u,
    ULen   = OldR#random.ulen,
    NewU   = make_n_random(N), %% [U1, U2..]
    ResU   = lists:nthtail(N, U),
    NewUL  = NewU ++ ResU,
    ets:insert(IPDb, #random{u = NewUL, ulen = ULen}),
    ets:match_delete(IPDb, OldR).


make_n_random(N) -> make_n_random(N,[]).
make_n_random(0, Ack) -> Ack;
make_n_random(N, Ack) -> make_n_random(N-1, [random:uniform() | Ack]).


tolower([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs])                       -> [C | tolower(Cs)];
tolower([])                           -> [].

%% pot minor bugg 
%% ip_up sets old IPr value on lan_f with new timestamp!!..  or..? /pekka
%%
%% decodes, the data from the load_server.
%%
get_p_opcode(P) -> hd(get_p_data(2,lists:nthtail(?OUR_DNS_HEADER_LEN, P))).
get_p_ip(P)     -> list_to_ip(get_p_data(3,lists:nthtail(?OUR_DNS_HEADER_LEN, P))).
get_p_host(P)   -> get_p_data(4,lists:nthtail(?OUR_DNS_HEADER_LEN, P)).
get_p_fe(P)     -> list_to_float(get_p_data(5,lists:nthtail(?OUR_DNS_HEADER_LEN, P))).
get_p_be(P)     -> list_to_float(get_p_data(6,lists:nthtail(?OUR_DNS_HEADER_LEN, P))).
get_p_sign(P)   -> get_p_data(7,lists:nthtail(?OUR_DNS_HEADER_LEN, P)).

get_p_data(N, P) -> get_p_data(N,P,1).
get_p_data(N,[Len|P],N) -> lists:sublist(P, Len);
get_p_data(N,[],N) -> [];
get_p_data(N, [Len|P], X) ->
    Rest = lists:nthtail(Len, P),
    get_p_data(N, Rest, X + 1).

list_to_ip([A,B,C,D]) -> {A,B,C,D};
list_to_ip([A,B,C,D,E,F,G,H]) -> {A,B,C,D,E,F,G,H}.    

unpack(P, Cookie) ->
    case lists:prefix(?OUR_DNS_HEADER, P) of
	true  -> unpack_d(lists:nthtail(?OUR_DNS_HEADER_LEN, P), Cookie, P);
	false -> false
    end.
	
unpack_d([], Cookie, P) -> [];
unpack_d([0 | DR], Cookie, P) -> unpack_d(DR, Cookie, P);
unpack_d([DataLen | DR], Cookie, P) ->
    Data = lists:sublist(DR, DataLen),
    if DR /= [] ->
	    R = (lists:nthtail(DataLen, DR)),
	    [Data | unpack_d(R, Cookie, P)];
       true -> []
    end.

%% get_all_but_sign(P) -> {PackWithoutSign, Sign}
%% this piece of weirdness gets the whole package except and the sign,
%% note that the first part of the sign is the lenght of it, must 
%% remove before comparing..!
get_all_but_sign(P)           -> get_all_but_sign_h(?OUR_DNS_HEADER_LEN, P,[]).
get_all_but_sign_h(0,P,A)       -> get_all_but_sign_r(6,P,A);
get_all_but_sign_h(X, [D|PR],A) -> get_all_but_sign_h(X-1, PR, [D|A]).

get_all_but_sign_r(0,[],A)                 -> {lists:reverse(A), []};
get_all_but_sign_r(0,Sign,A) -> {lists:reverse(A), Sign};
get_all_but_sign_r(X,[Len|Data],A)         -> get_all_but_sign_l(Len, Data, X, [Len|A]).
    
get_all_but_sign_l(0, D, X,A)     -> get_all_but_sign_r(X-1, D,A);
get_all_but_sign_l(S, [D|PR],X,A) -> get_all_but_sign_l(S-1, PR, X, [D|A]);
get_all_but_sign_l(S, [],X,A)     -> get_all_but_sign_l(S-1, [], X, A).

% get_all_but_sign(P)           -> get_all_but_sign_h(2, P).
% get_all_but_sign_h(0, P)      -> get_all_but_sign_r(5,P);
% get_all_but_sign_h(X, [D|PR]) -> [D|get_all_but_sign_h(X-1, PR)].

% get_all_but_sign_r(0,[])                 -> [];
% get_all_but_sign_r(0,[_LenOfSign|_Sign]) -> [];
% get_all_but_sign_r(X,[Len|Data])         -> [Len | get_all_but_sign_l(Len, Data, X)].
    
% get_all_but_sign_l(0, D, X)     -> get_all_but_sign_r(X-1, D);
% get_all_but_sign_l(S, [D|PR],X) -> [D | get_all_but_sign_l(S-1, PR, X)];
% get_all_but_sign_l(S, [],X)     -> get_all_but_sign_l(S-1, [], X).

open_udp(Port) -> gen_udp:open(Port, [list]).
