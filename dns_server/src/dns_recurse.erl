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

%%% File    : dns_recurse.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : The recursive evaluation part of a DNS query.
%%% Created :  1 Apr 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Modified:  1 Jun 1998 by Pekka Hedqvist <pekka@eddieware.org>
%%% Modified: 20 Aug 1998 by Pekka Hedqvist <pekka@eddieware.org>
%%% Modified: 13 Sep 1999 by Pekka Hedqvist <pekka@eddieware.org>
%%% Modified: 21 Feb 2000 by Pekka Hedqvist <pekka@eddieware.org>
%%% other stuff too
%%%
%%% Done: -Added dns_recurse_udp_tracker to better handle many  
%%%       recursive queries.
%%%       -Fixed bug when a ns query returned only names without 
%%%        any IP:s. then the process would not wait for these 
%%%        pending queries. Some resolvs then failt wich is really
%%%        bad.
%%%      -Fixed a number of character case related buggs, is now paranoid,
%%%       makes everything lowercase, case is not preserved. Make better
%%%       solutions later.
%%%       -Fixed CNAME recursive resolution. 
%%%
%%%
%%% --------------------------------------------------------------

-module(dns_recurse).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-vsn('$Revision: /main/eddie/eddie-1.0/7').
-author('pekka@eddieware.org').

-export([start_link/5, start_link/6, start_link/7, init/8]).
-include("dns.hrl").

-define(TIMEOUT, 5000). %% Default query timeout.
-define(SCNT,       3). %% Number of times to traverse SLIST.
-define(NO_RECURSE, 0). %% Recursion not desired.
-define(RECURSE,    1). %% Recursion desired (forwarded).

-record(state, {cnt,
		parent,
		peer_ip,
		prev_q,
		hints,
		question,
		domain,
		type,
		class,
		pend, %% pending address requests
		time,
	        timeout = ?TIMEOUT,
		udp,
		ids = [], %% list of sent ids to expect, for now just add
		ip,
		options,
		forwarders,
		scnt = ?SCNT,   %% Slist cnt
	        slist}).

-record(slist, {matchcnt,
		nslist = []}).

-record(ns, {domain,
	     ip = pending}).

-include_lib("../../misc/include/logger.hrl").
%-include_lib("misc/include/logger.hrl").

start_link(Cnt, Query, Opts, PeerIP, PrevQ) when record(Query, dns_query) ->
    proc_lib:start_link(?MODULE, init,
			[self(), Cnt, Query, false, times(), Opts, PeerIP, PrevQ]).

start_link(Cnt, Query, Opts, {NsL, ArL}, PeerIP, PrevQ) when record(Query, dns_query) ->
    Hints = {NsL, ArL},
    proc_lib:start_link(?MODULE, init,
			[self(), Cnt, Query, Hints, times(), Opts, PeerIP, PrevQ]).

start_link(Cnt, Query, Opts, Hints, Time, PeerIP, PrevQ)
  when record(Query, dns_query) ->
    proc_lib:start_link(?MODULE, init,
		[self(), Cnt, Query, Hints, Time, Opts, PeerIP, PrevQ]).

%%
%% We know that the query domain name does not exist in
%% any of our own zones at this stage.
%% Time is used in order to let us use cached data with
%% TTL = 0 during recursion.
%%
init(Parent, Cnt0, Query, Hints, Time, Options, PeerIP, PrevQ) ->
    process_flag(trap_exit, true),
    Query1 = Query#dns_query{domain = tolower(Query#dns_query.domain)},
    {ok, IPs} = dns_catalog:own_ip(),
    Domain = Query1#dns_query.domain,
    Forwarders = dns_catalog:forwarders(),
    NPrevQ = [#pend{domain = Domain, hints = Hints} | PrevQ],
    proc_lib:init_ack({ok, self()}),
    {Slist,Pend,Cnt} = create_slist(Domain,Hints,Time,Cnt0,IPs,Options,PeerIP,NPrevQ,[]),
%%    ?INFO(?F("~p init create_slist Parent: ~p~n Query: ~p Domain: ~p Hints: ~p Time: ~p Cnt0: ~p"
%%	     "IPs: ~p Options: ~p~n PrevQ: ~p~n returned ~p~n",
%%	     [self(), Parent,Query1, Domain, Hints, Time, Cnt0, IPs, Options, NPrevQ, {Slist, Pend, Cnt}])),
    %%    SNSs = lists:map(fun(X) -> X#ns{domain = tolower(X#ns.domain)} end,
    %%		     Slist#slist.nslist),
    S = #state{parent    = Parent,
	       prev_q    = NPrevQ,
	       hints     = Hints, 
	       peer_ip   = PeerIP,
	       cnt       = Cnt,
	       question  = Query1,
	       domain    = Domain,
	       type      = Query1#dns_query.type,
	       class     = Query1#dns_query.class,
	       pend      = Pend,
	       time      = Time,
               ip        = IPs,
	       options   = Options,
	       forwarders = Forwarders,
	       slist      = Slist},
    do_recurse(S).

do_recurse(State) ->
    case query_forwarders(State) of
	true         -> exit(shutdown);
	{false, Udp} -> slist_loop((State#state.slist)#slist.nslist,
				   State#state{udp = Udp})
    end.

%% {Pend1, Cnt1} = spawn_recursive(Domain, Pend, Options, Hints, Time, Cnt,PeerIP)
spawn_recursive(_, Pend, _, _, _, 0, _PeerIP, _PQ,_) -> {Pend, 0};
spawn_recursive(Domainl,Pend,Opts,Hints,Time,Cnt,PeerIP,PQL,Answ) ->
    %%    ?INFO(?F("~p spawn_recursive call Domainl ~p~n Pend: ~p~n Hints: ~p~n PeerIP: ~p~nPQL:~p~n", 
    %%	     [self(), Domainl, Pend, Hints, PeerIP,PQL])),
    %% only spawn for new recursive domain queries 
    Domain = tolower(Domainl),
    %% check that same query is not pending..
    case lists:any(fun(#pend{domain=Dom})-> tolower(Dom)==Domain end, Pend) of
	true ->
	    %%	    ?INFO(?F("~p spawn_recursive pending - stop Domain: ~p~n Pend: ~p~n"
	    %%		     "Hints: ~p~n PQ: ~p",
	    %%		     [self(), Domain, Pend, Hints, PQL])),
	    {Pend, Cnt};
	false ->
	    %% check that its not the same query from a dead spawn with the
	    %% same Hints
  	    PSpawn = is_same_query_again(PQL, Domain, Hints),
%	    ?INFO(?F("~p PSpawn ~p i_s_q_a PQL: ~p Domain: ~p Hints: ~p~n pend: ~p~n",
%		     [self(),PSpawn, PQL,Domain,Hints, Pend])),
	    if PSpawn == true ->
		    %%		    ?INFO(?F("~p spawn_recursive tried stop Domain: ~p~n Pend: ~p~n"
		    %%			     "Hints: ~p~n PQ: ~p",
		    %%			     [self(), Domain, Pend, Hints, PQL])),
		    {Pend, Cnt};
	       PSpawn == false -> 
		    Cnt1  = Cnt - 1,
		    Start = start_link(Cnt1, 
				       #dns_query{domain=Domain,class=in, type = ?S_A},
				       Opts,Hints,Time, PeerIP, PQL ++ Pend),
		    case Start of
			{ok, Pid} ->
			    {[#pend{pid = Pid, domain = Domain, hints = Hints, answ = Answ}
			      | Pend],
			     Cnt1};
			_ ->
			    {Pend, Cnt}
		    end
	    end
    end.

is_same_query_again([Pend|PQL], Domain, Hints) ->
    #pend{domain = Domu, hints = Hintspq} = Pend,
    Dom = tolower(Domu),
    case {Dom, Hintspq} of
	{Domain, Hints} -> true;
	{Domain,     _} -> false;
	_Else           -> is_same_query_again(PQL, Domain, Hints)
    end;
is_same_query_again([],D,H) ->     
    false.

%%%----------------------------------------------------------------------
%%% Query all forwarders recursively.
%%% Returns:
%%%    true         - if an answer has been found or the option forward-only
%%%                   is true (and no answer is found)
%%%    {false, Udp} - if no answer has been found
%%%----------------------------------------------------------------------
query_forwarders(State) ->
    #state{forwarders = Fs,
	   options = Options,
	   domain  = Domain,
	   class   = Class,
	   type    = Type,
	   udp     = Udp0} = State,
    case query_forwarders(Fs, Domain, Class, Type, Udp0, State) of
	true ->  true;
	_Else when ?FORWARD_ONLY(Options) ->
	    true;
	False ->
	    False
    end.

query_forwarders([IP|Fs], Domain, Class, Type, Udp0, State) ->
    IDS = State#state.ids,
    {ID, Udp} = send_rec_query(IP, Domain, Class, Type, Udp0),
    receive
	{udp, Udp, IP, _, Data} ->
	    case inet_dns:decode(binary_to_list(Data)) of
		{ok, Answ} ->
		    %% Do handle_resp/4 in order to cache answer.
		    %% The type of answer does not matter, just 
		    %% forward it back to the querier.
		    handle_resp(Answ, State, IP, State#state{ids = [ID|IDS]}),
%		    ?INFO(?F("~p query_forwarders sends ~p ~n to ~p~n",
%		     [self(), Answ, State#state.parent])),
		    State#state.parent ! {self(), Answ},
		    true;
		Error ->
		    ?ERROR(?F("Erroneous recursive response from ~p - ~p",
			      [IP, Error])),
		    true
	    end;
	{udp_closed, Udp} ->
	    NewUdp = udp_open(),
	    query_forwarders(Fs, Domain, Class, Type, NewUdp, State);
	{'EXIT', Pid, R} when Pid == State#state.parent ->
	    exit(shutdown);
	{error, Error} ->
	    ?ERROR(?F("Erroneous recursive response from ~p - ~p",
		      [IP, Error]))
    after ?TIMEOUT ->
	    query_forwarders(Fs, Domain, Class, Type, Udp, State)
    end;
query_forwarders([], _, _, _, Udp, _) ->
    {false, Udp}.

%%%----------------------------------------------------------------------
%%% The main loop. Traverse the SLIST, SCNT number of times.
%%% If a referal to NS(s) with a greater matchcount set the SLIST
%%% to these NS(s) and restart using the new SLIST.
%%%----------------------------------------------------------------------
slist_loop([], State) when State#state.scnt == 1, State#state.pend /= [] ->
    IDS = State#state.ids,
    PendingNS = [PNS#ns{domain = tolower(PNS#ns.domain)}
		 || PNS <- (State#state.slist)#slist.nslist],
    {Reason, S, NSs} = poll_pending(State, PendingNS, infinity), 
    case Reason of
	received ->
	    #state{domain = Domain, class = Class, type = Type, udp = Udp0} = S,
	    {NS, NSs1} = get_first_non_pending_ns(NSs),
%%	    NSsDomainIPL = lists:dropwhile(
%%			     fun(NS) when NS#ns.ip /= pending -> false;
%%				(_) -> true
%%			     end,
%%           		     NSs),
	    case NS of
		[] ->
%		    ?INFO(?F("~p No IP matching NSs ~p for domain ~p",
%			     [self(), NSs, Domain])),
		    exit(shutdown);
		_  ->
		    valid_ip_address(NS, Domain, Class, Type, "slist_loop[]"),
%		    ?INFO(?F("~p slist_loop[] before send_query: State~p~n NSs:~p~n",
%			     [self(), State, NSs])),
		    {ID, Udp} = send_query(NS, Domain, Class, Type, Udp0),
		    slist_receive_udp(NS, NSs1, Domain, S#state{udp = Udp, ids = [ID|IDS]})
	    end;
	timeouted ->  
	    %% ?INFO(?F("~p slist_loop poll_pending timeouted", [self()])),
	    exit(shutdown)
    end;
slist_loop([], State) when State#state.scnt == 1, State#state.pend == [] -> 
%    ?INFO(?F("~p slist_loop shutdown, scnt ==1 no pendings State: ~p", [self(), State])),
    exit(shutdown);
slist_loop([], State) ->
    Slist = State#state.slist,
    Scnt = State#state.scnt,
    slist_loop(Slist#slist.nslist,
	       State#state{scnt = Scnt - 1});
slist_loop([NS|NSs], State) when NS#ns.ip == pending ->  slist_loop(NSs, State);
slist_loop(NSs0, State0) ->
    {_TimeoutOrReceived, State01, [NS|NSs]} = poll_pending(State0, NSs0),
    #state{domain = Domain, class = Class, type = Type, udp = Udp0, ids = IDS} = State01,
    valid_ip_address(NS, Domain, Class, Type, "slist_loop NSs0"),
%    ?INFO(?F("~p slist_loop NSs: ~p~n before  send_query: State~p~n",[self(),NSs,State01])),
    {ID, Udp} = send_query(NS, Domain, Class, Type, Udp0),
    slist_receive_udp(NS, NSs, Domain, State01#state{udp = Udp, ids = [ID|IDS]}).
slist_receive_udp(NS, NSs, Domain, State) ->
    receive
	{udp, Udp, IP, Port, Data} ->
	    UAnsw = inet_dns:decode(binary_to_list(Data)),
%	    ?INFO(?F("~p slist_receive_udp NS: ~p~n NSs: ~p~n received UAnsw:~n~p~nState:~p~n",[self(),NS, NSs,UAnsw,State])),
	    case UAnsw of
		{ok, Answ} ->
		    HRESP = handle_resp(Answ, State, IP, State),
%		    ?INFO(?F("~p s_r_u handle_resp returned ~p~n",[self(), HRESP])),
		    case HRESP of
			{cached, State1}    -> slist_loop(NSs, State1);
			{new_slist, State1} -> slist_loop([], State1);
			{follow_cname, Cnt, Pend} ->
			    %% remember Answ as good, and this pid,
			    %% when this pid returns with a answ,
			    %% pick answ, pack them together and reply!
%%			    ?INFO(?F("~p follow_cname in slist_receive_udp"
%%				     "Cnt: ~p, Pend: ~p",[self(), Cnt, Pend])),
			    slist_loop(NSs, State#state{cnt = Cnt,
							pend = Pend});
			do_answer ->
%			    ?INFO(?F("~p s_r_u do_answer sends ~p ~n to ~p~n",
%				     [self(), Answ, State#state.parent])),
			    State#state.parent ! {self(), Answ},
			    exit(shutdown);
			{error, State1} ->
			    slist_loop(NSs, State1);
			XX ->
%			    ?ERROR(?F("~p Bad recursive response Answ:"
%				      "~p~nState: ~p~n XX: ~p~n",
%				      [self(), Answ, State, XX])),
			    %% Ignore bad response.
			    slist_loop(NSs, State)
		    end;
		Error ->
		    ?ERROR(?F("Badly formated data recursive response from ~p - ~p",
			      [IP, Error])),
		    slist_loop(NSs, State)
	    end;
	{udp_closed, Udp} ->
	    NewUdp = udp_open(),
	    slist_loop(NSs, State#state{udp = NewUdp});
	{'EXIT', Pid, R} when Pid == State#state.parent ->
	    exit(shutdown)
    after State#state.timeout ->
	    slist_loop(NSs, State)
    end.

%% TBD, if the timeout BIF is used this code can be in the 
%% receive statement above instead.
%% only pending NS left, collect them
poll_pending(State, NSs) -> poll_pending(State, NSs, 0).
poll_pending(State, NSs, T) ->
    Slist  = State#state.slist,
    SNSs   = Slist#slist.nslist,
    receive
	%% Answ already in the cache put there by child, handle it from there!
	{Pid, Answ} when pid(Pid) ->
%%	    if T == infinity -> 
%%		    ?INFO(?F("~p poll_pending received from ~p Answ: ~p~n"
%%			     "state: ~p~n NSs: ~p~n", 
%%			     [self(), Pid, Answ, State, NSs]));
%%		    true -> true
%%            end,
	    case is_cname_pend(Pid, State) of
		{false,P} ->
		    {SNSs1, NSs1} = new_ip_info(SNSs, NSs),
		    IP     = State#state.ip,
		    SNSs2  = sort_nslist(SNSs1, IP),
		    NSs2   = sort_nslist(NSs1,  IP),
		    State1 = State#state{slist = Slist#slist{nslist = SNSs2}},
		    {received, remove_pend(Pid, State1), NSs2};
		{true, P} ->
		    complete_cname_and_send(Answ, P, State),
		    {received, remove_pend(Pid, State), SNSs}
	    end;
	{'EXIT', Pid, R} when Pid == State#state.parent ->
	    exit(shutdown);
	{'EXIT', Pid, R} ->
	    {received, remove_pend(Pid, State), NSs}
    after T ->
%	    ?INFO(?F("~p poll_pending timeouted State: ~p~n~n NSs~p~n",
%                     [self(), State, NSs])),
	    {timeouted, State, NSs}
    end.

complete_cname_and_send(Answ, Pend, State) -> 
%   ?INFO(?F("~p complete_cname_and_send Answ: ~p~n"
%	     "Pend: ~p~n State: ~p~n", 
%	     [self(), Answ, Pend, State])),
    PAnsw = Pend#pend.answ,
    #dns_rec{qdlist = [PQuery], anlist = PAW,
	     nslist = PNS, arlist = PAR} = PAnsw,
    #dns_rec{qdlist = [Query], anlist = AW,
	     nslist = NS, arlist = AR} = Answ,
    QDomain  = Query#dns_query.domain,
    PQDomain = PQuery#dns_query.domain,
    %% 
    %%
    [FillInRR|_FF] = [CName || CName <- AW,
			       Query#dns_query.type == CName#dns_rr.type,
			       CName#dns_rr.domain  == QDomain,
			       ?IS_IP(CName#dns_rr.data)],
%    ?INFO(?F("~p fillin  ~p~n", [self(), FillInRR])),
    WithCnamePAnsw = PAnsw#dns_rec{anlist = [FillInRR| PAW]},
%    ?INFO(?F("~p complete_cname_and_send to ~p~n Answ: ~p~n State: ~p~n", 
%	     [self(), State#state.parent, Answ, State])),
    State#state.parent ! {self(), WithCnamePAnsw},
    exit(shutdown).
    
    
%% {SNSs1, NSs1} = new_ip_info(SlistNSs, CurrentNSs)
%% As a pending request has returned the addresses of name servers
%% can be filled in. Both the current SLIST and the slist_loop/2
%% traversing list will be updated.
new_ip_info(SNSs, NSs) -> new_ip_info(SNSs, NSs, []).
new_ip_info([SNS|SNSs], NSs, Ack) when SNS#ns.ip == pending ->
    Domain = tolower(SNS#ns.domain),
    case dns_cache:lookup(Domain, in, ?S_A) of
	{ok, RRs} ->
	    ENSs = ns_address(RRs, Domain),
	    NSs1 = new_current_list(NSs, ENSs, Domain),
	    new_ip_info(SNSs, NSs1, ENSs ++ Ack);
	_ ->
	    new_ip_info(SNSs, NSs, [SNS|Ack])
    end;
new_ip_info([SNS|SNSs], NSs, Ack) ->
    new_ip_info(SNSs, NSs, [SNS|Ack]);
new_ip_info([], NSs, Ack) ->
    {lists:reverse(Ack), NSs}.

%%
new_current_list([#ns{domain = Domain, ip = pending}|NSs], ENSs, Domain) ->
    ENSs ++ NSs;
new_current_list([NS|NSs], ENSs, Domain) ->
    [NS|new_current_list(NSs, ENSs, Domain)];
new_current_list([], _, _) ->
    [].

% new_current_list(NSs, ENSs, Domain) ->
%     ENSs ++ lists:filter(fun(NS) when NS#ns.domain == Domain,
% 				      NS#ns.ip == pending ->
% 				 false;
% 			    (_) -> true
% 			 end,
% 			 NSs).

remove_pend(Pid, State) ->
    Pend0 = State#state.pend,
    case lists:keysearch(Pid, #pend.pid, Pend0) of
	{value, P} ->
	    Pend = lists:keydelete(Pid, #pend.pid, Pend0),
	    PQ = State#state.prev_q,
	    State#state{pend = Pend, prev_q = [P|PQ]};
	false ->
	    State
    end.

%% is this query a cname query which we now got an answer from?
is_cname_pend(Pid, State) ->
    Pend0 = State#state.pend,
    %% we like to have an error if not existing Pid,
    %% then something strange has happened
    case lists:keysearch(Pid, #pend.pid, Pend0) of
	{value, P} when P#pend.answ /= [] -> {true, P}; %% only cname spawns has
	{value, P}                        -> {false, P} %% old answ to fill in
    end.
	    
%%%----------------------------------------------------------------------
%%% Handle a response from another DNS server.
%%%----------------------------------------------------------------------

handle_resp(Answ, State, IP, S) when record(S, state) ->
    IDS = S#state.ids,
    Header = Answ#dns_rec.header,
    Resp = case lists:member(Header#dns_header.id, IDS) of
	       true -> resp_p(Header, Header#dns_header.id,
			      Answ#dns_rec.qdlist, State#state.question);
	       false -> false
	   end,
    %% Resp = resp_p(Header, ID, Answ#dns_rec.qdlist, State#state.question),
    case Resp of
	{aa, Rcode} ->
	    cache_rrs(true, Header#dns_header.tc, Answ, Rcode),
	    case follow_if_only_cname(Rcode, Answ, S) of
		{follow_cname, Cnt, Pend} -> {follow_cname, Cnt, Pend};
		false ->
		    do_answer
	    end;
	{noaa, Rcode} ->
	    cache_rrs(false, Header#dns_header.tc, Answ, Rcode),
	    NSREF = ns_referal(Rcode, Answ, S),
%	    ?INFO(?F("NS Referal returned ~p~n", [NSREF])),
	    case NSREF of 
		{ok, State1} ->   {new_slist, State1};
		false        ->   do_answer;
		no_answer ->
		    %% This name server has not a current copy 
		    %% of its zone, delete it from SLIST.
		    State1 = delete_ns(IP, State),
		    {cached, State1};
		{follow_cname, Cnt, Pend} ->
		    {follow_cname, Cnt, Pend}
	    end;
	{error, Rcode} ->
	    %% really use IP as id when removing?
	    State1 = delete_ns(IP, State),
	    {error, State1};
	_ ->
	    false
    end.

%%  Resp = resp_p(Header, ID, Answ#dns_rec.qdlist, State#state.question),
resp_p(#dns_header{qr = 1, aa = 1, rcode = Rcode, id = ID}, ID, [Q], Q)
  when Rcode == ?NOERROR ->
    {aa, Rcode};
resp_p(#dns_header{qr = 1, aa = 1, rcode = Rcode, id = ID}, ID, [Q], Q)
  when Rcode == ?NXDOMAIN ->
    {aa, Rcode};
resp_p(#dns_header{qr = 1, aa = 1, rcode = Rcode, id = ID}, ID, [Q], Q) ->
    {error, Rcode};
resp_p(#dns_header{qr = 1, aa = 0, rcode = Rcode, id = ID}, ID, [Q], Q)
  when Rcode == ?NOERROR ->
    {noaa, Rcode};
resp_p(#dns_header{qr = 1, aa = 0, rcode = Rcode, id = ID}, ID, [Q], Q)
  when Rcode == ?NXDOMAIN ->
    {noaa, Rcode};
resp_p(#dns_header{qr = 1, aa = 0, rcode = Rcode, id = ID}, ID, [Q], Q) ->
    {error, Rcode};
resp_p(_, _, _, _) ->
    false.

%%
%% Cache all RR's if the answer wasn't truncated.
%% TBD, we should also cache negative responses !
%% SOA RR's are never cached, only used to cache negative responses
%% with an appropriate TTL.
cache_rrs(_, 1, _, _) ->   false;
cache_rrs(Auth, _, #dns_rec{qdlist = [Query],
			    anlist = AN,
			    nslist = NS,
			    arlist = AR},
	  ?NOERROR) ->
    #dns_query{domain = Domain, class = Class, type = Type} = Query,
    case valid_anlist(AN, Domain, Class, Type) of
	true -> dns_cache:insert(Auth, AN ++ NS ++ AR);
	_    -> false
    end;
cache_rrs(_, _, _, _) ->  false.

%% valid_anlist(AN, Domain, Class, Type) of
valid_anlist(AN, D, C, T) ->
    case splitlist(AN, D, C, T) of
	{AN, []} -> true;
	{[], AN} ->
	    case splitlist(AN, D, C, ?S_CNAME) of
		{[], AN} -> false;
		{CN, Any} ->
		    CNAME = (hd(CN))#dns_rr.data,
		    case splitlist(Any, CNAME, C, T) of
			{Any, []} -> true;
			_         -> false
		    end
	    end;
	_ ->
	    false
    end.

%% splitlist(L, Domain, Class, Type) -> {EQL, NOTEQL}
%% 
%% Split the list and let EQL contain RRs which contains
%% Domain, Class AND Type and NOTEQL all others.
%% Note: Keeps the list ordering.
splitlist(L, Domain, Class, Type) ->
    splitlist(L, Domain, Class, Type, [], []).

splitlist([RR|RRs], Domain, Class, Type, Eq, Neq)
  when RR#dns_rr.domain == Domain,
       RR#dns_rr.class == Class,
       RR#dns_rr.type == Type ->
    splitlist(RRs, Domain, Class, Type, [RR|Eq], Neq);
splitlist([RR|RRs], Domain, Class, Type, Eq, Neq) ->
    splitlist(RRs, Domain, Class, Type, Eq, [RR|Neq]);
splitlist([], _, _, _, Eq, Neq) -> {lists:reverse(Eq), lists:reverse(Neq)}.

%% 
%% If anlist is empty there may be information about
%% other name servers closer to the query domain name.
ns_referal(?NOERROR, Answ, S) when record(S,state) ->
    #dns_rec{qdlist = [Query],
	     anlist = AW,
	     nslist = NS,
	     arlist = AR} = Answ,
    QDomain = Query#dns_query.domain,
    PQ = S#state.prev_q,
    case dns_query:matching_rrs(AW, Query#dns_query.type) of
	[] ->
	    case dns_query:matching_rrs(AW, ?S_CNAME) of
		[] ->
		    case dns_query:matching_rrs(NS, ?S_NS) of
			[] -> no_answer;
			NSRRs ->
			    X = new_slist(NSRRs, AR, S),
			    X
		    end;
		CNs ->
		    case [CRR || CRR <- CNs, ?IS_IP(CRR#dns_rr.data)] of
			[] ->
			    CN = hd(CNs),
			    QD = CN#dns_rr.data,
			    #state{pend = Pend, hints = Hints, options = Opts,
				   time = Time,cnt = Cnt, peer_ip = PeerIP} = S,
%			    ?INFO(?F("~p ns_ref s_r pend: ~p pq: ~p~n",[self(), Pend,PQ])),
			    {Pend1, Cnt1} = spawn_recursive(QD, Pend, Opts,{NS,AR},
							    Time, Cnt, PeerIP, PQ, Answ),
%			    ?INFO(?F("~p ns_referal follow_cname ~nQuery: ~p~n"
%				     "Anlist: ~p~n"
%				     "NSlist: ~p~n ARList: ~p~n"
%				     "CNs: ~p~n",
%				     [self(), Query, AW, NS, AR, CNs])),   
			    {follow_cname, Cnt1, Pend1};
			_Else ->
%			    ?INFO(?F("~p ns_referal do_answer CNs has data ~p",[self(),CNs])),
			    do_answer
		    end
	    end;
	_AWQs ->
	    %% Same type is in ANList but is IP and Domain in there?
	    case [RR || RR <- AW, ?IS_IP(RR#dns_rr.data), RR#dns_rr.domain == QDomain] of
		[] -> %% no, any CNAMEs with right Domain?
		    case [RR || RR <- AW, RR#dns_rr.type == ?S_CNAME] of
			[]    -> no_answer; %% no cname either
			[CnameRR|_] -> %% yes, any 
			    QD = CnameRR#dns_rr.data, 
			    case [RR || RR <- AW,
					RR#dns_rr.domain == QD, 
					?IS_IP(RR#dns_rr.data)] of 
				[] -> %% no answer at all, but a cname, follow it!
				    #state{pend = Pend, hints = Hints, options = Opts, 
					   time = Time, cnt = Cnt, peer_ip = PeerIP} = S,
%				    ?INFO(?F("~p ns_ref2 s_r pend: ~p pq: ~p~n",[self(), Pend, PQ])),
				    {Pend1, Cnt1} = spawn_recursive(QD, Pend, Opts, {NS,AR}, Time,
								    Cnt, PeerIP, PQ, Answ),
				    {follow_cname, Cnt1, Pend1};
				Else -> % we have a cname and its IP, answer!
				    false
			    end
		    end;
		Elsed ->
		    %% ?INFO(?F("~p ns_referal return false, ~p~n",[self(),Elsed])),
		    %% yes the answer is there.
		    false  
	    end
    end.

follow_if_only_cname(?NOERROR, Answ, S) when record(S, state) ->
%    ?INFO(?F("~p follow_if_only_cname Answ: ~p~n State: ~p~n",
%	     [self(),Answ, S])),
    #dns_rec{qdlist = [Query],  anlist = AW,
	     nslist = NS, arlist = AR} = Answ,
    PQ = S#state.prev_q,
    AWT = dns_query:matching_rrs(AW, Query#dns_query.type),
    AWC = dns_query:matching_rrs(AW, ?S_CNAME),
    NSs = dns_query:matching_rrs(NS, ?S_NS),
    case AWT of
	[] when AWC == [] -> false;
	[] when AWC /= [] ->
	    case [CRR || CRR <- AWC, ?IS_IP(CRR#dns_rr.data)] of
		[] -> 
		    QD = (hd(AWC))#dns_rr.domain,
		    #state{pend = Pend, hints = Hints, options = Opts,
			   time = Time,cnt = Cnt, peer_ip = PeerIP} = S,
%		    ?INFO(?F("~p f_i_o_cn s_r pend: ~p pq: ~p~n",[self(), Pend, PQ])),
		    {Pend1, Cnt1} = spawn_recursive(QD, Pend, Opts, {NS,AR},
						    Time, Cnt, PeerIP, PQ, Answ),
		    {follow_cname, Cnt1, Pend1};
		_Else -> false
	    end;
	_ -> false
    end;
follow_if_only_cname(_, _Answ, S) when record(S, state) -> false.

%% Create a new SLIST if better match count value.
new_slist(NSRRs, AR, State) ->
    Zone = (hd(NSRRs))#dns_rr.domain,
    MatchCnt = matchcnt(Zone),
    #state{slist = Slist, options = Opts} = State,
    if
	MatchCnt > Slist#slist.matchcnt ->
	    #state{time = Time, cnt = Cnt, pend = Pend, peer_ip = PeerIP, ip = IP} = State,
	    {Slist1, Pend1, Cnt1} = new_slist(NSRRs, AR, MatchCnt,Time,Cnt,IP, Opts,PeerIP,
					      State#state.prev_q, Pend),
	    {ok, State#state{pend = Pend1, cnt = Cnt1, scnt = ?SCNT, slist = Slist1}};
	MatchCnt == Slist#slist.matchcnt ->
	    %% not better but maybe the answer has the answer of queries that
	    %% are pending now
%	    ?INFO(?F("~p is_answ_of_pends NSRRS: ~p~n AR: ~p~n State: ~p~n",
%		     [self, NSRRs, AR, State])),
	    case is_answ_of_pends(NSRRs, AR, State) of
		no_answer ->
%		    ?INFO(?F("~p is_answ_of_pends no_answer ~n",[self()])),
		    no_answer;
		NSlist    ->
%		    ?INFO(?F("~p is_answ_of_pends returned: ~p~n", [self(), NSlist])),
		    {ok, State#state{scnt = State#state.scnt + 1,
					      slist = Slist#slist.nslist}}
	    end;
	true ->	no_answer  %% false /pekka
    end.

new_slist(NSRRs, AR, MatchCnt, Time, Cnt, IP, Opts, PeerIP,PQ,Pend) ->
    {NSs,Pend1,Cnt1} = get_ns_addresses(NSRRs,AR,Time,Cnt,IP,Opts,PeerIP, PQ,Pend),
    NSs1  = sort_nslist(NSs, IP),
    Slist = #slist{matchcnt = MatchCnt, nslist = NSs1},
    {Slist, Pend1, Cnt1}.

%%
%% Same scount ns:s, but is these NSs we received the same as the 
%% ones as are pending? If so, fill in the ip:s in the pending ns:s.
%% donot remove pending from pend as for now..

is_answ_of_pends(NSRRs, AR, State) ->
    NSwithAR = [NS || NS <- NSRRs, ARs <- AR, NS#dns_rr.data == ARs#dns_rr.domain],
    KillPends = [Pends || Pends <- State#state.pend, NS <- NSwithAR,
                          Pends#pend.domain == NS#dns_rr.data],
    case KillPends of
        [] -> no_answer;
        _  ->
            SList = State#state.slist,
            SNSs = SList#slist.nslist,
	    ARwithNS = [ARs || NS <- NSRRs, ARs <- AR, NS#dns_rr.data == ARs#dns_rr.domain],
            NSNSs = replace_pend_ns(SNSs, ARwithNS),
            SList#slist{nslist = NSNSs}
    end.

replace_pend_ns(NSs, ARwithNS) -> replace_pend_ns(NSs, ARwithNS, []).
replace_pend_ns([NS|NSs], ARwithNS, Ack) when NS#ns.ip == pending ->
    case lists:keysearch(NS#ns.domain, #ns.domain, ARwithNS) of
        {value, AR} ->
            replace_pend_ns(NSs, ARwithNS, [NS#ns{ip = AR#dns_rr.data} | Ack]);
        false ->
            replace_pend_ns(NSs, ARwithNS, [NS|Ack])
    end;
replace_pend_ns([NS|NSs], ARwithNS, Ack) ->
            replace_pend_ns(NSs, ARwithNS, [NS|Ack]);
replace_pend_ns([], ARwithNS, Ack) -> lists:reverse(Ack).

delete_ns(IP, State) ->
    Slist = State#state.slist,
    NSs   = lists:keydelete(IP, #ns.ip, Slist#slist.nslist),
    State#state{slist = Slist#slist{nslist = NSs}}.

%%%----------------------------------------------------------------------
%%% Misc. functions
%%%----------------------------------------------------------------------
times() -> {Mega,Secs,_} = erlang:now(), Mega*1000000 + Secs.

%%
%% {Slist, Pend, Cnt'} = create_slist(Domain, Hints, Time, Cnt,
%%                                    OwnIPs, Options,PeerIP, PrevQ,Pend)
%% Create the SLIST
%%
create_slist(_,_,_,Cnt, IPs, Opts, _, PrevQ,Pend) when ?FORWARD_ONLY(Opts) ->
    {#slist{matchcnt = -1, nslist = []}, Pend, Cnt};
create_slist("", _, Time, Cnt, IPs, Opts, PeerIP, PrevQ, Pend) ->
    case dns_cache:lookup(".") of
	{ok, NSRRs} ->
	    {NSs, Pend1, Cnt1} =
		get_ns_addresses(NSRRs,[],Time,Cnt,IPs,Opts,PeerIP,PrevQ,Pend),
	    NSs1 = sort_nslist(NSs, IPs),
	    Slist = #slist{matchcnt = 0, nslist = NSs1},
	    {Slist, Pend1, Cnt1};
	_ ->
	    case dns_catalog:root_hints(".") of 
		{ok, NSRRs, AddrRRs} ->
		    {NSs, Pend1, Cnt1} = root_hint(NSRRs, AddrRRs, Time, Cnt,
						  IPs, Opts, PeerIP, PrevQ, Pend),
		    NSs1 = sort_nslist(NSs, IPs),
		    Slist = #slist{matchcnt = -1,
				   nslist = NSs1},
		    {Slist, Pend ++ Pend1, Cnt1};
		{error, nozone} ->
		    {#slist{matchcnt = -1, nslist = []}, Pend, Cnt}
	    end
    end;
create_slist(Domain, Hints, Time, Cnt,_,_, _PeerIP,_PQ, Pend) when record(Hints,slist) ->
    {Hints, Pend, Cnt};
create_slist(Domain,{NSRRs,ARRRs},Time,Cnt,IPs,Opts,PeerIP,PQ,Pend) when NSRRs/=[]->
    %% Use initial info as SLIST, e.g. due to a cut in a local zone.
    Zone = (hd(NSRRs))#dns_rr.domain,
    MatchCnt = matchcnt(Zone),
    new_slist(NSRRs, ARRRs, MatchCnt, Time, Cnt, IPs, Opts, PeerIP,PQ,Pend);
create_slist(Domain, _, Time, Cnt, IPs, Opts, PeerIP, PQ, Pend) ->
    case dns_cache:lookup(Domain, in, ?S_NS, Time) of
	{ok, NSRRs} ->
	    {NSs, Pend1, Cnt1} = get_ns_addresses(NSRRs, [], Time, Cnt, IPs, Opts, PeerIP, 
						  PQ, Pend),
	    case NSs of
		[] -> %% no spawns
%		    ?INFO(?F("~p create slist no spawns Domain ~p~n  PQ: ~p~n Pend ~p~n"
%			     "Pend1 ~p~n NSs: ~p~n",
%			     [self(), Domain, PQ,Pend, Pend1,NSs])),
		    create_slist(strip_label(Domain),false,Time,Cnt1,IPs,Opts,PeerIP,PQ,
				 Pend1);
		_Else ->
		    NSs1 = sort_nslist(NSs, IPs),
		    Slist = #slist{matchcnt = matchcnt(Domain),  nslist = NSs1},
%		    ?INFO(?F("~p create slist spawns Domain ~p~n PQ: ~p~n Pend ~p~n"
%			     "Pend1: ~p~n NSs: ~p~n",
%			     [self(), Domain, PQ,Pend, Pend1,NSs1])),
		    {Slist, Pend ++ Pend1, Cnt1}
	    end;
	_ ->
	    create_slist(strip_label(Domain),false,Time,Cnt,IPs,Opts,PeerIP,PQ,Pend)
    end.

strip_label([$.|Domain]) -> Domain;
strip_label([_|T])       -> strip_label(T);
strip_label([])          -> [].
        
%% {NSs,Pend',Cnt1} = get_ns_addresses(NSRRs,Addrs,Time,Cnt,OwnIPs,Opts,PeerIP,PQ)
get_ns_addresses(NSRRs, Addrs, Time, Cnt, IPs, Opts, PeerIP, PrevQ, Pend) ->
    get_ns_addresses(NSRRs, Addrs, Pend, [], Time, Cnt, IPs, Opts, PeerIP, PrevQ).

%% NSRRs is all of type ?S_NS, i.e. domain name is in data !
get_ns_addresses([#dns_rr{data = Domainu}|NSRRs], Addrs, Pend, NSs, Time, Cnt, IPs, Opts,
		 PeerIP, PrevQ) ->
    Domain = tolower(Domainu),
    case ns_address(Addrs, Domain) of
	[] ->
	    case local_lookup(Domain,in,?S_A,Time,Cnt,IPs,Opts,PeerIP,PrevQ,Pend) of
		{ok, RRs} ->
%%		    ?INFO(?F("~p get_ns_addresses local_lookup ok RRs: ~p Domain: ~p",
%%			     [self(), RRs, Domain])),		    
		    F = fun(#dns_rr{data = IP}) -> #ns{domain = Domain, ip = IP} end,
		    NewNSs = lists:map(F, RRs),
		    get_ns_addresses(NSRRs, Addrs, Pend, NSs ++ NewNSs,
				     Time, Cnt, IPs, Opts, PeerIP,PrevQ);
		{spawned, Pend1, Cnt1} ->
%%		    ?INFO(?F("~p get_ns_addresses local_lookup spawned Pend: ~p Domain: ~p",
%%			     [self(), Pend1, Domain])),		    
		    NewNSs = if Cnt1 == Cnt -> NSs; %% no spawn
				       true -> NSs ++ [#ns{domain = Domain}] %% spawns
			     end,
		    get_ns_addresses(NSRRs, Addrs, Pend1, NewNSs,Time,Cnt1, IPs, Opts,
				     PeerIP, PrevQ);
		false -> 
%%		    ?INFO(?F("~p get_ns_addresses local_lookup false Domain: ~p",
%%			     [self(), Domain])),		    
%		    ?INFO(?F("~p g_ns_add s_r pend: ~p pq:~p ~n",[self(), Pend, PrevQ])),
		    {Pend1, Cnt1} = spawn_recursive(Domain, Pend, Opts, false,
						    Time,Cnt,PeerIP,PrevQ,[]),
		    NewNSs = if Cnt1 == Cnt -> NSs; %% no spawn
				true -> NSs ++ [#ns{domain = Domain}] %% spawns
			     end,
		    get_ns_addresses(NSRRs, Addrs, Pend1, NewNSs,
				     Time, Cnt1, IPs, Opts, PeerIP, PrevQ)
	    end;
	NewNSs ->
	    get_ns_addresses(NSRRs, Addrs, Pend, NSs ++ NewNSs, Time, Cnt, IPs, Opts, 
			     PeerIP, PrevQ)
    end;

get_ns_addresses([], _, Pend, NSs, _, Cnt, _, _,_PeerIP, _PrevQ) ->
    NSs2 = lists:map(fun(X) -> X#ns{domain = tolower(X#ns.domain)} end, NSs),
    {NSs2, Pend, Cnt}.

%% extract IP from #dns_rr.data and create #ns.ip if they has IPs
ns_address([#dns_rr{domain = Domain, data = IP}|Addrs], Domain) ->
    case ?IS_IP(IP) of
	true  -> [#ns{domain = Domain, ip = IP}|ns_address(Addrs, Domain)];
	false -> ns_address(Addrs, Domain)
    end;

ns_address([#dns_rr{domain = NSDomain, data = IP}|Addrs], Domain) ->
    case ?IS_IP(IP) of
	true ->  case tolower(NSDomain) of
		     Domain ->
			 [#ns{domain = NSDomain, ip = IP}|ns_address(Addrs, Domain)];
		     _Else ->
			 ns_address(Addrs, Domain)
		 end;
	false -> ns_address(Addrs, Domain)
    end;

ns_address([_|Addrs], Domain) ->  ns_address(Addrs, Domain);
ns_address([], _)             ->  [].

local_lookup(Domain, Class, Type, Time, Cnt, IPs, Opts, PeerIP, PrevQ, Pend) ->
    case dns_query:find_zone(Domain) of
	{ok, Zone} ->
	    case dns_query:do_match_zone(#dns_query{domain=Domain,class=Class,type=Type},
					 Zone) of
		{match, [RR]} when RR#dns_rr.type == ?S_CNAME ->
%		    ?INFO(?F("~p local_lookup CNAME"
%			     "Domain: ~p~nClass:~p~nType:~p~n",
%			     [self(), Domain, Class, Type])),		    
		    local_lookup(RR#dns_rr.domain, Class, Type, Time, Cnt, IPs, Opts,
				 PeerIP, PrevQ, Pend);
		{match, RRs} ->
		    case dns_query:matching_rrs(RRs, Type) of
			[]   -> false;
			MRRs -> {ok, MRRs}
		    end;
		{cut, Zone, _, NSRRs} ->
		    %% This is not part of our authoritative data.
		    Addrs = dns_query:get_additional_addrs(NSRRs, Zone,PeerIP),
		    {NSs, Pend0, Cnt1} = get_ns_addresses(NSRRs, Addrs, Time, Cnt, IPs, 
							  Opts,PeerIP, PrevQ, Pend),
		    NSs1 = sort_nslist(NSs, IPs),
		    Slist = #slist{matchcnt = matchcnt(Zone),  nslist = NSs1},
		    %% ?INFO(?F("~p spawn recursive due to a cut",[self()])),   
%		    ?INFO(?F("~p l_l cut s_r pend: ~p pq: ~p~n",[self(), Pend, PrevQ])),
		    %% the last arg is empty since no prev answ to fill in
		    {Pend2, Cnt2} = spawn_recursive(Zone, Pend0, Opts, Slist, Time, Cnt1,
						    PeerIP, PrevQ, []),
		    {spawned, Pend2, Cnt2};
		{wildcard, RRs} ->
		    MRRs = dns_query:matching_rrs(RRs, Type),
		    {ok, MRRs};
		false -> 
		    false
	    end;
	_ ->
	    case dns_cache:lookup(Domain, Class, Type, Time) of
		{ok, RRs} -> {ok, RRs};
		_         -> false
	    end
    end.

root_hint(NSRRs,Addrs,Time,Cnt,OwnIPs,Opts,PeerIP,PQ,Pend) ->
    root_hint(NSRRs,Addrs,Time,Cnt,[],Pend,OwnIPs,Opts,PeerIP,PQ).

root_hint([NSRR|NSRRs], Addrs,Time,Cnt,ANSs,APend, IPs, Opts, PeerIP, PQ) ->
    Domain = NSRR#dns_rr.data,
    case lists:keysearch(Domain, #dns_rr.domain, Addrs) of
	{value, ArRR} ->
	    NS = #ns{domain = Domain,
		     ip = ArRR#dns_rr.data},
	    root_hint(NSRRs,Addrs,Time,Cnt,[NS|ANSs],APend,IPs,Opts,PeerIP,PQ);
	_ ->
	    {NSs, Pend1, Cnt1} = get_ns_addresses([NSRR], [], Time,
						 Cnt, IPs, Opts, PeerIP,PQ,APend),
	    root_hint(NSRRs, Addrs, Time, Cnt1, NSs  ++ ANSs, Pend1,
		      IPs, Opts, PeerIP, PQ)
    end;
root_hint([], _, _, Cnt, ANSs, AP, _, _, _PeerIP,PQ) ->
    {lists:reverse(ANSs), AP, Cnt}.

%%
%% Sort NS list in order to get the 'best' server first.
%% Only sorted according to network address at the moment.
%% TBD, use response time information gathered earlier !
%%
sort_nslist(NSs, OwnIPs) ->  sort_nslist(NSs, OwnIPs, 4).

sort_nslist(NSs, _, 0) ->  NSs;
sort_nslist(NSs, IPs, N) ->
    {MNSs, NSs1} = split_nslist(NSs, IPs, N, [], []),
    MNSs ++ sort_nslist(NSs1, IPs, N - 1).

split_nslist([NS|NSs], IPs, N, MNSs, ANSs) ->
    case match_ip_p(NS, IPs, N) of
	true -> split_nslist(NSs, IPs, N, [NS|MNSs], ANSs);
	_    -> split_nslist(NSs, IPs, N, MNSs, [NS|ANSs])
    end;
split_nslist([], _, _, MNSs, ANSs) ->
    {lists:reverse(MNSs), lists:reverse(ANSs)}.

match_ip_p(NS, [IP|IPs], N) ->
    case match_ip_p1(NS, IP, N) of
	true ->
	    true;
	_ ->
	    match_ip_p1(NS, IPs, N)
    end;
match_ip_p(NS, [], N) ->
    false.

match_ip_p1(#ns{ip = IP}, IP, 4)                           -> true;
match_ip_p1(#ns{ip = {IP1,IP2,IP3,_}}, {IP1,IP2,IP3,_}, 3) -> true;
match_ip_p1(#ns{ip = {IP1,IP2,_,_}}, {IP1,IP2,_,_}, 2)     -> true;
match_ip_p1(#ns{ip = {IP1,_,_,_}}, {IP1,_,_,_}, 1)         -> true;
match_ip_p1(_, _, _)                                       -> false.

%% Number of labels in domain.
matchcnt(Domain) ->
    string:words(Domain, $.).

%%
%% Send an iterative request to a nameserver.
%%
send_query(NS, Domain, Class, Type, Udp) ->
    valid_ip_address(NS, Domain, Class, Type, Udp),
    #ns{ip = IP} = NS,
    {ok, ID} = dns_catalog:next_id(),
    case mk_query(Domain, Class, Type, ID, ?NO_RECURSE) of
	{ok, Buffer} ->
%	    ?INFO(?F("~p send_query~nNS: ~p~nDomain:~p~nClass: ~p~n"
%		     ", Type:~p~n ID: ~p~n",
%		     [self(), NS, Domain, Class, Type,ID])),
	    Udp1 = udp_send(Udp, IP, Buffer),
	    {ID, Udp1};
	_ ->
	    ?ERROR(?F("Couldn't encode not recursive question: ~p",
		     [{Domain, Class, Type}])),
	    {ID, Udp}
    end.


valid_ip_address(NS, Domain, Class, Type, Txt) ->
    #ns{ip = IP} = NS,
    case ?IS_IP(IP) of
	true  -> true;
	false ->
%	    ?INFO(?F("At ~p~nShutdown query, tried to send a query to a NS ~p "
%		     "not an IP. Domain: ~p, Class: ~p: Type: ~p",
%		      [Txt, NS, Domain, Class, Type])),
	    exit(shutdown)
    end.

%%
%% Send a recursive request to a forwarded nameserver.
%%
send_rec_query(IP, Domain, Class, Type, Udp) ->
%    ?INFO(?F("~p send_rec_query Domain: ~p~n",
%	     [self(), Domain])),
    {ok, ID} = dns_catalog:next_id(),
    case mk_query(Domain, Class, Type, ID, ?RECURSE) of
	{ok, Buffer} ->
	    valid_ip_address(#ns{ip =IP}, Domain, Class, Type, Udp),
	    Udp1 = udp_send(Udp, IP, Buffer),
	    {ID, Udp1};
	_ ->
%	    ?INFO(?F("Couldn't encode recursive question: ~p",
%		     [{Domain, Class, Type}])),
	    {ID, Udp}
    end.

udp_send(undefined, IP, Buffer) -> udp_send(udp_open(), IP, Buffer);
udp_send(Udp, IP, Buffer) ->
    case gen_udp:send(Udp, IP, ?NAMESERVER_PORT, Buffer) of
	ok ->
	    Udp;
	{error, closed} ->
	    %% TBD delete; covers for old gen_udp bug.
	    RealReason = flush_udp_error(Udp, closed),
	    ?INFO(?F("~p: Restart (recursive) UDP socket - ~p",
		     [?MODULE, RealReason])),
	    udp_close(Udp),
	    udp_open();
	{error, Reason} ->
	    udp_close(Udp),
	    udp_open()
    end.

flush_udp_error(Udp, Reason) ->
    receive
	{udp_error, Udp, RealReason} ->
	    RealReason
    after 0 ->
	    Reason
    end.

udp_close(UDP) -> dns_recurse_udp_tracker:close_udp(UDP).
udp_open() -> dns_recurse_udp_tracker:open_udp().


mk_query(Domain, Class, Type, ID, RD) ->
    Rec = #dns_rec{header = #dns_header{id = ID, 
					opcode = ?QUERY,
					rd = RD,
					rcode = ?NOERROR},
		   qdlist = [#dns_query{domain = Domain, 
					type = Type, 
					class = Class}]},
    case inet_dns:encode(Rec) of
	{ok, Buffer} ->
	    {ok, list_to_binary(Buffer)};
	Error ->
	    Error
    end.

%%
%% get_first_non_pending_ns(NSs) = {FirstNonPendingNS,NSs--FirstNonPendingNS}
%%
get_first_non_pending_ns(NSs) -> get_first_non_pending_ns(NSs, []).
get_first_non_pending_ns([NS|NSs], Ack) when NS#ns.ip /= pending ->
    {NS, lists:reverse(Ack) ++ NSs};
get_first_non_pending_ns([NS|NSs], Ack) ->
    get_first_non_pending_ns(NSs, [NS|Ack]);
get_first_non_pending_ns([], Ack) ->
    {[], lists:reverse(Ack)}.

%%
%% compare two strings case insensitive wise.
%% must be identical lenght etc
%% str_match("fisk", fisk") -> true
%% str_match("fisk", fiSk") -> true
%% str_match("fisk", fisq") -> false
%%
str_match([A|AR], [A|BR]) -> str_match(AR, BR);
str_match([A|AR], [B|BR]) when A >= $A, A =< $Z, A+32==B -> str_match(AR, BR);
str_match([A|AR], [B|BR]) when A >= $a, A =< $z, A-32==B -> str_match(AR, BR);
str_match([], []) -> true;
str_match(_A, _B) -> false.

%%
%% Map upper-case to lower-case 
%%
tolower([H|T]) when H >= $A, H =< $Z -> [H+32 | tolower(T)];
tolower([H|T]) -> [H | tolower(T)];
tolower([]) -> [].
