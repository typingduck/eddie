-module(load_server).
-author('magnus@erix.ericsson.se').
%%%----------------------------------------------------------------------
%%% File    : load_server.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>,
%%%           Pekka.Hedqvist@ericsson.com
%%%           Geoff
%%% Purpose : Get load figures for our host, distribute to all
%%%           other hosts (thus every server holds info for all
%%%           others).
%%%           At periodic intervals load information is sent to
%%%           the DNS server(s). Only if this server is the 'global'
%%%           server.
%%% Notes   : Should be set to a higher run priority than other 
%%%           Erlang processes.
%%% Created : 7 Apr 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%
%%% Apr 1999 - geoff@eddieware.org - Changes to maintain 12 load avs
%%%            (each 5 seconds apart)
%%% Modified: 28 Apr 1999 by tobbe@eddieware.org
%%%           Corrected some of the calculations.
%%% Modified: 13 Sep 1999 by Pekka@Eddieware.org
%%%           Prevent backend node from becoming master.
%%%           Added md5 signatures to load data sent to DNS for somewhat 
%%%           more security.
%%% 21/07/00  clement.lyons@eddieware.org
%%%           Could previously match on migrated node FE node when 
%%%           calculating the FE factor in function be_factor/2.   
%%%----------------------------------------------------------------------
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
%%% Contributor(s): 
%%%     2000 Apr geoff@eddieware.org Fixes for agentless load balancing.
%%%         Now maintains its own list of "up" and "down" nodes instead
%%%         of relying on erlang nodes().
%%%
%%%----------------------------------------------------------------------
-vc('$Id: load_server.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
%% External exports
-export([start_link/0,dns_servers/1, subscribe_reject/0, add_fe/3, add_be/2,
	 delete_fe/1, delete_be/1, failover_fe/1, release_fe/1, init_be_ac/4,
	 lookup_best/0, best_nodes/0, init_seed/0, make_packet/1, status/0]).
%% Only for test
-export([node_load/0]).
%% Local exports
-export([resolve_global/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-behaviour(gen_server).
-include("load_server.hrl").
-include("logger.hrl").
-include("ip.hrl").

-record(state, {db,
		global,            % true | OtherGlobalPid
		cookie,
		udp,
		subscribers = [],
		be_ac = #be_ac{},
		dns_servers = [],  % [{DNS_server, Port}]
		%% a list of twelve 5-second load averages
		load_value = lists:duplicate(12, 1.0),
		os,
		load_avg,
		load_timer,
		dns_timer
	       }
       ).

%%%---------------------------------------------------------------------
%%% API
%%%---------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% DNSs == [{DNS_domain_name, Port}]
%% where Port = 0 implies default port.
%%
dns_servers(DNSs) when list(DNSs) ->
    catch gen_server:call(?MODULE, {dns_servers, DNSs}).

%%
%% Caller receives {reject, Percentage} at regular intervalls.
%% Percentage of new connection attempts that will be rejected
%% until next reject message is received.
%%
subscribe_reject() ->
    catch gen_server:call(?MODULE, {subscribe_reject, self()}).

%%
%% Add a new frontend IP address at this node.
%% The IP will be reported to the DNS servers.
%% 
%% Perc and Load represents the load threshold value at this host.
%% Load is the average cpu load the last minute
%% (as returned by UNIX uptime(1)).
%% Perc is the percentage Load represents of the total capacity of
%% the host (e.g. 0.8 = 80%).
%%
%% BE_nodes is a list of nodes (hosts) this frontend uses to perform
%% the actual job.
%%
add_fe(IP, {Perc, Load}, BE_nodes) when Perc > 0, Perc =< 1,
					number(Load), Load > 0,
					list(BE_nodes) ->
    gen_server:cast(?MODULE, {add_fe, IP, Perc, Load, BE_nodes}).
%%
%% Add a new backend IP address at this node.
%% Perc and Load is as above.
%%
add_be(IP, {Perc, Load}) when Perc > 0, Perc =< 1,
			      number(Load), Load > 0 ->
    ?TEMP_INFO(?F("add_be ~p ~p", [ IP, { Perc, Load } ])),
    gen_server:cast(?MODULE, {add_be, IP, Perc, Load}).
%%
%% Delete frontend IP address.
%%
delete_fe(IP) -> gen_server:cast(?MODULE, {delete, IP, ?FE}).

%%
%% Delete backend IP address.
%%
delete_be(IP) -> gen_server:cast(?MODULE, {delete, IP, ?BE}).
%%
%% IP has been moved to this node (host) from another node.
%% This IP will not be reported to the DNS servers. (I don't think so).
%% Only clients using cached IP addresses uses this type of
%% IP addresses.
%%
failover_fe(IP) ->  gen_server:cast(?MODULE, {failover_fe, IP}).
%%
%% Delete previously failovered IP address.
%%
release_fe(IP) -> gen_server:cast(?MODULE, {release_fe, IP}).
%%
%% Lookup the best (least loaded) backend node.
%% Note: init_seed/1 must have been executed before.
%%
lookup_best() -> lookup_best1().
%%
%% Lookup a list of backend nodes, sorted according to the
%% current load situations at all nodes.
%% Note: init_seed/1 must have been executed before.
%%
best_nodes() ->
    ?TEMP_INFO(?F("~p best_nodes()",[self()])),
    best_nodes1().
%%
%% inet_server listener process generates a random number
%% for each started accept process (argument) !!!
%%
init_seed() ->
    {X,Y,Z} = now(),
    random:seed(Z,X,Y).

status() ->
  gen_server:call({global,?MODULE},status).

%% Only for test
node_load() -> node_load1().

%%%----------------------------------------------------------------------
%%% Local exports.
%%%----------------------------------------------------------------------

%% Resolve a global name conflict.
resolve_global(Name, Pid1, Pid2) ->
    {Min, Max} = if node(Pid1) < node(Pid2) -> {Pid1, Pid2};
		    true                    -> {Pid2, Pid1}
		 end,
    Max ! {resolve_name_conflict, Name, Min},
    Min.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    Db = ets:new(?MODULE, [named_table, bag, {keypos, #load.node}]),
    ets:new(nodelist, [named_table, set, public]),
    set_system_error_logging(),
    global:sync(),
    {Global, Udp, DNS_timer} = new_global(),
    net_kernel:monitor_nodes(true),
    refresh(),     %% ask for load info from all other load_servers
    LoadRef = erlang:send_after(?LOAD_TIMEOUT, self(), load_timeout),
    OS = os:type(),
    Cookie = atom_to_list(erlang:get_cookie()),
    case init_load_avg(OS) of
	{ok, LoadAvg} ->
	    {ok, #state{db = Db,
			global = Global,
			udp = Udp,
			cookie = Cookie,
 			dns_servers = [],
			os = OS,
			load_avg = LoadAvg,
			load_timer = LoadRef,
			dns_timer = DNS_timer
		       }};
	Error ->
	    {stop, Error}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call({dns_servers, DNSs}, _, State) ->
    %% Set the current set of DNS servers.
    case check_dns(DNSs) of
	true ->
	    {reply, ok, State#state{dns_servers = dns(DNSs)}};
	_ ->
	    {reply, {error, invalid_DNS}, State}
    end;

handle_call({subscribe_reject, Pid}, _, State) ->
    #state{subscribers = Subs,
	   be_ac = #be_ac{'B' = B}} = State,
    case lists:member(Pid, Subs) of
	true ->
	    {reply, {error, already_member}, State};
	_ ->
	    tell_B([Pid], B),
	    link(Pid),
	    {reply, ok, State#state{subscribers = [Pid|Subs]}}
    end;

handle_call(status,_,State) ->
  {reply,format_status(State#state.db),State}.

handle_cast({add_fe, IP, Perc, LoadT, BE_nodes}, State) ->
%%  check if the IP, Perc, LoadT, really is there
%% 
%% 

    #state{global = G, dns_timer = DT, udp = UDP} = State,
    {G2, UDP2, DT2} = new_global(UDP, G, DT),
    LoadV = hd(State#state.load_value),
    Load = #load{class = ?FE,
		 type = ?STATIC,
		 status = up,
		 ip     = IP,
		 load = LoadV,
		 nodes = BE_nodes,
		 'R' = LoadT},
    gen_server:abcast(?MODULE, {self(), ip_up, Load}),
    {noreply, State#state{global = G2, udp = UDP2, dns_timer = DT2}};

handle_cast({add_be, IP, Perc, LoadT}, State) ->
    #state{global = G, dns_timer = DT, udp = UDP} = State,
    {G2, UDP2, DT2} = new_global(UDP, G, DT),
    LoadV = hd(State#state.load_value),
    Load = #load{class = ?BE,
		 ip = IP,
		 status = up,
		 type = ?STATIC,
		 load =  LoadV,
		 'R' = LoadT},
    gen_server:abcast(?MODULE, {self(), ip_up, Load}),
    {noreply, State#state{global = G2, udp = UDP2, dns_timer = DT2}};

handle_cast({failover_fe, IP}, State) ->
    #state{global = G, dns_timer = DT, udp = UDP} = State,
    {G2, UDP2, DT2} = new_global(UDP, G, DT),
    Load = #load{class = ?FE,
		 ip = IP,
		 status = up,
		 type = ?FAILOVER,
		 load = 0},
    gen_server:abcast(?MODULE, {self(), ip_up, Load}),
    {noreply, State#state{global = G2, udp = UDP2, dns_timer = DT2}};

handle_cast({release_fe, IP}, State) ->
    #state{global    = G,
	   dns_timer = DT,
	   udp       = UDP} = State,
    {G2, UDP2, DT2} = new_global(UDP, G, DT),
    gen_server:abcast(?MODULE, {self(), ip_down, IP, ?FE}),
    {noreply, State#state{global = G2, udp = UDP2, dns_timer = DT2}};

handle_cast({delete, IP, Class}, State) ->
    #state{global    = G,
	   dns_timer = DT,
	   udp       = UDP} = State,
    {G2, UDP2, DT2} = new_global(UDP, G, DT),
    gen_server:abcast(?MODULE, {self(), ip_down, IP, Class}),
    {noreply, State#state{global = G2, udp = UDP2, dns_timer = DT2}};

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% send back this node Loads to asker

handle_cast({Pid, refresh}, State) ->
    case ets:lookup(State#state.db, node()) of
	[] ->
	    {noreply, State};
	Loads ->
	    F = fun(Load) -> gen_server:cast(Pid, {self(), ip_up, Load}) end,
	    lists:foreach(F, Loads),
	    {noreply, State}
    end;

handle_cast({Pid, ip_info, Load}, State) ->
    #state{db = Db, be_ac = #be_ac{'SR' = SR}} = State,
    case new_fraction(Db, Load, SR) of
	{ok, F, OldL} ->
	    ets:match_delete(Db, OldL),
	    ets:insert(Db, Load#load{f = F}),
	    ?TEMP_INFO(?F("IP_INFO F=~p ets:insert ~p ~p", [ F, Pid, Load ])),
	    {noreply, State};
	%% previously unknown IP
	ip_up ->
	    ?TEMP_INFO(?F("IP UNKNOWN - doing an IP_UP ~p", [ Load ])),
	    handle_cast({Pid, ip_up, Load}, State)
    end;

%% 
handle_cast({Pid, ip_up, Load}, State) ->
    #load{node = Node,
	  class = Class,
	  ip = IP,
	  type = Type,
	  'R' = Threshold,
	  host = Host,
	  nodes = Nodes,
	  load = LoadV} = Load,
    #state{db = Db,
	   udp = Udp,
	   subscribers = Subs,
	   be_ac = BE_AC} = State,
    Finit = init_fraction(Db, Node, Class, Threshold, BE_AC),
    ets:match_delete(Db, ?MATCH(Node, Class, IP)),
    ets:insert(Db, Load#load{f = Finit}),
    ets:insert(nodelist,{Node,0}),
    ?TEMP_INFO(?F("Node (~p:~p) accepting connections", [ Node, IP ])),
    Udp2   = tell_dns_up(Type, Class, Udp, Db, IP, Host, Nodes, State),
    BE_AC1 = init_be_ac(Class, BE_AC, Db, Subs),
    {noreply, State#state{udp = Udp2, be_ac = BE_AC1}};

handle_cast({Pid, ip_down, Load}, State) ->
    #state{db          = Db,
	   udp         = Udp,
	   subscribers = Subs,
	   be_ac       = BE_AC} = State,
    case ets:match_object(Db, ?MATCH(Load#load.node, Load#load.class, Load#load.ip)) of
	[Down|_] ->
	    ets:delete(nodelist, Load#load.node),
	    ets:match_delete(Db, ?MATCH(Load#load.node, Load#load.class, Load#load.ip)),
	    ?INFO(?F("Node (~p:~p) taken down", [ Load#load.node, Load#load.ip ]));
	_        ->
	    ?TEMP_INFO(?F("received ip_down from: ~p", [ Load ]))
    end,
    Udp1   = tell_dns_down(Load#load.type, Load#load.class, Udp, Db, Load#load.ip, State),
    State1 = State#state{udp = Udp1},
    BE_AC1 = init_be_ac(Load#load.class, BE_AC, Db, Subs),
    {noreply, State1#state{be_ac = BE_AC1}};

handle_cast({Pid, ip_down, IP, Class}, State) ->
    #state{db = Db,
	   udp = Udp,
	   dns_timer = DNS_timer,
	   subscribers = Subs,
	   be_ac = BE_AC} = State,
    Udp1 = case ets:match_object(Db, ?MATCH('_', Class, IP)) of
        	[Down|_] ->
            		ets:delete(nodelist, Down#load.node),
            		tell_dns_down(Down#load.type, Class, Udp, Db, IP, State);
        	_        ->
            		?TEMP_INFO(?F("received ip_down from unknown CLASS: ~p IP: ~p", [Class, IP])),
			Udp
    end,
    ets:match_delete(Db, ?MATCH(node(Pid), Class, IP)),
    BE_AC1 = init_be_ac(Class, BE_AC, Db, Subs),
    {noreply, State#state{udp = Udp1, be_ac = BE_AC1}};


handle_cast(Unknown, State) ->
    ?ERROR(?F("Unknown handle_cast in load_server.erl (~p)", [ Unknown ])),
	{noreply, State}.


%% END of Callback functions from gen_server
%%
%%----------------------------------------------------------------------
%% Stuff needed for the load_timeout computations
%% 
%% This computations might seems strange but they are "correct" (!).
%% They are there to remove the "uptime" one minut average returned
%% and instead create the "real" process quelength insead on a one 
%% minut average!
%% 

rm_last([X])   -> []; %% rm last elem
rm_last([H|T]) -> [H|rm_last(T)];
rm_last([])    -> [].

sum(L) ->
    F = fun(X,Acc) -> X + Acc end,
    lists:foldl(F,0,L).

%% Compute a running load average.
%% Assumes Loads has 12 elements 
%% (as it should if properly maintained)
sumav(Lv, Loads) -> max(((Lv * 12) - sum(Loads)),
			0).
%%----------------------------------------------------------------------
%%
%% Algorithm:
%%  1. Get current load value.
%%  2. Remove the oldest 5 sec value we've got from the load vector.
%%  3. Compute a new 5 sec value.
%%  4. Use the new value to update the load information.
%%  5. Save the new 5 sec value in the load vector.
%%
handle_info(load_timeout, State) ->
    #state{db = Db,
	   os = OS,
	   load_avg = LoadAvg,
	   subscribers = Subs,
	   be_ac = BE_AC} = State,
    LoadV    = get_load(OS, LoadAvg, State),
    Tv       = rm_last(State#state.load_value),
    NewLoadV = sumav(LoadV,Tv),
    update_load(Db, OS, LoadAvg, NewLoadV, State),
    BE_AC1 = be_ac(?BE, BE_AC, Db, Subs),
    LoadRef = erlang:send_after(?LOAD_TIMEOUT, self(), load_timeout),
    S1 = State#state{load_value = [NewLoadV|Tv],
		     be_ac = BE_AC1,
		     load_timer = LoadRef},
    {noreply, S1};

handle_info({timeout,REF,dns_timeout}, State) when State#state.dns_timer == REF ->
    #state{udp = Udp,
	   cookie = Cookie,
	   global      = Global, 
	   dns_servers = DNSs,
	   db = Db} = State,
    ?TEMP_INFO(?F("dns_timeout ~w",[erlang:now()])),
    Udp1 = tell_dns(DNSs, Udp, Db, Cookie),
    DNSRef = dns_timer(Global),
    {noreply, State#state{dns_timer = DNSRef,
			  udp = Udp1}};

handle_info({resolve_name_conflict, ?MODULE, Pid}, State) ->
    link(Pid),
    #state{udp = Udp, dns_timer = DNS_timer} = State,
    close_udp(Udp),
    erlang:cancel_timer(DNS_timer),
    rpc:abcast(nodes(), ?MODULE, {self(), new_global, Pid}),
    {noreply, State#state{udp = undefined,
			  dns_timer = undefined,
			  global = Pid}};

handle_info({Pid, new_global, GPid}, State) when Pid == State#state.global ->
    unlink(Pid),
    link(GPid),
    erlang:cancel_timer(State#state.dns_timer),
    DNS_Timer =  State#state.dns_timer,
    %% be sure we have flush any outstanding dns_timer message
    receive {timeout, DNS_Timer, dns_timer} -> ok
    after   0                               -> ok  end,
    close_udp(State#state.udp),
    GPid2 = if self() == GPid -> true; true -> GPid end,
    {noreply, State#state{global = GPid2, dns_timer = undefined, udp = undefined}};

%% we previously failed to open and udp, retry
handle_info(retry_udp, State)  ->
    case {State#state.udp, State#state.global} of
	{undefined, true} -> %% yes no udp and we are global retry!
	    UDP = open_udp(true),
	    {noreply, State#state{udp = UDP}};
	_Else ->
	    {noreply, State}
    end;

%% failed to register as global previously and we must make sure we retry..
handle_info(retry_global_register, State) ->
    {Global, Udp, DNS_timer} = new_global(),
    {noreply, State#state{udp       = Udp,
			  dns_timer = DNS_timer,
			  global    = Global}};

handle_info({'EXIT', Pid, _}, State) when Pid == State#state.global ->
    {Global, Udp, DNS_timer} = new_global(),
    {noreply, State#state{udp = Udp,
			  dns_timer = DNS_timer,
			  global = Global}};

handle_info({'EXIT', Pid, _}, State) when Pid == State#state.load_avg ->
    case init_load_avg(State#state.os) of
	{ok, LoadAvg} ->
	    {noreply, State#state{load_avg = LoadAvg}};
	Error ->
	    {stop, Error, State}
    end;

handle_info({'EXIT', Pid, _}, State) ->
    #state{subscribers = Subs} = State,
    case lists:member(Pid, Subs) of
	true ->
	    {noreply, State#state{subscribers = lists:delete(Pid, Subs)}};
	_ ->
	    {noreply, State}
    end;

%% When state.global is a pid then we are NOT the global load_server
%% and its the global server that has crashed.
%% so the dns:s need to be informed (several times but thats no problem)
%% and we have to participate in the new global load_server election!
handle_info({nodedown, Node}, State) when pid(State#state.global) ->
    ?TEMP_INFO(?F("Node ~w received nodedown from ~w with global set to: ~w",
	     [node(), Node, State#state.global])),
    #state{db = Db,
	   subscribers = Subs,
	   be_ac = BE_AC} = State,
    {Global, Udp, DNS_timer} = new_global(),
    Udp1 = tell_dns_node_down(Udp, Db, Node, Global, State),
    delete_ips(Node, Db),
    BE_AC1 = init_be_ac(?BE, BE_AC, Db, Subs),
    {noreply, State#state{udp = Udp1,
			  dns_timer = DNS_timer,
			  global = Global,
			  be_ac = BE_AC1}};

%% we are global, and a node has dissapered.
handle_info({nodedown, Node}, State) when State#state.global == true ->
    ?TEMP_INFO(?F("Node ~w received nodedown from ~w with global set to: ~w",
	     [node(), Node, State#state.global])),
    #state{db = Db,
	   udp = Udp,
	   subscribers = Subs,
	   global = Global,
	   be_ac = BE_AC} = State,
    Udp1 = tell_dns_node_down(Udp, Db, Node, Global, State),
    delete_ips(Node, Db),
    BE_AC1 = init_be_ac(?BE, BE_AC, Db, Subs),
    {noreply, State#state{udp = Udp1,
			  be_ac = BE_AC1}};

handle_info({nodeup, Node}, State) ->
    ?TEMP_INFO(?F("Node ~w received nodedup from ~w with global set to: ~w",
	     [node(), Node, State#state.global])),
    gen_server:cast({?MODULE, Node}, {self(), refresh}),
    {noreply, State};

handle_info({udp_closed, Udp}, State) when State#state.udp == Udp ->
    NewUdp = open_udp(State#state.global),
    {noreply, State#state{udp = NewUdp}};

handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

check_dns([{DNS, Port}|DNSs]) when integer(Port), Port >= 0 ->
    case inet:getaddr(DNS,inet) of
	{ok,_} ->
	    check_dns(DNSs);
	_ ->
	    ?TEMP_INFO(?F("Invalid DNS server - ~p", [DNS])),
	    false
    end;
check_dns([H|_]) ->
    ?TEMP_INFO(?F("Invalid DNS server - ~p", [H])),
    false;
check_dns([]) ->
    true.

%%
%% Port = 0 implies default port.
%% Ignore invalid servers !
%%
dns([{DNS, 0}|T]) when list(DNS) ->
    [{DNS, ?DNS_LOAD_PORT}| dns(T)];
dns([{DNS, Port}|T]) when list(DNS), integer(Port) ->
    [{DNS, Port}| dns(T)];
dns([]) ->
    [].

%%
%% Initiate the load average method.
%%
init_load_avg({unix, sunos}) ->
    Version = os:version(),
    if
	element(1, Version) > 4 ->
	    ensure_cpu_sup_started();
	true ->
	    ?FATAL(?F("Unsupported OS version ~p", [Version])),
	    {error, no_load_monitor}
    end;
init_load_avg({unix, linux}) ->
    case open_linux_load() of
	{ok, FdPid} ->
	    {ok, FdPid};
	Error ->
	    ?FATAL(?F("Couldn't open /proc/loadavg - ~p", [Error])),
	    Error
    end;
init_load_avg({unix, freebsd}) ->
    case open_freebsd_load() of
	{ok, Port} ->
	    {ok, Port};
	Error ->
	    ?FATAL(?F("Couldn't inititate loadavg - ~p", [Error])),
	    Error
    end;
init_load_avg({win32, nt}) ->
    case open_nt_load() of
        {ok, Port} ->
            {ok, Port};
        Error ->
	    ?FATAL(?F("Couldn't inititate NTLoadCounter - ~p", [Error])),
	    Error
    end;
init_load_avg(OS) ->
    ?FATAL(?F("Unsupported OS - ~p", [OS])),
    {error, no_load_monitor}.

ensure_cpu_sup_started() ->
    case cpu_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    {ok, Pid};
	Error ->
	    ?FATAL(?F("Couldn't start cpu supervisor - ~p", [Error])),
	    Error
    end.

open_linux_load() ->
    file:open("/proc/loadavg",[read]).

open_freebsd_load() ->
    Prog = filename:join(code:priv_dir(load_info), "load_avg"),
    case catch open_port({spawn, Prog}, []) of
	Port when port(Port) -> {ok, Port};
	Error                -> {error, Error}
    end.

open_nt_load() ->
    Prog = filename:join(code:priv_dir(load_info), "NTLoadCounter"),
    case catch open_port({spawn, Prog}, [use_stdio]) of
	Port when port(Port) -> {ok, Port};
	Error                -> {error, Error}
    end.


%%
%% Name: get_load/0
%% Purpose: returns the latest 5 second load average
%%
get_load({unix, sunos}, _, _) ->
    cpu_sup:avg1() / 256;
get_load({unix, linux}, LoadAvg, State) ->
    file:position(LoadAvg,bof),
    case file:read(LoadAvg,80) of
	{ok, Data} ->
	    [Load60|_] = string:tokens(Data, " \n"),
	    list_to_float(Load60);
	eof ->
	    1.0;
	Error ->
	    ?FATAL(?F("Couldn't read /proc/loadavg - ~p", [Error])),
	    throw({stop, Error, State})
    end;
get_load({unix, freebsd}, Port, State) ->
    Port ! {self(), {command, [0]}},
    receive
	{Port, {data, [OpCode|T]}} ->
	    decode_freebsd(OpCode, T, State);
	{Port, closed} ->
	    ?FATAL(?F("Couldn't get loadavg - port closed", [])),
	    throw({stop, closed, State});
	{'EXIT', Port, Reason} ->
	    ?FATAL(?F("Couldn't get loadavg - port exited ~p", [Reason])),
	    throw({stop, {error, Reason}, State})
    end;
get_load({win32, nt}, Port, State) ->
    Port ! {self(), {command, [0]}},
    receive
        {Port, {data, T}} ->
            decode_nt(T, State);
        {Port, closed} ->
	    ?FATAL(?F("Couldn't get NT load - port closed", [])),
	    throw({stop, closed, State});
	{'EXIT', Port, Reason} ->
	    ?FATAL(?F("Couldn't get NT load - port exited ~p", [Reason])),
	    throw({stop, {error, Reason}, State})            
    end.

decode_freebsd(OpCode, [I0,I1,I2,I3], _) ->
    Load = (((I0) bsl 24) bor ((I1) bsl 16) bor ((I2) bsl 8) bor (I3)),
    if
	OpCode == 0 ->
	    %% Kept two decimals
	    Load / 100;
	true ->
	    Load
    end;
decode_freebsd(_, Data, State) ->
    ?FATAL(?F("Couldn't get loadavg - bad data ~p", [Data])),
    throw({stop, bad_data, State}).

decode_nt([I0,I1,I2,I3], _) ->
    Load = (((I0) bsl 24) bor ((I1) bsl 16) bor ((I2) bsl 8) bor (I3)),
    Load / 100;
decode_nt(Data, State) ->
    ?FATAL(?F("Couldn't get NT load - bad data ~p", [Data])),
    throw({stop, bad_data, State}).

%%----------------------------------------------------------------------
%%
%% Try to register our selfs as the global (master) server.
%% If another server already is global, return the Pid of
%% that process.
%%
global_register() -> global_register(can_become_master()).
global_register(true) ->
    case global:register_name(?MODULE, self(), {?MODULE, resolve_global}) of
	yes -> 
	    ?DIST_INFO(?F("~p is the newly elected global load_server~n", [ node() ])),
        true;
	_ ->
	    case global:whereis_name(?MODULE) of
		   undefined ->
		    retry_global_register();
		Pid when Pid /= self() -> %% there is a global and its not me..
		       link(Pid),
		    Pid;
		OwnPid  ->
		    true
	       end
    end;

global_register(_false) ->
    case global:whereis_name(?MODULE) of
	undefined ->
	    ?INFO(?F("Warning, no master load_server registred! "
		     "Also, this process (~w) is only a backend and cannot "
		     "become master load_server",[self()])),
	    retry_global_register();
	Pid ->
	    link(Pid),
		      Pid
    end.
    
%% new_global() -> {Global, Udp, DNS_timer}
new_global() ->
    Global = global_register(),
    Udp = open_udp(Global),
    DNS_timer = dns_timer(Global),
    {Global, Udp, DNS_timer}.

%%
%% new_global(UDP, Global, DNS_Timer) -> {Global, UDP, DNS_Timer1}
%% If caller is global (Global == true} return same args
%% otherwise participat en elections
%%
new_global(UDP, true, DNSTimer) -> {true, UDP, DNSTimer};
new_global(OUdp, _Global, ODNSTimer) ->
    Global = global_register(),
    Udp = open_udp(Global),
    DNS_timer = dns_timer(Global),
    Udp1 = if Udp == undefined -> OUdp; true -> Udp end,
    DNS_timer1 = if DNS_timer== undefined -> ODNSTimer; true -> DNS_timer end,
    {Global, Udp1, DNS_timer1}.

%% can_become_master() -> true/false
%% a node can only become master if any of the #load.node
%% values in the db for matching node() is ?FE
%%
can_become_master() ->
    Loads = ets:match_object(?MODULE, ?MATCH(node(),'_','_')),
    lists:any(fun(Load) -> Load#load.class == ?FE end, Loads).

%% 
%% retry_global_register/0 -> undefined
%% makes the process retry to be in the global election process 0.5 sec from now
%%
retry_global_register() ->
    erlang:send_after(?RETRY_GLOBAL_TIMEOUT, self(), retry_global_register),
    undefined.



%%----------------------------------------------------------------------
%%
%% Wants information from all other load_servers.
%%
refresh() ->
    gen_server:abcast(nodes(), ?MODULE, {self(), refresh}).

%%----------------------------------------------------------------------
%% Open an udp socket if we are the global load_server.
%% We do accept that it isn't possible to open an udp socket
%% (no file descriptors left) as the global process floats around
%% and we do not want to terminate the node for this reason.
%% Instead we try again later !!!
%%
open_udp(true) ->
    case gen_udp:open(0, [list]) of
	{ok, Udp} ->    Udp;
	Error     ->
	    %% Try to open it whenever it shall be used.
	    ?DIST_INFO(?F("~p: Can't open DNS socket (UDP)", [?MODULE])),
	    retry_udp()
    end;
open_udp(_) ->    undefined.

retry_udp() ->
    %% we use same timeout as dns, just to save typing..
    erlang:send_after(?DNS_TIMEOUT, self(), retry_udp),
    undefined.

close_udp(undefined) -> ok;
close_udp(Udp)       -> gen_udp:close(Udp).

%%----------------------------------------------------------------------
%% Start the dns timer if we are the global load_server.
%%
dns_timer(true) ->
    erlang:start_timer(?DNS_TIMEOUT, self(), dns_timeout);
dns_timer(_) ->  undefined.

%%----------------------------------------------------------------------
%% Delete all IP's for Node.
%%
delete_ips(Node, Db) ->
    ets:delete(Db, Node).

%% Admission Control and Load information functions.

%%
%% Name: update_load
%% Purpose:
%% Gets the load at the j:th measurement period and updates
%% all IP's (BE and FE) belonging to our node.
%% Send the updated BE/FE info to all other load_servers.
%%  (Agent-based load collection)
%%
update_load(Db, OS, LoadAvg, LoadV, State) ->
    Node = node(),
    LoadL = ets:lookup(Db, Node),
    ?TEMP_INFO(?F("update_load for ~p to (~p)", [ Node, LoadL ])),
    ets:delete(Db, Node),
    FF = fun(N) when N#load.class == ?BE ->
		 #load{f = F, 'R' = MR} = N,
		 Rj = LoadV,
		 Fj = fraction(F, Rj, MR),
		 L = N#load{load = Rj,
			    f = low_mark(Fj)},
		 ets:insert(Db, L),
		 gen_server:abcast(nodes(), ?MODULE, {self(), ip_info, L});
	    (N) when N#load.type == ?STATIC ->
		 Rj = LoadV,
		 L = N#load{load = Rj},
		 ets:insert(Db, L),
		 gen_server:abcast(nodes(), ?MODULE, {self(), ip_info, L});
	    (N) ->
		 Rj = LoadV,
		 L = N#load{load = Rj},
		 ets:insert(Db, L),
		 gen_server:abcast(nodes(), ?MODULE, {self(), ip_info, N})
	 end,
    lists:foreach(FF, LoadL).

%%
%% Name: uplist/0
%% Purpose: return a list of up and running nodes (that have supplied
%%  load information.
%%  It returns all the "fake" nodelist monitor node nodes AND all "real"
%%  nodes
uplist() ->
    %%  UL = ets:match(nodelist, {'$1','_'}),
    ?TEMP_INFO(?F("length of uplist is ~w",[length(ets:tab2list(nodelist))])),
    DelDupFun = fun(X,Ack) ->
			N = case X of {No,_} -> No; _ -> X end, %% if {N,V} from nodelist
			case lists:member(N, Ack) of
			    true -> Ack;
			    false -> [N|Ack]
			end
		end,
    %% remove duplicate nodes..
    Unique = lists:foldl(DelDupFun,[],ets:tab2list(nodelist) ++ [node()|nodes()]),
    ?TEMP_INFO(?F("uplist returned ~w", [Unique])),
    Unique
	.

%% ------------------------------------------------------------
%% Recalculate the static parts of the admission control
%% algoritm according to the new set of active IP addresses.
%% A new nodes fraction list will also be created as a ?BE node
%% has (dis)appeared.
%% init_be_ac(Class, BE_AC, Db, Subs) -> BE_AC'
%% ------------------------------------------------------------                 
init_be_ac(?FE, BE_AC, _, _) ->
    BE_AC;
init_be_ac(?BE, BE_AC, Db, Subs) ->
    {R, SR} = new_R(Db),
    %% Ignore new Rj, we are only interested in the
    %% side effects, i.e. update the nodes fraction list.
    Nodes = uplist(),
    ?TEMP_INFO(?F("init_be_ac on nodes (~p)", [ Nodes ])),
    new_rf(Nodes, Db),
    BE_AC#be_ac{'R' = R, 'SR' = SR};
init_be_ac(A, B, C, D) ->
    ?TEMP_INFO(?F("Unknown class of server (~p) in loadserver", [ A ])).

%%
%% new_R(Db) -> {AverageR, SumR}
%% Calculate the sum and average threshold load value of all BE's.
%%
new_R(Db) ->
    Nodes = uplist(),
    F = fun(Node, {R, N}) ->
		?TEMP_INFO(?F("new_R Node: ~w, {~w, ~w}",[Node, R, N])),
		case ets:match_object(Db, ?MATCH(Node, ?BE, '_')) of
		    [L|_] -> {R + L#load.'R', N + 1};
		    _     -> {R, N}
		end
	end,
    {SR, Nb} = lists:foldl(F, {0,0}, Nodes),
    X = if
	Nb > 0 -> {SR / Nb, SR};
	true   -> {0, 0}
	end,
    ?TEMP_INFO(?F("new_R returned: ~w",[X])),
    X.

%% ---------------------------------------------------------------------
%% Recalculate the admission control algoritm according
%% to the new load values.
%% be_ac(Class, BE_AC, Db, Subs) -> BE_AC'
%% ------------------------------------------------------------
be_ac(?FE, BE_AC, _, _) ->
    BE_AC;
be_ac(?BE, BE_AC, Db, Subs) ->
    #be_ac{'G' = G, 'R' = R, 'B' = B} = BE_AC,
    {Gj, Bj} = recalc(Db, R, B, G),
    tell_B(Subs, Bj),
    BE_AC#be_ac{'G' = Gj, 'B' = Bj}.

%%
%% Calculate the new G (geometrically decaying average BE load) and
%% new B (fraction of connection attempts that should be rejected)
%% during the j:th measurement period.
%%
%% See more about side effects of new_rf/2 below.
%%
recalc(Db, R, B, G) ->
    Nodes = uplist(),
    Rj = new_rf(Nodes, Db),
    Gj = ?MIN_G(?ALPHA * G + (1 - ?ALPHA) * Rj),
    Bj = new_b(B, R, Gj),
    {Gj, Bj}.

%%
%% Calculate the fraction of new requests that should be
%% rejected by the FE's.
%%
%% If B has reached 100% rejection rate it has to be lowered
%% in the formula below; otherwise it will never be lowered.
%%
%%  Hey, check with Mike (mpr@eddieware.org) about this stuff !!
%%    /Tobbe (tobbe@eddieware.org)            
%%
new_b(B,R,Gj) ->
    change_b(1 - B,R,Gj).

-define(MIN_B_CHANGE, 0.01).
-define(BETA, 0.01).

change_b(OldAcc,R,Gj) when Gj =< R ->
    %% Load is low !!
    X1 = OldAcc * (1 + ?BETA),
    %% Force a minimum change
    X2 = if ((X1 - OldAcc) < ?MIN_B_CHANGE) -> OldAcc + ?MIN_B_CHANGE;
	    true                            -> X1
	 end,
    %% Make sure we stay in (0,1)
    X3 = if (X2 > (1 - ?MIN_B_CHANGE)) -> 1;
	    true                       -> X2
	 end,
    (1 - X3);
change_b(OldAcc,R,Gj) ->
    %% Load is high !!
    X1 = OldAcc * (1 - ?BETA),
    %% Force a minimum change
    X2 = if ((OldAcc - X1) < ?MIN_B_CHANGE) -> OldAcc - ?MIN_B_CHANGE;
	    true                            -> X1
	 end,
    %% Make sure we stay in (0,1)
    X3 = if (X2 < ?MIN_B_CHANGE) -> 0;
	    true                 -> X2
	 end,
    (1 - X3).

%%
%% new_rf(Nodes, Db) -> Rj
%% Calculate the average BE load during j:th measurement period.
%% 
%% As a side effect of this function, the fraction of all BE IP's
%% will be normalized and the nodes fraction list will be
%% updated.
%%
new_rf(Nodes, Db) ->
    F = fun(Node, {R, N, Fn}) ->
		case ets:match_object(Db, ?MATCH(Node, ?BE, '_')) of
		    [Li|_] ->
			#load{load = Ri, f = Fi} = Li,
			{R + Ri, N + 1, Fn + Fi};
		    _ ->
			{R, N, Fn}
		end
	end,
    {SR, Nb, Fs} = lists:foldl(F, {0,0,0}, Nodes),
    normalize_fs(Nodes, Db, Fs),
    if
	Nb > 0 -> SR / Nb;
	true   -> 0
    end.

%%
%% Normalize the fraction among all BE nodes (all IP's at one
%% node get the same fraction).
%% Update the node fraction list used by e.g. best_nodes/0.
%%
normalize_fs(Nodes, Db, Fsum) ->
    F = fun(Node) ->
		IPs = ets:lookup(Db, Node),
		FF = fun(N, _) when N#load.class == ?BE ->
			     Fi = N#load.f,
			     Fj = Fi / Fsum,
			     ets:match_delete(Db, N), %% delete old instance
			     ets:insert(Db, N#load{f = low_mark(Fj)}),
			     [{Node, Fj}];
			(_, Ack) ->
			     Ack
		     end,
		lists:foldl(FF, [], IPs)
	end,
    Ns = lists:flatmap(F, Nodes),
    ?TEMP_INFO(?F("All fraction: ~p~n", [ Ns ])),
    ets:delete(Db, nodes),  %% It's a bag !
    ets:insert(Db, #nodes{fractions = Ns}),
    ok.

%%
%% Inform subscribers about the new rejection rate of
%% new incoming requests.
%%
tell_B(Subscribers, B) -> tell_B1(Subscribers, {reject, B}).

tell_B1([Pid|Subs], Msg) ->
    Pid ! Msg,
    tell_B1(Subs, Msg);
tell_B1([], _) ->
    ok.

%%
%% Calculate the initial fraction of an FE/BE.
%% A FE is not used locally, the DNS holds own values.
%%
init_fraction(Db, Node, Class, Threshold, BE_AC) ->
    case ets:match_object(Db, ?MATCH(Node, Class, '_')) of
	_ when Class == ?FE ->
	    ?FE_FRACTION;
	[] ->
	    SR = BE_AC#be_ac.'SR',
	    Threshold / (SR + Threshold);
	[L|_] ->
	    %% Set initial fraction according to old backend
	    %% IP's at the same node.
	    L#load.f
    end.

%%
%% Calculate the new fraction of a BE according to the
%% load value.
%% A FE don't use the local fraction value, however we
%% do also notice if this IP is previously unknown.
%%
new_fraction(Db, Load, SR) ->
    #load{node = Node,
	  class = Class,
	  ip = IP,
	  'R' = MR,
	  load = Rj} = Load,
    case ets:match_object(Db, ?MATCH(Node, Class, IP)) of
	[] ->
	    %% New IP.
	    ip_up;
	[Old] when Class == ?FE ->
	    {ok, Old#load.f, Old};
	[Old] ->
	    %% BE, Calculate a new F fraction relative the old
	    %% value at this node.
	    #load{f = F} = Old,
	    Fj = fraction(F, Rj, MR),
	    {ok, low_mark(Fj), Old}
    end.

%%
%% Select a node according to fractions of the nodes.
%% Thus, the resulting node should (may) look different for
%% every call but the number of times a specific node is
%% choosen will settle around the given Node fraction.
%%
lookup_best1() ->
    [#nodes{fractions = Nodes}] = ets:lookup(?MODULE, nodes),
    Rand = random:uniform(),
    {_, Node, _} = select(Nodes, 0, Rand, []),
    Node.

%%
%% Sort the node list according to fractions of the nodes.
%% Thus, the resulting list should (may) look different for
%% every call but the number of times a specific node is
%% the head of the list will settle around the given Node
%% fraction.
%%
best_nodes1() ->
    X = ets:lookup(?MODULE,nodes),
    ?TEMP_INFO(?F("Best nodes fractions: ~p", [ X ])),
    BS = 
	case X of
	[#nodes{fractions = Nodes}] when length(Nodes) > 0 ->
	    Rand = random:uniform(),
	    sort(Nodes, Rand, 1);
	_ ->
	    %% No backends are started.
	    []
    end,
    ?TEMP_INFO(?F("Best nodes sortes: ~p", [ BS ])),
    BS.


%% Sort a list of nodes using the fraction numbers related
%% to each Node and the generated random number.
%% The random number interval is decreased (each recursive call)
%% related to the fraction of choosen nodes.
%%
sort([{Node, _}], _, _) ->
    [Node];
sort(Nodes, Rand, Tot) ->
    {Part, Node, Nodes1} = select(Nodes, 0, Rand, []),
    Tot1 = Tot - Part,
    [Node|sort(Nodes1, Rand * Tot1, Tot1)].

%%
%% Select the Node representing the Rand(om) part (%) of
%% the still existing cake.
%%
%% The last clause is there in order to cover for the
%% case Rand > N + P (with an empty list) which is in theory
%% impossible. But, the precision of the float may result
%% in this case. The choosen Node is the correct one anyhow !
%%
select([{Node, P}|Nodes], N, Rand, Ack) when Rand =< N + P ->
    {P, Node, Ack ++ Nodes};
select([{Node, P}|Nodes], N, Rand, Ack) ->
    select(Nodes, N + P, Rand, [{Node, P}|Ack]);
select([], _, _, [{Node, P}|Ack]) ->
    {P, Node, Ack}.

%%----------------------------------------------------------------------
%% The send DNS information part.

%%
%% Send load information about all active ?STATIC frontend IP
%% addresses to the DNS servers.
%% Load per IP = average(ALL) + IPload
%% All udp send  functions return a new Udp value
%% in case the Udp terminated (TBD remove, cover for old bug in gen_udp).
%%
tell_dns(DNSs, UDP, Db, Cookie) ->
    LoadInfo = lookup_all_static(Db),
    F = fun(Load, Udp) ->
		#load{ip = IP,
		      'R' = R,
		      load = Ri,
		      host = Host,
		      nodes = BE_nodes} = Load,
		%BE_f = be_factor(BE_nodes, Db),
		BE_f  = be_factor(uplist(), Db),
		FE_f = fe_factor(R, Ri),
		Udp1 = send_packet(DNSs, Udp,
				   [?LOADINFO, IP, Host, FE_f, BE_f], Cookie),
		Udp1
	end,
    lists:foldl(F, UDP, LoadInfo).

%%
%% The fraction of new connections to a FE will be adjusted proportionally
%% to the relative difference between Ri and R (by the DNS servers).
%%
fe_factor(R, Ri) ->   factor(R, Ri).


%% Calculate an adjustment factor proportionell to the relative
%% difference between Ri and R.
%%
factor(0,_)               -> 0;
factor(R,Ri) when Ri =< R -> 1 + ?PHI1;
factor(_,_)               -> 1 - ?PHI1.

%%
%% The fraction of new connections to a LAN (cluster) will be adjusted 
%% proportionally to the relative difference between the average current
%% load of all BE's and the average target utilization of the BE's (by
%% the DNS servers).
%%
be_factor(Nodes, Db) ->
    % be calculations
    Fun_BE = fun(Node, {SR, SRr, NoOfNodes}) ->
		case ets:match_object(Db, ?MATCH(Node, ?BE, '_')) of
		    [#load{'R' = R, load = Rr}|_] ->
			{SR + R, SRr + Rr, NoOfNodes + 1};
		    _ ->
			{SR,     SRr,      NoOfNodes}
		end
	end,
    {R_BE, Ri_BE, N_BE_Nodes} = lists:foldl(Fun_BE, {0,0,0}, Nodes),
    BE_More_Or_Less = case N_BE_Nodes of
			  0 ->
			      0;
			  _ ->
			      Av_BE_Thresh              = R_BE  / N_BE_Nodes,
			      Av_BE_Resp_Time           = Ri_BE / N_BE_Nodes,
			      factor(Av_BE_Thresh, Av_BE_Resp_Time)
		      end,
%%
    %% fe calculations
    Fun_FE = fun(Node, {SR, SRr, NoOfNodes}) ->
		    case lookup_all_static(Node, Db) of
		    [#load{'R' = R, load = Rr}|_] ->
			{SR + R, SRr + Rr, NoOfNodes + 1};
		    _ ->
			{SR,     SRr,      NoOfNodes}
		end
	end,
    {R_FE, Ri_FE, N_FE_Nodes} = lists:foldl(Fun_FE, {0,0,0}, Nodes),
    Av_FE_Thresh              = R_FE / N_FE_Nodes,
    Av_FE_Load                = Ri_FE / N_FE_Nodes,
    FE_More_Or_less           = factor(Av_FE_Thresh, Av_FE_Load),
    %% ; = OR, if less than 1 they are overloaded and return ?TO_HIGH,
    %% otherwise return TO_LOW (normal case)
    if (BE_More_Or_Less =< 1) ; (FE_More_Or_less =< 1) -> %% OR
	    1-?PHI;
       true -> %% else
	    1+?PHI
    end.


%%
%% Send a protocol packet to all DNS servers.
%%
send_packet(DNSs, UDP, P, Cookie) ->
    ?TEMP_INFO(?F("~w send packet at node: ~w Packet: ~w",[erlang:now(), node(), P])),
    PPF  = lists:flatten(make_packet(P)),
    Sign = crypto:md5_final(crypto:md5_update(crypto:md5_init(), Cookie ++ PPF)),
    send_packet1(DNSs, UDP, [PPF, size(Sign), Sign]).
    
send_packet1([DNS|DNSs], Udp, Packet) ->
    Udp1 = do_send_packet(Packet, DNS, Udp),
    send_packet1(DNSs, Udp1, Packet);
send_packet1([], Udp, _) ->
    Udp.

do_send_packet(Packet, DNSP, undefined) ->
    case open_udp(true) of
	undefined -> undefined;
	Udp       -> do_send_packet(Packet, DNSP, Udp)
    end;

do_send_packet(Packet, {DNS, Port}, Udp) ->
    case inet:getaddr(DNS,inet) of
	{ok, IP} ->
	    case gen_udp:send(Udp, IP, Port, Packet) of
		ok ->
		    Udp;
		{error, Reason} ->
		    gen_udp:close(Udp),
		    ?DIST_ERROR(?F("~p: Couldn't send to DNS ~p:~p - ~p",
				   [?MODULE, DNS, Port, Reason])),
		    open_udp(true)
	    end;
	{error, Reason} ->
	    ?DIST_ERROR(?F("~p: Couldn't resolv name ~p Reason: ~p",
			   [?MODULE, DNS, Reason])),
	    Udp
    end.


%% encode an erlang ip to a list of bytes with length of the list 
%% as the first value.
encode_ip({A,B,C,D}) when A =< 16#FF,B =< 16#FF,C =< 16#FF,D =< 16#FF,
			  integer(A+B+C+D) -> %% + is a check for ints
    [4,A,B,C,D];
encode_ip({A,B,C,D,E,F,G,H}) when A =< 16#FF,B =< 16#FF,C =< 16#FF,D =< 16#FF,
				  E =< 16#FF,F =< 16#FF,G =< 16#FF,H =< 16#FF,
				  integer(A+B+C+D+E+F+G+H) -> %% + is a check for ints
    [8,A,B,C,D,E,F,G,H].

make_packet([OPCODE, IP | Rest]) ->
    %% Format:
    %% DNSHEADER %% se headerdef
    %% hstringlenbyte, hstring
    %% opcodelenbyte, opcodebyte
    %% iplenbyte, ipbytes{4,8}
    %% hostlenbyte, hostnmame
    %% felenbute, fe_float_string
    %% belenbyte, be_gloat_string
    %% 
    %% after this comes also the:
    %% signature lenbyte, sign
    [
     ?OUR_DNS_HEADER,
     length(?HSTRING),?HSTRING,
     ?OP_CODE_LEN, OPCODE,
     encode_ip(IP), %% len of ip + 4 or 8 bytes of ip addr
     case Rest of
	 [] -> [0,0,0]; %% host, fef bef
	 [Host | []] ->
	     [length(Host), Host, 0,0]; %% the two zeroes is empty fe be 
	 [Host,FE_F, BE_F] ->
	     FE_F_Str     = mfl(FE_F),
	     FE_F_Str_Len = length(FE_F_Str),
	     BE_F_Str     = mfl(BE_F),
	     BE_F_Str_Len = length(BE_F_Str),
	     [
	      length(Host), Host, 
	      FE_F_Str_Len, FE_F_Str, %% len, FE_F
	      BE_F_Str_Len, BE_F_Str %% len, BE_F
	     ]
     end].

mfl(0)                 -> "0.0";
mfl(F) when float(F)   -> float_to_list(F);
mfl(F) when integer(F) -> float_to_list(erlang:float(F)).

%%
%% Send up info for all ?STATIC IPs belonging to node,
%% which was the previous global load_server, i.e this
%% information has not been able to be told to the DNS server yet. 
%%
tell_dns_node_down(Udp, Db, Node, true, State) ->
    LoadInfo = lookup_all_static(Node, Db),
    FIP = fun(#load{ip = IP}) -> IP end,
    IPs = lists:map(FIP, LoadInfo),
    #state{dns_servers = DNSs} = State,
    tell_dns_ips(DNSs, IPs, Udp, ?IP_DOWN, [], State#state.cookie);
tell_dns_node_down(Udp, Db, Node, PID, State) when pid(PID) ->
    Udp;
tell_dns_node_down(Udp, _, _,undefined, _) ->
    ?INFO(?F("Warning, no master load_server",[])),
    Udp.

%%
%% Inform the DNS servers about broken FE IP addresses.
%% only send as if data is for a FE and we are the global load_server
%%
tell_dns_down(?STATIC, ?FE, Udp, Db, IP, State) when State#state.global == true ->
    #state{dns_servers = DNSs, cookie = Cookie} = State,
    tell_dns_ips(DNSs, [IP], Udp, ?IP_DOWN, [], Cookie);
tell_dns_down(_, _, Udp, _, _, _) -> Udp.

%%
%% Inform the DNS servers about new FE IP addresses.
%%
tell_dns_up(?STATIC, ?FE, Udp, Db, IP, Host, BEs, State) ->
%%  when  State#state.global == true ->
    #state{dns_servers = DNSs, cookie = Cookie} = State,
    tell_dns_ips(DNSs, [IP], Udp, ?IP_UP, [Host], Cookie);
tell_dns_up(_, _, Udp, _, _, _, _, _) ->  Udp.

tell_dns_ips(DNSs, IPs, UDP, OPcode, Extra, Cookie) ->
    F = fun(IP, Udp) ->
		Udp1 = send_packet(DNSs, Udp, [OPcode, IP | Extra], Cookie),
		Udp1
	end,
    UDP1 = lists:foldl(F, UDP, IPs).

%%----------------------------------------------------------------------
%%
%% Lookup all static frontends for a node or for all nodes.
%%
lookup_all_static(Db) ->
    lookup_all_static1(Db, ?MATCH('_', ?FE, '_')).

lookup_all_static(Node, Db) ->
    lookup_all_static1(Db, ?MATCH(Node, ?FE, '_')).

lookup_all_static1(Db, Match) ->
    ets:match_object(Db, Match#load{type = ?STATIC}).

%%
%% Get the load per node.
%% Only for test.
%%
node_load1() ->
    LoadInfo = lookup_all_static(?MODULE),
    F = fun(#load{node = Node, load = Load}) -> {Node, Load} end,
    lists:map(F, LoadInfo).

%%
%% Initiate the Eddie disk_log_handler in the error_logger.
%% This function will be moved to the misc (?) application !!
%%
set_system_error_logging() ->
    Hs = gen_event:which_handlers(error_logger),
    case {lists:member(error_logger_tty_h, Hs),
	  lists:member(error_logger_file_h, Hs)} of
	{false, false} ->
	    %% Delete the (possibly existing) unformating handler
	    error_logger:delete_report_handler(error_logger),
	    %% Add the simplest handler that directs reports not
	    %% belonging to this node to the correct node.
	    error_logger:simple_logger(),
	    ok;
	_ ->
	    ok
    end.

logfile() ->
    File =  atom_to_list(node()) ++ "_event.log",
    case application:get_env(log_dir) of
	{ok, Dir} -> filename:join(Dir, File);
	_         -> filename:join(".", File)
    end.

verbose() ->
    case application:get_env(verbose) of
	{ok, false} -> false;
	_           -> true
    end.

%%
%% Proportional to the relative diff. between Rj and MR.
%%
fraction(F, Rj, MR) ->    if
			      Rj =< MR -> F * (1 + ?PHI);
			      true     -> F * (1 - ?PHI)
			  end.

low_mark(F) when F < ?MIN_FRACTION_L -> ?MIN_FRACTION_L;
low_mark(F)                        -> F.

max(X,Y) when X>Y -> X;
max(_X,Y)           -> Y.


format_status(Db) ->
    LoadL = lookup_all_static1(Db, ?MATCH('_', '_', '_')),
    [{nodes,nodes,BeNodes}|_] = ets:lookup(Db,nodes),
    ?F("~n-- Load statistics for frontend and backend nodes:~n~n",[]) ++
	?F("-- Load server is running on node ~s~n",[node()]) ++
	lists:foldl(fun(L,String) ->
			    case L#load.class of
				?FE ->
				    ?F("  -- ~s ~s~n"
				       "     Load                  ~.4f~n"	
				       "     Fe Mulitplier         ~.4f~n"
				       "     Be Mulitplier         ~.4f~n",
				       [L#load.node,
					?IP2STR(L#load.ip),
					float(L#load.load),
					float(fe_factor(L#load.'R',L#load.load)),
					float(be_factor(uplist(), Db))])
					++String;
				?BE ->
				    {value, {_, Fraction}} = lists:keysearch(L#load.node,1,BeNodes),
				    ?F("  -- ~s ~s~n"
				       "     Load                  ~.4f~n"	
				       "     Fraction of traffic   ~.4f~n",	
				       [L#load.node,
					?IP2STR(L#load.ip),
					float(L#load.load),
					float(Fraction)])
					++String
			    end
		    end,[],lists:sort(LoadL)).

%%% ------------------------EOF-----------------------------------------

