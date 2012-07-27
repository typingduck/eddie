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
%%% File    : dns_catalog.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : The DNS catalog owner.
%%% Created : 10 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_catalog).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/0, zone/1, root_hints/1, all_root_hints/0, load_function/1,
	 netmasks/0, options/0, xfrnets/0, forwarders/0,
	 prefs/0, cookie/0, logfile/0, verbose/0,
	 lb_port/0, lb_ttl/0,
	 init_ip/1, own_ip/0, next_id/0, zone_db/2,
	 init_traverse_zones/0, traverse_zones/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {db}).

-record(zone, {type,
	       info,
	       dir,        % Reference to the directory name.
	       bm = false, % Bitmap (false or string if some uppercase)
	       db}).

-include("dns.hrl").
-include_lib("../../misc/include/logger.hrl").

-define(LB_TTL, 120).  % Default TTL for load balanced domains; 2 minutes.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init_traverse_zones() ->
    ets:first(?MODULE).

traverse_zones({zone, Zone}) ->
    Key = {zone, Zone},
    [{_, #zone{type = Type, info = Info, bm = BM, dir = DirNo}}] =
	ets:lookup(?MODULE, Key),
    NKey = ets:next(?MODULE, Key),
    Dir = decode_dir(?MODULE, DirNo),
    {NKey, {Type, Zone, Info, BM, Dir}};
traverse_zones('$end_of_table') ->
    false;
traverse_zones(Key) ->
    NKey = ets:next(?MODULE, Key),
    traverse_zones(NKey).

zone(Name) -> 
    case ets:lookup(?MODULE, {zone, Name}) of
	[{_, #zone{db = Db}}] -> {ok, Db};
	_                     -> {error, nozone}
    end.

root_hints(Zone) -> 
    case ets:lookup(?MODULE, {root_hints, Zone}) of
	[{_, NS, Addr}] -> {ok, NS, Addr};
	_               -> {error, nozone}
    end.

all_root_hints() ->
    ets:match_object(?MODULE, {root_hints, '_'}).

load_function(Domain) -> 
    case ets:lookup(?MODULE, {load_balance, Domain}) of
	[{_, Function}] ->
	    {ok, Function};
	_ ->
	    [{_, RegLB}] = ets:lookup(?MODULE, reg_load_balance),
	    case reg_match(RegLB, Domain) of
		{ok, RegDomain} ->
		    [{_, Function}] =
			ets:lookup(?MODULE, {load_balance, RegDomain}),
		    {ok, Function};
		_ ->
		    {error, nofunction}
	    end
    end.

cookie() ->
    case ets:lookup(?MODULE, cookie) of
	[{_, Cookie}] ->
	    {ok, Cookie};
	_ ->
	    {ok, []}
    end.
   

prefs() ->
    case ets:lookup(?MODULE, prefs) of
	[{_, Prefs}] ->
	    {ok, Prefs};
	_ ->
	    {ok, []}
    end.

    
netmasks() ->
    case ets:lookup(?MODULE, netmasks) of
	[{_, NetMasks}] ->
	    {ok, NetMasks};
	_ ->
	    {error, none}
    end.

options() ->
    case ets:lookup(?MODULE, options) of
	[{_, Options}] -> ?CREATE_OPTS(Options);
	_              -> ?CREATE_OPTS([])
    end.

xfrnets() ->
    case ets:lookup(?MODULE, xfrnets) of
	[{_, XfrNets}] -> XfrNets;
	_              -> []
    end.

forwarders() ->
    case ets:lookup(?MODULE, forwarders) of
	[{_, Forwarders}] -> Forwarders;
	_                 -> []
    end.

lb_port() ->
    case ets:lookup(?MODULE, lb_port) of
	[{_, Port}] -> {ok, Port};
	_           -> undefined
    end.

lb_ttl() ->
    case ets:lookup(?MODULE, lb_ttl) of
	[{_, TTL}] -> {ok, TTL};
	_          -> {ok, ?LB_TTL}
    end.

own_ip() -> 
    case ets:lookup(?MODULE, ip) of
	[{_, IP}] -> {ok, IP};
	_         -> {error, none}
    end.

init_ip(IP) -> gen_server:call(?MODULE, {init_ip, IP}).

next_id() -> gen_server:call(?MODULE, next_id).

zone_db(Zone, Db) -> gen_server:call(?MODULE, {zone_db, Zone, Db}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    Db = ets:new(?MODULE, [named_table]),
    set_system_error_logging(),
    case fill_db(Db) of
	ok ->
	    insert_lb_info(Db),
	    ets:insert(Db, {query_id, 0}),
	    {ok, #state{db = Db}};
	Error ->
	    delete_system_error_logging(),
	    {stop, Error}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call({init_ip, IP}, From, State) ->
    ets:insert(State#state.db, {ip, IP}),
    {reply, ok, State};

handle_call(next_id, From, State) ->
    Db = State#state.db,
    I = ets:update_counter(Db, query_id, 1),
    Next = if
	       I > 16#ffff ->
		   ets:insert(Db, {query_id, 1}),
		   1;
	       true ->
		   I
	   end,
    {reply, {ok, Next}, State};

handle_call({zone_db, Zone, ZoneDb}, From, State) ->
    Db = State#state.db,
    Key = {zone, Zone},
    [{_, ZoneD}] = ets:lookup(Db, Key),
    ets:insert(Db, {Key, ZoneD#zone{db = ZoneDb}}),
    {reply, ok, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    delete_system_error_logging(),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

fill_db(Db) ->
    BootFile = bootfile(),
    io:fwrite("Got boot file ~p~n", [BootFile]),
    case dns_parse:boot(BootFile) of
	{ok, Pid} ->
	    fill_db(Db, Pid);
	Error ->
	    ?ERROR(?F("~s: Parse error - ~p", [BootFile, Error])),
	    Error
    end.

%% Insert {{zone, Zone}, #zone{}}
%%      where #zone.db will be filled in later.
%% Insert {{root_hints, Zone}, NS, Addr}
fill_db(Db, Pid) ->
    ets:insert(Db, {dir_no, 0}),
    fill_db(Pid, Db, ".", []).

fill_db(Pid, Db, Dir, Cache) ->
    case dns_parse:parse_next(Pid) of
	{ok, eof} ->
	    insert_root_hints(Db, Cache),
	    ok;
	{ok, Item} ->
	    fill_db(Item, Pid, Db, Dir, Cache);
	Error ->
	    Error
    end.

fill_db({primary, Zone, File}, Pid, Db, Dir, Cache) ->
    D = encode_dir(Db, Dir),
    {LowZone, BM} = tolower(Zone),
    ets:insert(Db, {{zone,LowZone}, #zone{type = primary,
					  info = File,
					  bm = BM,
					  dir = D}}),
    fill_db(Pid, Db, Dir, Cache);
fill_db({secondary, Zone, Addrs, ""}, Pid, Db, Dir, Cache) ->
    {LowZone, BM} = tolower(Zone),
    ets:insert(Db, {{zone,LowZone}, #zone{type = secondary,
					  bm = BM,
					  info = {Addrs, false}}}),
    fill_db(Pid, Db, Dir, Cache);
fill_db({secondary, Zone, Addrs, Backup}, Pid, Db, Dir, Cache) ->
    D = encode_dir(Db, Dir),
    {LowZone, BM} = tolower(Zone),
    ets:insert(Db, {{zone,LowZone}, #zone{type = secondary,
					  info = {Addrs, Backup},
					  bm = BM,
					  dir = D}}),
    fill_db(Pid, Db, Dir, Cache);
fill_db({cache, Zone, File}, Pid, Db, Dir, Cache) ->
    {LowZone, _} = tolower(Zone),
    fill_db(Pid, Db, Dir, [{LowZone, filename(Dir, File)}|Cache]);
fill_db({directory, Dir}, Pid, Db, _, Cache) ->
    fill_db(Pid, Db, Dir, Cache);
fill_db({options, Options}, Pid, Db, Dir, Cache) ->
    case ets:lookup(Db, options) of
	[{_, Os}] ->
	    ets:insert(Db, {options, Options ++ Os}),
	    fill_db(Pid, Db, Dir, Cache);
	_ ->
	    ets:insert(Db, {options, Options}),
	    fill_db(Pid, Db, Dir, Cache)
    end;
fill_db({xfrnets, XfrNets}, Pid, Db, Dir, Cache) ->
    case ets:lookup(Db, xfrnets) of
	[{_, Xs}] ->
	    ets:insert(Db, {xfrnets, XfrNets ++ Xs}),
	    fill_db(Pid, Db, Dir, Cache);
	_ ->
	    ets:insert(Db, {xfrnets, XfrNets}),
	    fill_db(Pid, Db, Dir, Cache)
    end;
fill_db({forwarders, Forwarders}, Pid, Db, Dir, Cache) ->
    case ets:lookup(Db, forwarders) of
	[{_, Fs}] ->
	    ets:insert(Db, {forwarders, Forwarders ++ Fs}),
	    fill_db(Pid, Db, Dir, Cache);
	_ ->
	    ets:insert(Db, {forwarders, Forwarders}),
	    fill_db(Pid, Db, Dir, Cache)
    end.

tolower(Zone) ->
    case tolower1(Zone) of
	Zone    -> {Zone, false};
	LowZone -> {LowZone, Zone}
    end.

tolower1([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | tolower1(Cs)];
tolower1([C|Cs])                       -> [C | tolower1(Cs)];
tolower1([])                           -> [].

bootfile() ->
    case application:get_env(boot) of
	{ok, File} -> File;
	_          -> "named.boot"
    end.

%% Encode the Directory string, i.e. store the directory at one
%% place and only the reference for each zone (e.g. if 10000 zones
%% exists using the same directory it's stupid to store the Directory
%% 10000 times !!
encode_dir(Db, Dir) ->
    case ets:match(Db, {dir, '$1', Dir}) of
	[[No]] ->
	    No;
	_ ->
	    No = ets:update_counter(Db, dir_no, 1),
	    ets:insert(Db, {dir, No, Dir}),
	    No
    end.

%% Lookup an encoded directory number.
decode_dir(Db, No) ->
    case ets:match(Db, {dir, No, '$1'}) of
	[[Dir]] -> Dir;
	_       -> undefined
    end.

%% Insert all root hints.
insert_root_hints(Db, Cache) ->
    Data = gather_data(Cache),
    insert_cache_zones(Data, Db).

insert_cache_zones([{Zone, RRs}|Data], Db) ->
    {NS, Addr} = split_ns_addr(RRs),
    ets:insert(Db, {{root_hints, Zone}, NS, Addr}),
    insert_cache_zones(Data, Db);
insert_cache_zones([], _) ->
    ok.

split_ns_addr(RRs) -> split_ns_addr(RRs, [], []).
split_ns_addr([RR|RRs], NS, Addr) when RR#dns_rr.type == ?S_NS ->
    split_ns_addr(RRs, [RR|NS], Addr);
split_ns_addr([RR|RRs], NS, Addr) when RR#dns_rr.type == ?S_A ->
    split_ns_addr(RRs, NS, [RR|Addr]);
split_ns_addr([_|RRs], NS, Addr) ->
    split_ns_addr(RRs, NS, Addr);
split_ns_addr([], NS, Addr) ->
    {lists:reverse(NS), Addr}.

gather_data([{Zone, FileName}|Cache]) ->
    case parse_dns_master(FileName) of
	{ok, RRs} ->
	    [{Zone, decode_rrs(RRs, Zone, FileName)} | gather_data(Cache)];
	Error ->
	    ?INFO(?F("~s: cache directive format error - ~p",
		     [FileName, Error])),
	    gather_data(Cache)
    end;
gather_data([]) ->
    [].

%% We count on "small" cache files, thus we can keep the complete
%% parse info in a list.
parse_dns_master(File) ->
    case dns_parse:master(File) of
	{ok, Pid} ->
	    case catch parse_dns_master1(Pid) of
		{throwed, Error} -> Error;
		Res              -> {ok, Res}
	    end;
	Error ->
	    Error
    end.

parse_dns_master1(Pid) ->
    case dns_parse:parse_next(Pid) of
	{ok, eof}  -> [];
	{ok, Item} -> [Item|parse_dns_master1(Pid)];
	Error      -> throw({throwed, Error})
    end.

decode_rrs(RRs, Zone, FileName) ->
    decode_rrs(RRs, dns_rr:cr_default(Zone), Zone, FileName).

decode_rrs([{rr, RR}|RRs], Default, Zone, FileName) ->
    {ok, RRrec, NDef} = dns_rr:decode(RR, Default, Zone),
    [RRrec | decode_rrs(RRs, NDef, Zone, FileName)];
decode_rrs([H|RRs], Default, Zone, FileName) ->
    ?INFO(?F("~s: ignoring - ~p", [FileName, H])),
    decode_rrs(RRs, Default, Zone);
decode_rrs([], _, _, _) ->
    [].

%%
%% Insert information gathered from the load balance configuration
%% file.
%% Information about domain names subject for load balancing and
%% netmasks for IP addresses allowed to send load information
%% about IP addresses.
%%
%% File syntax:
%%          # comment
%%          Netmask IP/Bits [IP/Bits]
%%          ...
%%          Netmask IP/Bits [IP/Bits]
%%          Domain DomainName [DomainName]
%%          ...
%%          Domain DomainName [DomainName]
%%
%%          Prefer OrigIPNetmask Domain PrefLan1Netmask Pre2Lan2Netmask...3,4..
%%                                ^^^^^^<- only domainnames, no wildcards..
%%         
%%   where
%%      Bits is the number of bits (MSB) used as netmask and
%%      Bits = h | H is equivalent with 32.
%%      DomainName = x.y.z where * can be used as wildcard, e.g.
%%      x*y.*.se expands to the regular expression x[^.]*y.[^.]+.se !
%%
insert_lb_info(Db) ->
    case get_lb_file() of
	{ok, File} ->
	    case parse_lb_file(File) of
		{ok, Contents} ->
		    store_lb(Contents, [], [], [],File, Db);
		Error ->
		    ?FATAL(?F("~s: Parse error - ~p", [File, Error])),
		    exit(lb_error)
	    end;
	_ ->
	    store_lb([], [], [], [], "nofile", Db)
    end.
	
parse_lb_file(File) ->
    config_file:parse(File).

get_lb_file() ->
    %% Optional
    case application:get_env(lb_boot) of
	{ok, LB_file} -> {ok, LB_file};
	_             -> false
    end.

store_lb([{Line, "cookie", [Cookie]}|T], RLAck, NAck, PAck,F, Db) ->
    ets:insert(Db, {cookie, Cookie}),
    store_lb(T, RLAck, NAck, PAck, F, Db);

store_lb([{Line, "prefer", [OrigIPMask, Domain | PrefLanMasks]}|T],
	 RLAck, NAck, PAck,F, Db) ->
    OrigIPMask2 = valid_netmasks(Line, [OrigIPMask], F),
    PrefLanMasks2 = valid_netmasks(Line, PrefLanMasks, F),
    NewPAck = case {OrigIPMask2, PrefLanMasks2} of
		  {[_|_],[_|_]} -> [{hd(OrigIPMask2) , Domain, PrefLanMasks2}|PAck];
		  _ -> PAck
	      end,
    store_lb(T, RLAck, NAck, NewPAck, F, Db);

store_lb([{Line, "port", [Port]}|T], RLAck, NAck, PAck,F, Db) ->
    case catch list_to_integer(Port) of
	P when integer(P) ->
	    ets:insert(Db, {lb_port, P}),
	    store_lb(T, RLAck, NAck, PAck, F, Db);
	_ ->
	    ?INFO(?F("~s:~p: ignoring Port - ~p",
		     [F, Line, Port])),
	    store_lb(T, RLAck, NAck,PAck, F, Db)
    end;
store_lb([{Line, "ttl", [TTL0]}|T], RLAck, NAck, PAck, F, Db) ->
    case catch list_to_integer(TTL0) of
	TTL when integer(TTL) ->
	    ets:insert(Db, {lb_ttl, TTL}),
	    store_lb(T, RLAck, NAck, PAck, F, Db);
	_ ->
	    ?INFO(?F("~s:~p: ignoring TTL - ~p",
		     [F, Line, TTL0])),
	    store_lb(T, RLAck, NAck, PAck, F, Db)
    end;
store_lb([{Line, "netmask", Netmasks}|T], RLAck, NAck, PAck, F, Db) ->
    Masks = valid_netmasks(Line, Netmasks, F),
    store_lb(T, RLAck, Masks ++ NAck, PAck,F, Db);
store_lb([{Line, "domain", Domains}|T], RLAck, NAck, PAck, F, Db) ->
    RegLBs = insert_load_domains(Line, Domains, F, Db),
    store_lb(T, RegLBs ++ RLAck, NAck, PAck, F, Db);
store_lb([H|T], RLAck, NAck, PAck,F, Db) ->
    Line = element(1, H),
    ?INFO(?F("~s:~p: ignoring ~p",[F, Line, element(2, H)])),
    store_lb(T, RLAck, NAck, PAck, F, Db);
store_lb([], RLAck, NAck, PAck, F, Db) ->
    ets:insert(Db, {reg_load_balance, RLAck}),
    ets:insert(Db, {netmasks, NAck}),
    ets:insert(Db, {prefs, PAck}),
    ok.

%%
%% Insert load balanced domain names.
%% Return domain names expressed using regular expressions;
%% they will be inserted later.
%%
insert_load_domains(Line, Domains, Fname, Db) ->
    Fn = fun(Domain) when list(Domain) ->
		 case expand_domain(Domain) of
		     Domain ->
			 ets:insert(Db, {{load_balance, Domain},
					 {dns_load, load_balance}}),
			 [Domain];
		     RegDomain ->
			 ets:insert(Db, {{load_balance, RegDomain},
					 {dns_load, load_balance}}),
			 [{regular, RegDomain}]
		 end;
	    (Other) ->
		 ?INFO(?F("~s:~p: ignoring ~p", [Fname, Line, Other])),
		 []
	 end,
    DoLoadBalance = lists:flatmap(Fn, Domains),
    split_reg(DoLoadBalance).

split_reg([{regular, RegDomain}|T]) -> [RegDomain|split_reg(T)];
split_reg([_|T])                    -> split_reg(T);
split_reg([])                       -> [].

%%
%% Keep only valid netmasks.
%%
valid_netmasks(Line, NetMasks, Fname) ->
    Fn = fun(NetMask) when list(NetMask) ->
		 case parse_netmask(NetMask) of
		     {ok, Mask} ->
			 [Mask];
		     Error ->
			 ?INFO(?F("~s:~p: ignoring ~p - ~p",
				  [Fname, Line, NetMask, Error])),
			 []
		 end;
	    (Other) ->
		 ?INFO(?F("~s:~p: ignoring ~p", [Fname, Line, Other])),
		 []
	 end,
    lists:flatmap(Fn, NetMasks).

parse_netmask(NetMask) ->
    case string:tokens(NetMask, [$/]) of
	[NetA, Bits] ->
	    parse_netmask(NetA, Bits);
	_ ->
	    {error, "missing /"}
    end.

parse_netmask(NetA, Bits) ->
    case inet_parse:address(NetA) of
	{ok, NetIP} ->
	    case make_mask(Bits) of
		{ok, NetMask} ->
		    {ok, {NetIP, NetMask}};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

%% Make a netmask for IPv4
make_mask("h") -> make_mask("32");
make_mask("H") -> make_mask("32");
make_mask(Bits) ->
    case catch list_to_integer(Bits) of
	N when integer(N), N =< 32, N >= 0 ->
	    I = make_i(N, 0),
	    {ok, {(I bsr 24) band 16#ff,
		  (I bsr 16) band 16#ff,
		  (I bsr 8) band 16#ff,
		  I band 16#ff}};
        _ ->
	    {error, "bits integer error"}
    end.

make_i(0, Ack) ->
    Ack;
make_i(N, Ack) ->
    Ack1 = (1 bsl (32 - N)) bor Ack,
    make_i(N - 1, Ack1).

%%
%% Append File to Dir if not an absolute filename.
%%    
filename(Dir, File) ->
    case filename:pathtype(File) of
	absolute -> File;
	_        -> filename:join(Dir, File)
    end.


%%% ----------------------------------------------------
%%% Expand a domain name given with wildcards (*) 
%%% to a domain name containg regular expressions.
%%% The only handled wildcard is '*' which is translated
%%% into the regular expression [^.]* .
%%% If '*' is given as only character between two '.'
%%% it is instead translated into the regular expression
%%% [^.]+ , i.e. where must be at least one character 
%%% between two '.'.
%%%
%%% Returns: RegDomain
%%% ----------------------------------------------------

expand_domain(Domain) ->
    Toks = string:tokens(Domain, [$.]),
    ExpToks = expand_tokens(Toks),
    reg_domain(ExpToks, false).

reg_domain([Tok|Toks], false) ->
    Tok ++ reg_domain(Toks, true);
reg_domain([Tok|Toks], F) ->
    [$.|Tok] ++ reg_domain(Toks, F);
reg_domain([], _) ->
    [].

%%
%% expand_tokens([Name]) -> [Name']
%%
%% Expand "*" ==> "[^.]+"
%%        "...*..." ==> "[^.]*"
%%
%% A single .../*/... is expanded to one or more whatever
%% except a '.' because it is a place holder for a domain name.
%%
expand_tokens(Names) ->
    lists:map(fun("*") ->
		      "[^.]+";
		 (N) ->
		      case lists:member($*, N) of
			  true -> expand(N, []);
			  _    -> N
		      end
	      end, Names).

expand([$*|T], Ack) ->
    expand(T, "*].^[" ++ Ack);  %% "[^.]*"
expand([H|T], Ack) ->
    expand(T, [H|Ack]);
expand([], Ack) ->
    lists:reverse(Ack).

%%
%% Find a matching regular expression.
%%
reg_match([], _) ->
    false;
reg_match(RegLB, Domain) ->
    reg_match(RegLB, Domain, length(Domain)).

reg_match([RegD|T], Domain, Len) ->
    case regexp:match(Domain, RegD) of
	{match,1,Len} ->
	    {ok, RegD};
	_ ->
	    reg_match(T, Domain, Len)
    end;
reg_match([], _, _) ->
    false.

%%
%% Initiate the Eddie disk_log_handler in the error_logger.
%% This function will be moved to the misc (?) application !!
%%
set_system_error_logging() ->
%    LogFile = logfile(),
%    Verbose = verbose(),
%    error_logger:add_report_handler(disk_log_handler, [LogFile, Verbose]),
    Hs = gen_event:which_handlers(error_logger),
    case {lists:member(error_logger_tty_h, Hs),
	  lists:member(error_logger_file_h, Hs)} of
	{false, false} ->
	    %% Delete the unformating handler
	    error_logger:delete_report_handler(error_logger),
	    %% Add the simplest handler that directs reports not
	    %% belonging to this node to the correct node.
	    error_logger:simple_logger(),
	    ok;
	_ ->
	    ok
    end.

logfile() ->
    File = "dns.log",
    case application:get_env(log_dir) of
	{ok, Dir} -> filename:join(Dir, File);
	_         -> filename:join(".", File)
    end.

verbose() ->
    case application:get_env(verbose) of
	{ok, false} -> false;
	_           -> true   % default
    end.

%%
%% Delete the Eddie syslog_handler in the error_logger.
%% This function will be moved to the misc application !!
%%
delete_system_error_logging() ->
    error_logger:delete_report_handler(disk_log_handler),
    ok.

%%   Not needed, just prints uninformative messages for the 
%%   DNS user, run debug mode 2 instead to see all info !!!
%    Hs = gen_event:which_handlers(error_logger),
%    case {lists:member(error_logger_tty_h, Hs),
%	  lists:member(error_logger_file_h, Hs)} of
%	{false, false} ->
%	    error_logger:delete_report_handler(error_logger),
%	    %% Add the unformating handler.
%	    error_logger:simple_logger(10),
%	    ok;
%	_ ->
%	    ok
%    end.

