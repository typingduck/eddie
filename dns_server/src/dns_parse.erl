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

%%% File    : dns_parse.erl
%%% Author  : Magnus Fröberg <magnus@erix.ericsson.se>
%%% Purpose : Parser for DNS (BIND) configuration files.
%%% Created : 12 Apr 1998 by Magnus Fröberg <magnus@erix.ericsson.se>

-module(dns_parse).
-author('magnus@erix.ericsson.se').

-export([boot/1, master/1, parse_next/1]).

%% Internal usage only !
-export([boot/2, master/2]). 

-import(lists, [reverse/1]).

-include_lib("misc/include/logger.hrl").

%% --------------------------------------------------------------------------
%% boot/1 and master/1 returns {ok Pid}.
%% Use parse_next(Pid) in order to parse one item per call.
%% --------------------------------------------------------------------------

parse_next(Pid) ->
    Pid ! {get_next, self()},
    receive
	{Pid, eof} ->
	    {ok, eof};
	{Pid, next, Val} ->
	    {ok, Val};
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    end.

%% --------------------------------------------------------------------------
%% Parse named.boot file.
%% Returns: {ok, Pid} | {error, Reason}
%% Syntax:
%%      directory Dir [comment] \n
%%      Type   Domain    SourceFile   [BackupFile] [comment] \n
%%      ; [comment] \n
%% --------------------------------------------------------------------------

boot(File) ->
    Pid = spawn_link(?MODULE, boot, [File, self()]),
    receive
	{Pid, ok} -> {ok, Pid};
	{Pid, Error} -> Error;
	{'EXIT', Pid, Reason} -> {error, Reason}
    end.

boot(File, Parent) ->
    Fn = fun(["primary", Domain, SrcF | T]) ->
		 dns_domain(Domain),
		 dns_cmt(T),
		 {primary, Domain, SrcF};
	    (["cache", Domain, SrcF | T]) ->
		 dns_domain(Domain),
		 dns_cmt(T),
		 {cache, Domain, SrcF};
	    (["secondary", Domain | T]) ->
		 dns_domain(Domain),
		 case dns_sec_addrs(T) of
		     {[], _} ->
			 exit(secondary_address);
		     {Addrs, T1} ->
			 case catch dns_cmt(T1) of
			     true ->
				 {secondary, Domain, Addrs, ""};
			     _ ->
				 [BackupF | T2] = T1,
				 dns_cmt(T2),
				 {secondary, Domain, Addrs, BackupF}
			 end
		 end;
	    (["directory", Dir | T]) ->
		 dns_cmt(T),
		 {directory, Dir};
	    (["options", Opt | T]) ->
		 Options = get_options([Opt|T], false),
		 {options, Options};
	    (["slave" | T]) ->
		 dns_cmt(T),
		 {options, [forward]};
	    (["forwarders", Forws | T]) ->
		 Fs = get_forwarders([Forws|T], false),
		 {forwarders, Fs};
	    (["xfrnets", Net | T]) ->
		 XfrNets = get_xfrnets([Net|T], false),
		 {xfrnets, XfrNets};
	    (Other) ->
		 dns_cmt(Other),
		 skip
	 end,
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Parent ! {self(), ok},
	    parse_one_at_a_time(File, Fd, Parent, Fn, 1);
	Error ->
	    Parent ! {self(), Error},
	    unlink(Parent),
	    exit(Error)
    end.

dns_cmt([])         -> true;
dns_cmt([[$;|_]|_]) -> true;
dns_cmt(_)          -> exit(fmt).

dns_domain(Domain) ->
    case domain(Domain) of
	true ->
	    true;
	_ ->
	    case absolute_domain(Domain) of
		true -> true;
		_    ->  wildcard_domain(Domain)
	    end
    end.

absolute_domain(".") ->
    true;
absolute_domain(Domain) ->
    case lists:last(Domain) of
	$. -> domain(lists:sublist(Domain, length(Domain) - 1));
	_  -> false
    end.

wildcard_domain("*") ->
    true;
wildcard_domain([$*, $. | Domain]) ->
    case domain(Domain) of
	true ->
	    true;
	_ ->
	    case absolute_domain(Domain) of
		true -> true;
		_    ->  exit(nodomain)
	    end
    end;
wildcard_domain(_) ->
    exit(nodomain).

dns_sec_addrs(L) -> dns_sec_addrs(L, []).

dns_sec_addrs([H|T], Ack) ->
    case inet_parse:address(H) of
	{ok, IP} ->
	    dns_sec_addrs(T, [IP|Ack]);
	_ ->
	    {lists:reverse(Ack), [H|T]}
    end;
dns_sec_addrs([], Ack) ->
    {lists:reverse(Ack), []}.

%%
%% Get a list of options.
%% Exit if non legal option.
%%
get_options([Option|Os], Found) ->
    case option(tolower(Option)) of
	{ok, Opt} ->
	    [Opt | get_options(Os, true)];
	_ when Found == true ->
	    dns_cmt([Option|Os]),
	    [];
	_ ->
	    exit(fmt)
    end;
get_options([], _) ->
    [].

option("no-recursion")  -> {ok, norec};
option("forward-only")  -> {ok, forward};
option("no-fetch-glue") -> {ok, noglue};
option("query-log")     -> {ok, qlog};
option("fake-iquery")   -> {ok, fiq};
option(_)               -> false.

%%
%% Get a list of forwarders.
%% Exit if non legal parameter.
%%
get_forwarders([ForwIP|T], Found) ->
    case inet_parse:address(ForwIP) of
	{ok, FIP} ->
	    [FIP|get_forwarders(T, true)];
	_ when Found == true ->
	    dns_cmt([ForwIP|T]),
	    [];
	_ ->
	    exit(fmt)
    end;
get_forwarders([], _) ->
    [].

%%
%% Get a list of networks allowed to do zone transfer.
%% Exit if non legal net.
%%
get_xfrnets([Net|T], Found) ->
    case string:tokens(Net, [$&]) of
	[NetIP] ->
	    case parse_netmask(NetIP) of
		false when Found == true ->
		    dns_cmt([Net|T]),
		    [];
		false ->
		    exit(fmt);
		Mask ->
		    [Mask|get_xfrnets(T, true)]
	    end;
	[NetIP, NetM] ->
	    case parse_netmask(NetIP, NetM) of
		false when Found == true ->
		    dns_cmt([Net|T]),
		    [];
		false ->
		    exit(fmt);
		Mask ->
		    [Mask|get_xfrnets(T, true)]
	    end;
	_ when Found == true ->
	    dns_cmt([Net|T]),
	    [];
	_ ->
	    exit(fmt)
    end;
get_xfrnets([], _) ->
    [].

parse_netmask(NetIP) ->
    %% Use default mask.
    case inet_parse:address(NetIP) of
	{ok, NIP} ->
	    NM = cr_netmask(NIP),
	    {NIP, NM};
	_ ->
	    false
    end.

cr_netmask({_,0,0,0}) -> {255,0,0,0};
cr_netmask({_,_,0,0}) -> {255,255,0,0};
cr_netmask({_,_,_,0}) -> {255,255,255,0};
cr_netmask({_,_,_,_}) -> {255,255,255,255}.

parse_netmask(NetIP, NetM) ->
    case inet_parse:address(NetIP) of
	{ok, NIP} ->
	    case inet_parse:address(NetM) of
		{ok, NM} ->
		    {NIP, NM};
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%% --------------------------------------------------------------------------
%% Parse DNS master file.
%% Returns: {ok, Pid} | {error, Reason}
%% Syntax:
%%      $ORIGIN Domain [comment] \n
%%      $INCLUDE Filename [Domain] [comment] \n
%%      Domain 'RR' [comment] \n
%%      'RR' [comment] \n
%%      ; comment \n
%%
%%   where:
%%     'RR' = [TTL] [Class] Type RData
%%            [Class] [TTL] Type RData
%% --------------------------------------------------------------------------

master(File) ->
    Pid = spawn_link(?MODULE, master, [File, self()]),
    receive
	{Pid, ok} -> {ok, Pid};
	{Pid, Error} -> Error;
	{'EXIT', Pid, Reason} -> {error, Reason}
    end.

master(File, Parent) ->
    %% erase(dns_data),
    %% erase(dns_left),
    Fn = fun([";"|_]) ->
		 skip;
	    (["$ORIGIN", Domain | T]) ->
		 dns_domain(Domain),
		 {origin, Domain};
	    (["$INCLUDE", FileN | T]) ->
		 case catch dns_cmt(T) of
		     true ->
			 {include, FileN};
		     _ ->
			 case T of
			     [Domain | T1] ->
				 dns_domain(Domain),
				 dns_cmt(T1),
				 {include, FileN, Domain};
			     _ ->
				 exit(fmt)
			 end
		 end;
	    (Other) ->
		 dns_parse_other(Other)
	 end,
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Parent ! {self(), ok},
	    parse_one_at_a_time(File, Fd, Parent, Fn, 1);
	Error ->
	    Parent ! {self(), Error},
	    unlink(Parent),
	    exit(Error)
    end.

%% 
%% This is really some ugly code. Uses the dictionary in order to
%% keep data until a matching ")" is encountered for a previous
%% "(".
%% 
dns_parse_other(Other) ->
    case get(dns_left) of
	I when integer(I) ->
	    case match_right(Other, []) of
		{true, D} ->
		    case sub_left() of
			0 ->
			    Data = get(dns_data),
			    erase(dns_data),
			    erase(dns_left),
			    clear_blank(),
			    dns_rr(Data ++ D);
			_ ->
			    Data = skip_cmt(D),
			    keep_data(Data),
			    skip
		    end;
		_ ->
		    Data = skip_cmt(Other),
		    keep_data(Data),
		    skip
	    end;
	_ ->
	    dns_rr(Other)
    end.

dns_rr(Data) ->
    case catch dns_parse_rr(Data) of
	left_paren ->
	    D = strip_left(Data),
	    case match_right(D, []) of
		{true, D1} ->
		    dns_rr(D1);
		_ ->
		    add_left(),
		    keep_data(D),
		    skip
	    end;
	{'EXIT', Reason} ->
	    exit(Reason);
	RR ->
	    {rr, RR}
    end.

%% Match and delete ")".
match_right([")"|T], Ack) -> {true, lists:reverse(Ack) ++ T};
match_right([H|T], Ack)   -> match_right(T, [H|Ack]);
match_right([], Ack)      -> false.

add_left() ->
    case get(dns_left) of
	undefined -> put(dns_left, 1);
	I         -> put(dns_left, I + 1)
    end.

sub_left() ->
    I = get(dns_left) - 1,
    put(dns_left, I),
    I.

strip_left(["("|Data]) -> skip_cmt(Data);
strip_left([H|T])      -> [H|strip_left(T)];
strip_left([])         -> [].

keep_data(Data) ->
    case get(dns_data) of
	undefined -> put(dns_data, Data);
	D         -> put(dns_data, D ++ Data)
    end.

skip_cmt([[$;|_]|T]) -> [];
skip_cmt([H|T])      -> [H|skip_cmt(T)];
skip_cmt([])         -> [].

%%
%% Parse a DNS RR.
%%
dns_parse_rr(["@"|T]) ->
    [{domain, "@"} | dns_parse_type(T, ttl_class_type)];
dns_parse_rr([H|T]) ->
    case get(blank) of
	true ->
	    dns_parse_type([H|T], ttl_class_type);
	_ ->
	    case catch dns_domain(H) of
		true -> [{domain, H} | dns_parse_type(T, ttl_class_type)];
		_    -> exit(no_domain)
	    end
    end.

dns_parse_type([H|T], ttl_class_type) ->
    is_left(H),
    case catch list_to_integer(H) of
	I when integer(I) ->
	    [{ttl, I} | dns_parse_type(T, class_type)];
	_ ->
	    case dns_class_p(H) of
		true -> [{class, tolowatom(H)} | dns_parse_type(T, ttl_type)];
		_    -> dns_parse_type([H|T], type)
	    end
    end;
dns_parse_type([H|T], class_type) ->
    is_left(H),
    case dns_class_p(H) of
	true -> [{class, tolowatom(H)} | dns_parse_type(T, type)];
	_    -> dns_parse_type([H|T], type)
    end;
dns_parse_type([H|T], ttl_type) ->
    is_left(H),
    case catch list_to_integer(H) of
	I when integer(I) -> [{ttl, I} | dns_parse_type(T, type)];
	_                 -> dns_parse_type([H|T], type)
    end;
dns_parse_type([H|T], type) ->
    is_left(H),
    case dns_type_p(H) of
	true -> [{type, tolowatom(H)} | dns_rdata(T)];
	_    -> exit(notype)
    end;
dns_parse_type(_, _) ->
    exit(fmt).

tolowatom(L) -> list_to_atom(tolower(L)).

tolower([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs])                       -> [C | tolower(Cs)];
tolower([])                           -> [].

dns_rdata(RD) ->
    case catch dns_cmt(RD) of
	true ->
	    exit(rdata);
	_    ->
	    [H|T] = RD,
	    is_left(H),
	    [{rdata, [H|dns_rdata1(T)]}]
    end.

dns_rdata1(RD) ->
    case catch dns_cmt(RD) of
	true ->
	    [];
	_ ->
	    [H|T] = RD,
	    is_left(H),
	    [H|dns_rdata1(T)]
    end.

dns_class_p(Class) -> lists:member(Class, ["in", "cs", "ch", "hs",
					   "IN", "CS", "CH", "HS"]).

is_left("(") -> throw(left_paren);
is_left(_)   -> false.

dns_type_p("a")     -> true;
dns_type_p("ns")    -> true;
dns_type_p("md")    -> true;
dns_type_p("mf")    -> true;
dns_type_p("cname") -> true;
dns_type_p("soa")   -> true;
dns_type_p("mb")    -> true;
dns_type_p("mg")    -> true;
dns_type_p("mr")    -> true;
dns_type_p("null")  -> true;
dns_type_p("wks")   -> true;
dns_type_p("ptr")   -> true;
dns_type_p("hinfo") -> true;
dns_type_p("minfo") -> true;
dns_type_p("mx")    -> true;
dns_type_p("srv")   -> true;
dns_type_p("txt")   -> true;
dns_type_p("A")     -> true;
dns_type_p("NS")    -> true;
dns_type_p("MD")    -> true;
dns_type_p("MF")    -> true;
dns_type_p("CNAME") -> true;
dns_type_p("SOA")   -> true;
dns_type_p("MB")    -> true;
dns_type_p("MG")    -> true;
dns_type_p("MR")    -> true;
dns_type_p("NULL")  -> true;
dns_type_p("WKS")   -> true;
dns_type_p("PTR")   -> true;
dns_type_p("HINFO") -> true;
dns_type_p("MINFO") -> true;
dns_type_p("MX")    -> true;
dns_type_p("SRV")   -> true;
dns_type_p("TXT")   -> true;
dns_type_p(_)       -> false.

%%
%% Check if a String is a domain name according.
%%
domain([H|T]) ->
    is_dom1([H|T]);
domain(_) ->
    false.

is_dom1([C | Cs]) when C >= $a, C =< $z -> is_dom_ldh(Cs);
is_dom1([C | Cs]) when C >= $A, C =< $Z -> is_dom_ldh(Cs);
is_dom1([C | Cs]) when C >= $0, C =< $9 -> is_dom_ldh(Cs);
is_dom1(_) -> false.

is_dom_ldh([C | Cs]) when C >= $a, C =< $z -> is_dom_ldh(Cs);
is_dom_ldh([C | Cs]) when C >= $A, C =< $Z -> is_dom_ldh(Cs);
is_dom_ldh([C | Cs]) when C >= $0, C =< $9 -> is_dom_ldh(Cs);
is_dom_ldh([$-,$. | _]) -> false;
is_dom_ldh([$_,$. | _]) -> false;
is_dom_ldh([$_ | Cs]) -> is_dom_ldh(Cs);
is_dom_ldh([$- | Cs]) -> is_dom_ldh(Cs);
is_dom_ldh([$. | Cs]) -> is_dom1(Cs);
is_dom_ldh([$\\, C | Cs]) when C >= $0, C =< $9 -> false;
is_dom_ldh([$\\, C | Cs]) -> is_dom1(Cs);
is_dom_ldh([]) -> true;
is_dom_ldh(_) -> false.

%%
%% Driven by the client, parse one item !
%%
parse_one_at_a_time(File, Fd, Parent, Fn, Line) ->
    receive
	{get_next, Parent} ->
	    Line1 = do_parse_next(File, Fd, Line, Fn, Parent),
	    parse_one_at_a_time(File, Fd, Parent, Fn, Line1)
    end.

do_parse_next(Fname, Fd, Line, Fun, Parent) ->
    case read_line(Fd) of
	eof ->
	    Parent ! {self(), eof},
	    unlink(Parent),
	    exit(shutdown);
	Cs ->
	    mark_blank(Cs),
	    case parse_line(Fname, Cs, Line, Fun) of
		{ok, Val} ->
		    Parent ! {self(), next, Val},
		    Line+1;
		_ -> % empty | skip | error
		    do_parse_next(Fname, Fd, Line+1, Fun, Parent)
	    end
    end.

%% Mark if a line starts with a blank; crucial.
mark_blank([$ |_])   -> put(blank, true);
mark_blank([$\t |_]) -> put(blank, true);
mark_blank(_)        -> erase(blank).

clear_blank() -> erase(blank).

%%
%% Parse a line.
%%
parse_line(Fname, Chars, Line, Fun) ->
    case split_line(Chars) of
	[] ->
	    empty;
	Toks ->
	    case catch Fun(Toks) of
		{'EXIT',_} -> 
		    ?ERROR(?F("~s:~p: erroneous line, SKIPPED",
			      [Fname,Line])),
		    error;
		{warning,Wlist,Val} ->
		    ?INFO(?F("~s:~p: warning! strange domain name(s) ~p",
			     [Fname,Line,Wlist])),
		    {ok, Val};
		skip ->
		    skip;
		Val ->
		    {ok, Val}
	    end
    end.

%%
%% Read a line
%%
read_line(Fd) ->
    collect_line(Fd, []).

collect_line(Fd, Cs) ->
    case file:read(Fd, 80) of
	{ok, Line} when binary(Line) ->
	    collect_line(Fd, size(Line), binary_to_list(Line), Cs);
	{ok, Line} ->
	    collect_line(Fd, length(Line), Line, Cs);
	eof when Cs == [] ->
	    eof;
	eof -> reverse(Cs)
    end.    

collect_line(Fd, N, [$\r, $\n|_], Cs) ->
    file:position(Fd, {cur,-(N-2)}),
    reverse([$\n|Cs]);
collect_line(Fd, N, [$\n|_], Cs) ->
    file:position(Fd, {cur,-(N-1)}),
    reverse([$\n|Cs]);
collect_line(Fd, _, [], Cs) ->
    collect_line(Fd, Cs);
collect_line(Fd, N, [X|Xs], Cs) ->
    collect_line(Fd, N-1, Xs, [X|Cs]).

split_line(Line) ->
    split_line(Line, []).

split_line([$# | Xs], Tokens) -> reverse(Tokens);
split_line([$ | L], Tokens) ->   split_line(L, Tokens);
split_line([$\t | L], Tokens) -> split_line(L, Tokens);
split_line([$\n | L], Tokens) -> split_line(L, Tokens);
split_line([], Tokens) -> reverse(Tokens);
split_line([C|Cs], Tokens) -> split_mid(Cs, [C], Tokens).

split_mid([$# | Cs], Acc, Tokens) -> split_end(Acc, Tokens);
split_mid([$  | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\t | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\r, $\n | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\n | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([], Acc, Tokens) -> split_end(Acc, Tokens);
split_mid([C|Cs], Acc, Tokens) -> split_mid(Cs, [C|Acc], Tokens).

split_end([], Tokens) -> reverse(Tokens);
split_end(Acc, Tokens) -> reverse([reverse(Acc) | Tokens]).


