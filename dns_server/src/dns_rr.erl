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

%%% File    : dns_rr.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Decode a parsed RR and return a #dns_rr{}.
%%% Created : 18 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>

-module(dns_rr).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-export([decode/3, cr_default/1, origin/2]).

%% The defaults used while parsing a master file.
%% The defaults changes thenever new values are encountered.
-record(default, {domain,           % Default domain
		  bm,               % Default bitmap
		  origin,           % Originating domain
		  ttl = false,
		  class = in}).

-include("dns.hrl").

cr_default(Zone) -> #default{domain = tolower(Zone),
			     bm = add_last_dot(Zone),
			     origin = Zone}.

origin(Default, Origin) ->
    case last_dot(Origin) of
	{true, Origin1} ->
	    Default#default{origin = Origin1};
	_ ->
	    Default#default{origin = Origin ++ "." ++ Default#default.origin}
    end.

decode(RR, Default, Zone) ->
    {Domain, BM, RR1} = get_domain(RR, Default, Zone),
    {TTL0, RR2} = get_ttl(RR1, Default),
    {Class, RR3} = get_class(RR2, Default),
    {Type, RR4} = get_type(RR3),
    RData0 = get_rdata(RR4),
    RData = cr_rdata(Class, Type, RData0, Default#default.origin),
    TTL = case Type of
	      soa when Domain == Zone, TTL0 == false  ->
		  element(7, RData);
	      _ ->
		  TTL0
	  end,
    RRrec = #dns_rr{domain = Domain,
		    class = Class,
		    type = Type,
		    ttl = TTL,
		    bm = BM,
		    data = RData},
    Def = Default#default{domain = Domain,
			  bm = BM,
			  ttl = TTL,
			  class = Class},
    {ok, RRrec, Def}.

%% Optional
get_domain([{domain, "@"}|T], Default, Zone) ->
    Domain = Default#default.origin,
    {tolower(Domain), add_last_dot(Domain), T};
get_domain([{domain, Domain0}|T], #default{origin = Zone}, Zone) ->
    Domain = cr_domain(Domain0, Zone),
    {tolower(Domain), Domain0, T};
get_domain([{domain, Domain0}|T], #default{origin = Origin}, _) ->
    Domain = cr_domain(Domain0, Origin),
    {tolower(Domain), add_last_dot(Domain), T};
get_domain(RR, Default, _) ->
    {Default#default.domain, Default#default.bm, RR}.

add_last_dot(".")    -> ".";
add_last_dot(Domain) -> Domain ++ ".".

%% Optional
get_ttl([{ttl, TTL}|T], _) ->
    {TTL, T};
get_ttl([{class,C},{ttl, TTL}|T], _) ->
    {TTL, [{class,C}|T]};
get_ttl(RR, Default) ->
    {Default#default.ttl, RR}.

%% Optional
get_class([{class,Class}|T], _) ->
    {Class, T};
get_class(RR, Default) ->
    {Default#default.class, RR}.

%% Mandatory
get_type([{type, Type}|T]) -> {Type, T}.

%% Mandatory
get_rdata([{rdata, RData}]) -> RData.

tolower([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs])                       -> [C | tolower(Cs)];
tolower([])                           -> [].

%% Add originating zone to domain if not an absolute domain name
%% and make all chars lower case.
%% If it was an absolute domain name, delete the trailing dot.
cr_domain(Domain, Zone) ->
    add_zone(Domain, Zone).

cr_low_domain(Domain, Zone) ->
    tolower(cr_domain(Domain, Zone)).

add_zone(".", _) ->
    ".";
add_zone(Domain, Zone) ->
    case last_dot(Domain) of
	{true, DomainButDot} -> DomainButDot;
	_                    -> Domain ++ "." ++ Zone
    end.

last_dot(D) -> last_dot(D, []).
last_dot([$.], Ack)  -> {true, lists:reverse(Ack)};
last_dot([H|T], Ack) -> last_dot(T, [H|Ack]);
last_dot([], _)      -> false.

cr_rdata(in, ?S_A, [IP], _) ->
    [A,B,C,D] = string:tokens(IP, "."),
    {list_to_integer(A), 
     list_to_integer(B), 
     list_to_integer(C), 
     list_to_integer(D)};
cr_rdata(in, ?S_AAAA, As, _) when length(As) == 16 ->
    F = fun(L) -> list_to_integer(L) end,
    list_to_tuple(lists:map(F, As));
cr_rdata(in, ?S_NS, [Domain], Zone)    -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_MD, [Domain], Zone)    -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_MF, [Domain], Zone)    -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_CNAME, [Domain], Zone) -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_SOA, [Mname, Rname, Ser, Ref, Ret, Exp, Min], Zone) ->
    {cr_low_domain(Mname, Zone),
     cr_low_domain(Rname, Zone),
     list_to_integer(Ser),
     list_to_integer(Ref),
     list_to_integer(Ret),
     list_to_integer(Exp),
     list_to_integer(Min)};
cr_rdata(in, ?S_MB, [Domain], Zone)    -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_MG, [Domain], Zone)    -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_MR, [Domain], Zone)    -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_NULL, [Domain], Zone)  -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_WKS, [A,B,C,D, P | BitMap], _) ->
    F = fun(L) -> list_to_integer(L) end,
    Addr = list_to_tuple(lists:map(F,[A,B,C,D])),
    { Addr, list_to_integer(P), lists:map(F, BitMap)};
cr_rdata(in, ?S_PTR, [Domain], Zone)   -> cr_low_domain(Domain, Zone);
cr_rdata(in, ?S_HINFO, [CPU, OS], _) ->
    {CPU, OS};
cr_rdata(in, ?S_MINFO, [RM, EM], Zone) ->
    {cr_low_domain(RM, Zone), cr_low_domain(EM, Zone)};
cr_rdata(in, ?S_MX, [Pref, Domain], Zone) ->
    {list_to_integer(Pref), cr_low_domain(Domain, Zone)};
cr_rdata(in, ?S_SRV, [Prio, Weight, Port, Target], Zone) ->
    {list_to_integer(Prio),
     list_to_integer(Weight),
     list_to_integer(Port),
     cr_low_domain(Target, Zone)};
cr_rdata(in, ?S_TXT, Data, _) -> Data;
cr_rdata(_, _, Data, _) -> Data.


