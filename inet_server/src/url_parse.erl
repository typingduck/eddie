-module(url_parse).
-author('tony@RIOJA').

%%%----------------------------------------------------------------------
%%%
%%% Purpose : Parse/Format an URL accoring to rfc 1738
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
%%% AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Created :  2 Aug 1998 by  <tony@RIOJA>
%%% Contributor(s): ______________________________________.
%%%----------------------------------------------------------------------
-vc('$Id: url_parse.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-id('$Id: url_parse.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/inet_server/src/url_parse.erl,v $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:25 $ ').
-state('$State: Exp $ ').

-compile(export_all).
-export([parse/1, format/1]).
-import(lists, [reverse/1, append/1, member/2]).

-define(SAFE, "$-_.+").
-define(EXTRA, "!*'(),").
-define(NATIONAL, "{}|\\^~[]`").
-define(PUNCTUATION, "<>#%\"").
-define(URL_RESERVED, ";/?:@&=").

%% URI from Uniform Resource Identifiers (URI): Generic Syntax (DRAFT)
-define(URI_RESERVED, ";/?:@&=+$,").
-define(URI_MARK, "-_.!~*'()").
%% unreserved = alphanum | URI_MARK
-define(URI_DELIMS, "<>#%\"").

%%
%% parse an url return:
%% AbsUrl or error
%%
parse(Cs) ->
    case parse_scheme(Cs) of
	{http, Cs1}    -> parse_http(Cs1);
	{ftp, Cs1}     -> parse_ftp(Cs1);
	{file,Cs1}     -> parse_file(Cs1);
	{telnet,Cs1}   -> parse_telnet(Cs1);
	{mailto,Cs1}   -> parse_mailto(Cs1);
	{sip,Cs1}      -> parse_sip(Cs1);
	{tel,Cs1}      -> parse_tel(Cs1);
	{fax,Cs1}      -> parse_fax(Cs1);
	{modem,Cs1}    -> parse_modem(Cs1);
	{Scheme,Cs1}   -> {Scheme, Cs1};
	Error -> Error
    end.

format(AbsURL) ->
    error.

%%
%% parse the scheme part ie. in <scheme>:<scheme-specific-part>
%%
parse_scheme(Cs) -> parse_scheme(Cs,[]).

parse_scheme([X|Xs], Acc) ->
    if
	X >= $a, X =< $z -> parse_scheme(Xs, [X|Acc]);
	X >= $0, X =< $9 -> parse_scheme(Xs, [X|Acc]);
	X >= $A, X =< $Z -> parse_scheme(Xs, [(X-$A)+$a|Acc]);
	X == $- -> parse_scheme(Xs, [X |Acc]);
	X == $+ -> parse_scheme(Xs, [X |Acc]);
	X == $. -> parse_scheme(Xs, [X |Acc]);
	X == $: -> {list_to_atom(reverse(Acc)), Xs};
	true -> error
    end;
parse_scheme([], _) -> error.

%%
%% parse the http scheme:
%%  "http://" hostport [ "/" hpath [ "?" search ] ]
%%
%% return {http,Host,Port,Path,Search}
%% or     error
%%
parse_http([$/,$/|Cs]) ->
    case parse_hostport(Cs) of
	{Host,Port,[$/ | Cs1]} ->
	    case parse_hpath(Cs1) of
		{HPath, []} -> 
		    {http, Host, Port, HPath, []};
		{HPath, [$?|Cs2]} ->
		    case parse_search(Cs2) of
			{Search, []} ->
			    {http,Host,Port,HPath,Search};
			_ -> error
		    end;
		_ -> error
	    end;
	{Host,Port,[]} -> 
	    {http,Host,Port,[],[]};
	_ -> error
    end;
parse_http(_) ->
    error.

%%
%% parse the ftp scheme:
%%   "ftp://" login [ "/" fpath [ ";type=" ftptype ]]
%%    {ftp, Login, Path, Type}
%% where Login= {User, Password, {Host,Port}}
%%
parse_ftp([$/,$/|Cs]) ->
    case parse_login(Cs) of
	{User,Password,HostPort, [$/ | Cs1]} ->
	    case parse_fpath(Cs1) of
		{FPath, []} ->
		    {ftp, {User,Password,HostPort}, FPath, default};
		{FPath, [$;,$t,$y,$p,$e,$=,T]} ->
		    case parse_ftptype(T) of
			error -> 
			    error;
			Type ->
			    {ftp, {User,Password,HostPort}, FPath, Type}
		    end;
		_ -> error
	    end;
	{User,Password,HostPort, []} ->
	    {ftp, {User,Password,HostPort}, "", default};
	_ ->
	    error
    end;
parse_ftp(_) ->
    error.


parse_ftptype($A) -> ascii;
parse_ftptype($a) -> ascii;
parse_ftptype($i) -> image;
parse_ftptype($I) -> image;
parse_ftptype($d) -> directory;
parse_ftptype($D) -> directory;
parse_ftptype(_) -> error.

%% 
%% parse the file scheme
%%  "file://" [ host | "localhost" ] "/" fpath
%% 
%% return:
%%  {file, Host, Path}
%%
parse_file([$/,$/,$l,$o,$c,$a,$l,$h,$o,$s,$t,$/ | Cs]) ->
    case parse_fpath(Cs) of
	{FPath, []} -> {file, "localhost", FPath};
	_  -> error
    end;
parse_file([$/,$/,$/ | Cs]) ->
    case parse_fpath(Cs) of
	{FPath, []} -> {file, default, FPath};
	_ -> error
    end;
parse_file([$/,$/ | Cs]) ->
    case parse_host(Cs) of
	{Host, [$/|Cs1]} ->
	    case parse_fpath(Cs1) of
		{FPath,[]} -> {file, Host, FPath};
		_ -> error
	    end;
	_ -> error
    end;
parse_file(_) ->
    error.

%%
%% Parse telnet scheme:
%%  "telnet://" login [ "/" ]
%%
%% return:
%%   {telnet, {User,Password, {Host,Port}}}
%%
parse_telnet([$/,$/ | Cs]) ->
    case parse_login(Cs) of
	{User,Password,HostPort, [$/]} ->
	    {telnet, User,Password,HostPort};
	{User,Password,HostPort, []} ->
	    {telnet, User,Password,HostPort};
	_ ->
	    error
    end.

%%
%% Parse mailto scheme (updated with RFC 2368)
%% "mailto:" [ #mailbox ] [ "?" header *( "&" header ) ]
%%  header     =  hname "=" hvalue
%%  hname      =  *urlc
%%  hvalue     =  *urlc
%%
%% Return:
%%   {mailto, MailBox, Header}
%%
parse_mailto(Cs) ->
    error.

%% mailbox    = addr-spec | group route-addr
%% addr-spec  = local-part "@" domain
%% group      = phrase ":" [#mailbox] ";"
%% route-addr = "<" [route] addr-spec ">"
%% route      = 1#("@" domain) ":"
%% local-part = word * ("." word )
%% domain     = sub-domain *("." sub-domain)
%% sub-domain = doamin-ref | domain-literal
%% domain-ref = atom
%% doamin-literal = "[" 1*digit "." 1*digit "." 1*digit "." 1*digit "]"
%% word           = atom | quouted-string
%% atom           = 1*<any char except specials space and ctls>
%% quoted-string  = <"> *(qtext | quoted-pair) <">
%% qtext          = <any char, includeing bare CR & bare LF but not CRLF>
%% quoted-pair    = "\" CHAR
%% specials       = "(" | ")" | "<" | ">" | "@" | "," | ";" | ":" |
%%                  "\" | <"> | "." | "[" | "]"
%% pharse         = 1*word
%% 
parse_mailbox(Cs) ->
    error.
    
%%
%% Parse sip scheme (DRAFT)
%%  "sip:" [ userinfo "@" ] hostport url-parameters [headers]
%%  userinfo = *( unreserved | escaped |
%%                ";" | ":" | "&" | "=" | "+" | "$" | "," )
%%     url-parameters  = *( ";" url-parameter )
%%     url-parameter   = transport-param | user-param
%%                     | ttl-param | maddr-param | tag-param | other-param
%%     transport-param = "transport=" ( "udp" | "tcp" )
%%     ttl-param       = "ttl=" ttl
%%     ttl             = 1*3DIGIT       ; 0 to 255
%%     maddr-param     = "maddr=" maddr
%%     maddr           = IPv4address    ; multicast address
%%     user-param      = "user=" ( "phone" )
%%     tag-param       = "tag=" UUID
%%     other-param     = *uric
%%     headers         = "?" header *( "&" header )
%%     hname           = *uric
%%     hvalue          = *uric
%%     digits          = 1*DIGIT
%%                  
%%  
parse_sip(Cs) ->
    case parse_userinfo(Cs) of
	{UserInfo, [$@|Cs1]} ->  %% this must be a user info
	    case parse_hostport(Cs1) of
		{Host,Port,Cs2} ->
		    case parse_sip_param_header(Cs2) of
			{Params, Headers, []} ->
			    {sip,UserInfo,{Host,Port}, Params, Headers};
			Error -> error
		    end;
		Error -> error
	    end;
	{_,_} -> %% no user info (may be a host) REPARSE
	    case parse_hostport(Cs) of
		{Host,Port,Cs2} ->
		    case parse_sip_param_header(Cs2) of
			{Params, Headers, []} ->
			    {sip,[],{Host,Port}, Params, Headers};
			Error -> error
		    end;
		_ -> error
	    end;
	Error -> error
    end.
%%
%% Parse:  url-parameters [headers]
%%
parse_sip_param_header(Cs) ->
    case parse_sip_opt_params(Cs) of
	{Ps, Cs1} ->
	    case parse_opt_headers(Cs1) of
		{Hs, Cs2} ->
		    {Ps,Hs,Cs2};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

parse_sip_opt_params(Cs) ->
    parse_sip_opt_params(Cs,[]).

parse_sip_opt_params([$; | Cs], Ps) ->
    parse_sip_params(Cs, Ps);
parse_sip_opt_params(Cs, Ps) ->
    {Ps, Cs}.

parse_sip_params([$t,$r,$a,$n,$s,$p,$o,$r,$t,$= | Cs],Ps) ->
    case Cs of
	[$u,$d,$p|Cs1] ->
	    parse_sip_opt_params(Cs1, [{transport,udp}|Ps]);
	[$t,$c,$p|Cs1] ->
	    parse_sip_opt_params(Cs1, [{transport,tcp}|Ps]);
	_ ->
	    error
    end;
parse_sip_params([$u,$s,$e,$r,$=,$p,$h,$o,$n,$e | Cs],Ps) ->
    parse_sip_opt_params(Cs, [{user,phone}|Ps]);
parse_sip_params([$t,$t,$l,$=|Cs],Ps) ->
    case parse_digits(Cs) of
	{[], Cs1} -> error;
	{Digits, Cs1} ->
	    TTL = list_to_integer(Digits),
	    Len = length(Digits),
	    if Len =< 3, TTL < 256 ->
		    parse_sip_opt_params(Cs1, [{ttl,TTL}|Ps]);
	       true ->
		    error
	    end;
	_ ->
	    error
    end;
parse_sip_params([$m,$a,$d,$d,$r,$=|Cs],Ps) ->
    case parse_ipv4_address(Cs) of
	{Addr,Cs1} ->
	    parse_sip_opt_params(Cs1, [{maddr,Addr}|Ps]);
	_ ->
	    error
    end;
parse_sip_params([$t,$a,$g,$= | Cs],Ps) ->
    case parse_uuid(Cs) of
	{UUID, Cs1} ->
	    parse_sip_opt_params(Cs1, [{tag,UUID}|Ps]);
	_ -> 
	    error
    end;
parse_sip_params(Cs, Ps) ->
    case parse_uric(Cs) of
	{Other, Cs1} ->
	    parse_sip_opt_params(Cs1, [{other,Other}|Ps]);
	_ ->
	    error
    end.


parse_userinfo(Cs) ->
    parse_uchars(Cs, [], ";:&=+$,").

%% Parse tel scheme (DRAFT)
parse_tel(Cs) ->
    error.

%% Parse fax scheme (DRAFT)
parse_fax(Cs) ->
    error.

%% Parse modem scheme (DRAFT)
parse_modem(Cs) ->
    error.

%% parse 
%%    host [ : port ]
%%
parse_hostport(Cs) ->
    case parse_host(Cs) of
	{Host, [$:|Cs1]} ->
	    case parse_port(Cs1) of
		{Port,Cs2} -> {Host,Port,Cs2};
		_ -> error
	    end;
	{Host, Cs1} -> {Host,default,Cs1};
	error -> error
    end.


%% parse [ headers ]
parse_opt_headers([$? | Cs]) ->
    parse_headers(Cs, []);
parse_opt_headers(Cs) ->
    {[], Cs}.

%% parse header *( "&" header ) 
%% FIXME: DONT WORK since = is in *uric !!!

parse_headers(Cs, Hs) ->
    case parse_uric(Cs) of
	{HName, [$=|Cs1]} ->
	    case parse_uric(Cs1) of
		{HValue, [$& | Cs2]} ->
		    parse_headers(Cs2, [{HName,HValue} | Hs]);
		{HValue, Cs2} ->
		    { [{HName,HValue}|Hs], Cs2};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

%% parse host
%% return {A,B,C,D} if numeric number
%%        hostname  if host name
%%
-define(ALPHA, 1).
-define(DIGIT, 2).

parse_host(Cs) -> parse_host(Cs, [], 0, [], []).

%% FIXME: check top-label  (must start with alpha!)
%% (if all components are numeric there may be only 4)
parse_host([X|Cs],Acc,Z,Lbls,Zs) ->
    if
	X >= $a, X =< $z -> parse_host(Cs, [X|Acc],Z bor ?ALPHA, Lbls, Zs);
	X >= $A, X =< $Z -> parse_host(Cs, [X|Acc],Z bor ?ALPHA, Lbls, Zs);
	X >= $0, X =< $9 -> parse_host(Cs, [X|Acc],Z bor ?DIGIT, Lbls, Zs);
	X == $-, Z =/= 0 -> parse_host(Cs, [X|Acc], Z, Lbls,Zs);
	X == $., Z =/= 0, hd(Acc) =/= $- ->
	    parse_host(Cs, [], 0, [".", reverse(Acc) | Lbls], [Z|Zs]);
	Z =/= 0 ->
	    make_host([reverse(Acc)|Lbls], [Z|Zs], [X|Cs]);
	true ->
	    error
    end;
parse_host([], Acc, Z, Lbls, Zs) when Z =/= 0, hd(Acc) =/= $- ->
    make_host([reverse(Acc)|Lbls], [Z|Zs], []);
parse_host(_, _, _, _, _) ->
    error.

%% Make host given labels (reversed) and do some post checking
make_host([L4,_,L3,_,L2,_,L1], [?DIGIT,?DIGIT,?DIGIT,?DIGIT], Cs) ->
    { {list_to_integer(L1), list_to_integer(L2), 
       list_to_integer(L3), list_to_integer(L4) }, Cs};
make_host([Ltop|Ls], [?DIGIT|_], Cs) -> error;
make_host([[X|Xs]|Ls], _, Cs) ->
    if
	X >= $a, X =< $z -> { append(reverse([[X|Xs]|Ls])), Cs};
	X >= $A, X =< $Z -> { append(reverse([[X|Xs]|Ls])), Cs};
	true -> error
    end;
make_host(Ls, _, Cs) ->
    error.

%% parse an UUID (DRAFT) string version
%% 4*hex "-" 2*hex "-" 2*hex "-" hex hex "-" 6*hex
parse_uuid([X1,X2,X3,X4,$-,X5,X6,$-,X7,X8,$-,X9,X10,$-,
	    N1,N2,N3,N4,N5,N6 | Cs]) ->
    case is_hex_string([X1,X2,X3,X4,X5,X6,X7,X9,X9,X10,
			N1,N2,N3,N4,N5,N6]) of
	true ->
	    { [X1,X2,X3,X4,$-,X5,X6,$-,X7,X8,$-,X9,X10,$-,
	       N1,N2,N3,N4,N5,N6], Cs};
	false ->
	    error
    end.

%% parse an ipv4 address return {{A,B,C,D}, Tail}
%% or error
parse_ipv4_address(Cs) ->
    parse_ipv4_address(Cs,[]).

parse_ipv4_address(Cs, Ns) ->
    case parse_digits(Cs) of
	{[],_} -> error;
	{Digits,[$.|Cs1]} ->
	    N = list_to_integer(Digits),
	    parse_ipv4_address(Cs1, [N|Ns]);
	{Digits, Cs1} when length(Ns) == 3 ->
	    D = list_to_integer(Digits),
	    [C,B,A] = Ns,
	    { {A,B,C,D}, Cs1 }
    end.


-define(FSTR, "?:@&=").

parse_fpath(Cs) ->
    parse_path(Cs, [], ?FSTR).

-define(HSTR, ";:@&=").

parse_hpath(Cs) ->
    parse_path(Cs, [], ?HSTR).

parse_path(Cs, Path, Allow) ->
    case parse_segment(Cs, Allow) of
	{HSeg, [$/|Cs1]} ->
	    parse_path(Cs1, ["/", HSeg | Path], Allow);
	{HSeg, Cs1} ->
	    {append(reverse([HSeg|Path])), Cs1};
	Error -> Error
    end.

parse_segment(Cs, Allow) ->
    parse_uchars(Cs, Allow).

parse_search(Cs) -> 
    parse_uchars(Cs, ?HSTR).

parse_uric(Cs) ->
    parse_uchars(Cs, [], ?URI_RESERVED ?URI_MARK).

parse_uchars(Cs, Allow) ->
    parse_uchars(Cs, [], ?SAFE ?EXTRA ++ Allow).

parse_uchars([X|Cs],Acc, Allow) ->    
    if
	X >= $a, X =< $z -> parse_uchars(Cs, [X|Acc],Allow);
	X >= $A, X =< $Z -> parse_uchars(Cs, [X|Acc],Allow);
	X >= $0, X =< $9 -> parse_uchars(Cs, [X|Acc],Allow);
	X == $% ->
	    case hex2(Cs) of
		{X1,Cs1} -> parse_uchars(Cs1, [X1|Acc],Allow);
		Error -> Error
	    end;
	true ->
	    case member(X, Allow) of
		true -> parse_uchars(Cs, [X|Acc],Allow);
		false -> {reverse(Acc), [X|Cs]}
	    end
    end;
parse_uchars([], Acc, _) ->
    {reverse(Acc), []}.

%% parse *DIGIT
parse_digits(Cs) ->
    parse_digits(Cs, []).

parse_digits([X|Cs], Acc) when X >= $0, X =< $9 ->
    parse_digits(Cs, [X|Acc]);
parse_digits(Cs, Acc) ->
    {reverse(Acc), Cs}.


%% parse port 
parse_port(Cs) ->
    parse_port(Cs,[]).
parse_port([X|Cs], Acc) when X >= $0, X =< $9 ->
    parse_port(Cs, [X|Acc]);
parse_port(Cs, []) -> error;
parse_port(Cs, Acc) ->
    {list_to_integer(reverse(Acc)), Cs}.

%% parse two hex digits
hex2([X1,X2|Cs]) ->
    case hex(X1) of
	error -> error;
	N1 ->
	    case hex(X2) of
		error -> error;
		N2 ->
		    {N1*16+N2, Cs}
	    end
    end.

hex(X) when X >= $0, X =< $9 -> X-$0;
hex(X) when X >= $a, X =< $f -> (X-$a)+10;
hex(X) when X >= $A, X =< $F -> (X-$A)+10;
hex(_) -> error.

%% check if only hex digits
is_hex_string([X|Xs]) ->
    case is_hex(X) of
	true -> is_hex_string(Xs);
	false -> false
    end;
is_hex_string([]) -> 
    true.

is_hex(X) when X >= $0, X =< $9 -> true;
is_hex(X) when X >= $a, X =< $f -> true;
is_hex(X) when X >= $A, X =< $F -> true;
is_hex(_) -> false.

%% check if only digits
is_digits([X|Xs]) when X >= $0, X =< $9 ->
    is_digits(Xs);
is_digits([_|Xs]) -> false;
is_digits([]) -> true.

%%  
%% parse login: [ user [ ":" password ] @ ] hostport
%% return {User,Password,{Host,Port}, Tail}
%%
parse_login(Cs) ->
    case parse_uchars(Cs, ";?&=") of
	{UserOrHost, [$: | Cs1]} ->
	    %% may be User or Host !!!
	    case parse_uchars(Cs1, ";?&=") of
		{Password, [$@ | Cs2]} -> %% ok UserOrHost was a User
		    case parse_hostport(Cs2) of
			{Host,Port,Cs3} ->
			    {UserOrHost,Password,{Host,Port}, Cs3};
			_ ->
			    error
		    end;
		{QDigits, Cs2} ->
		    %% if QDigits are digits only then it is a port number and
		    %% UserHost was a hots
		    case is_digits(QDigits) of
			true ->
			    Port = list_to_integer(QDigits),
			    {default,default,{UserOrHost,Port}, Cs2};
			false ->
			    error
		    end;
		_ -> 
		    error
	    end;
	{User, [$@ | Cs1]} -> %% must be User (hostport follows)
	    case parse_hostport(Cs1) of
		{Host,Port,Cs2} ->
		    {User,default,{Host,Port}, Cs2};
		_ ->
		    error
	    end;
	{FirstLabel, [$. | Cs1]} ->  %% no user given (firstlabel)
	    case parse_hostport(FirstLabel ++ [$. | Cs]) of
		{Host,Port,Cs2} ->
		    {default,default,{Host,Port},Cs2};
		_ ->
		    error
	    end;
	{Label, [$/ | Cs1]} ->  %% host only one label!
	    case parse_hostport(Label) of
		{Host,Port,[]} ->
		    {default, default, {Host,Port}, [$/|Cs1]};
		_ ->
		    error
	    end;
	_  ->
	    error
    end.

	    
			    
	    
	    
		

			    

						
	    


	    
	
	 

	     
	 
	    

	     
	 
		    
	    
    
    
    

	     
    

