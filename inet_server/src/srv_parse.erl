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

%%% File    : srv_parse.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : SIP Parser/HTTP Parser
%%% Created :  4 Aug 1998 by Tony Rogvall <tony@erix.ericsson.se>

-module(srv_parse).
-author('tony@erix.ericsson.se').
-modified_by('patrik@erix.ericsson.se').
-modified_by('jon@serc.rmit.edu.au').

-compile(export_all).
-import(lists, [reverse/1]).

-include("srv_parse.hrl").

-export([request/1,response/1,
	 add_header_to_parsed_headers/3,
	 integer_field/1,
	 dec_date/1,enc_date/1,enc_current_date/0]).

%% Functions currently not used:
%% dec_sip_via, parse_comment, parse_phrase


%%
%% we may have to run utf-8 decode before parsing ?
%%
request([$\r,$\n|Cs]) -> request(Cs);
request(Cs) ->
    case request_line(Cs) of
	{ok, Req, Type, Cs1} -> 
	    {ok, Headers} = opt_message_headers(Cs1, Type),
	    {ok, Req ++ Headers};
	Error -> Error
    end.


response([$\r,$\n|Cs]) -> response(Cs);
response(Cs) ->
    case status_line(Cs) of
	{ok, Res, Type, Cs1} ->
	    {ok, Headers} = opt_message_headers(Cs1, Type),
	    {ok, Res ++ Headers};
	Error ->  Error
    end.


%% <method> SP <uri> SP Type/Major.Minor
request_line(Cs) ->
    {M,Cs1} = method(Cs),
    {URI,Cs2} = request_uri(Cs1),
    {Type,Ver,Rest} = case Cs2 of
			  [] ->
			      {http,"HTTP/0.9",[]};
			  _ ->
			      {{Ty, Ve},Cs3} = version(Cs2),
			      {_,Cs4} = crlf_split(Cs3),
			      {Ty,Ve,Cs4}
    end,
    Method = case srv_table:lookup({Type,method,M}) of
		 undefined ->
		     {undefined, M};  %% no such method
		 {_, _, Fun} ->
		     Fun(decode)
	     end,
    {ok, [{method, Method},
	  {uri, URI },
	  {version, Ver}], Type, Rest}.

%% Type/Major.Minor <code> <phrase>
status_line(Cs) ->
    {{Type,Ver},[$ |Cs1]} = version(Cs),
    [X1,X2,X3,$ |Cs2] = Cs1,
    {Phrase,Cs3} = phrase(Cs2),
    {ok, [{version, Ver},
	   {status, [X1,X2,X2]}], Type, Cs3}.

phrase(Cs) ->    
    phrase(Cs, []).

phrase([$\r,$\n | Cs], Acc) ->
    {reverse(Acc), Cs};
phrase([C | Cs], Acc) ->
    phrase(Cs, [C|Acc]).

%% Parse method or fail. return as atom
method(Cs) ->
    {Method, Cs1} = key(Cs),
    {Method, skip_lwsp(Cs1)}.

%% Parse uri (uninterpreted for now)
request_uri(Cs) ->    
    request_uri(Cs,[]).

request_uri([$ |Cs],Acc) -> 
    {reverse(Acc), skip_lwsp(Cs)};
%%    case url_parse:parse(Request_URI) of
%%	error -> {{error,Request_URI}, Cs};
%%	URI   -> {URI,Cs}
%%    end;
request_uri([$\t | Cs],Acc) ->
    {reverse(Acc), skip_lwsp(Cs)};
request_uri([$\r,$\n | Cs],Acc) -> %HTTP/0.9
    {reverse(Acc),[]};
request_uri([$\n | Cs],Acc) -> %HTTP/0.9
    {reverse(Acc),[]};
request_uri([C|Cs], Acc) ->
    request_uri(Cs, [C|Acc]).

%% Sip/Http version
version(Cs) -> version(Cs, []).
version(Cs,Acc) ->
    {Type, [$/|Cs1]} = key(Cs),
    {Major,[$.|Cs2]} = num(Cs1),
    {Minor,Cs3} = num(Cs2),
    {{list_to_atom(Type), lists:concat(["HTTP/",Major,".",Minor])}, skip_lwsp(Cs3)}.

%% Parse keys i.e case insensitive 1*alpha  return as atoms
key(Cs) ->
    key(Cs, []).

key([C|Cs],Acc) when C >= $a, C =< $z ->
    key(Cs, [C|Acc]);
key([C|Cs],Acc) when C >= $A, C =< $Z ->
    key(Cs, [(C-$A)+$a|Acc]);
key(Cs, Acc) when length(Acc) >= 1 ->
    {reverse(Acc), Cs}.


%% return 1*digits as a number
num([C|Cs]) when C >= $0, C =< $9 ->
    num(Cs, C-$0).

num([C|Cs], N) when C >= $0, C =< $9 ->
    num(Cs, N*10 + (C-$0));
num(Cs, N) ->
    {N, Cs}.

%%
%% parse message_headers
%%   Key: Value CRLF
%%   stop at CRLF
%%
%% unknown header fields are coded as {undefined,{Key,Value}}
%% malformed fields are coded as {error,{Key,Value}}
%% other fields are coded as {Key,Value} where Key is an atom 
%% and value is the decoded form
%% 
opt_message_headers([],_) ->
    {ok,[]}; % no headers
opt_message_headers(Cs, Type) ->
    opt_message_headers(Cs,Type,[]).
    
opt_message_headers([$\r,$\n|Cs],Type,Hs) ->
    {ok, reverse(Hs)};
opt_message_headers([$\n|Cs],Type,Hs) ->
    {ok, reverse(Hs)};
opt_message_headers(Cs,Type,Hs) ->
    {Name, Cs1} = field_name(Cs),
    {Value,Cs2} = field_value(Cs1),
    case srv_table:lookup({Type,field,Name}) of
	undefined ->
	    opt_message_headers(Cs2, Type, [{undefined,{Name,Value}}|Hs]);
	{_, _, Fun} -> 
	    case catch Fun(decode,Value) of
		{'EXIT',_} ->
		    opt_message_headers(Cs2, Type, [{error,{Name,Value}}|Hs]);
		Header ->
		    opt_message_headers(Cs2, Type, [Header | Hs])
	    end
    end.

%% add a header to the parsed headers
%% used by: http_parse:parse_headers (adding x-forwarded-for)
add_header_to_parsed_headers(Name,Value,Parsed_headers) ->
    case srv_table:lookup({http,field,Name}) of
	undefined ->
	    [{undefined,{Name,Value}}|Parsed_headers];
	{_, _, Fun} ->
	    case catch Fun(decode,Value) of
		{'EXIT',_} ->
		    [{error,{Name,Value}}|Parsed_headers];
		Header ->
		    [Header|Parsed_headers]
	    end
    end.

%% interpret integer field
integer_field(Cs) ->
    num(skip_lwsp(Cs)).

%% skip space & tab
skip_lwsp([$ | Cs]) -> skip_lwsp(Cs);
skip_lwsp([$\t | Cs]) -> skip_lwsp(Cs);
skip_lwsp(Cs) -> Cs.

%%
%% split a list on first occurence of \r\n (CRLF), tolerate \n (LF)
%%
crlf_split(Cs) ->
    crlf_split(Cs,[]).

crlf_split([$\r,$\n|Cs],Acc) ->
    {reverse(Acc),Cs};
crlf_split([$\n|Cs],Acc) ->
    {reverse(Acc),Cs};
crlf_split([C|Cs],Acc) ->
    crlf_split(Cs,[C|Acc]);
crlf_split([],Acc) ->
    {reverse(Acc),[]}.

%%
%% Field names are parse so that uppercase is transform to lowercase
%%
field_name(Cs) ->
    field_name(Cs, []).

field_name([$:|Cs],Acc) when length(Acc) >= 1 ->
    {reverse(Acc), Cs};
field_name([C|Cs], Acc) when C >= $A, C =< $Z -> %% make lower case
    field_name(Cs, [(C-$A)+$a|Acc]);
field_name([C|Cs], Acc) when C > 32 -> %% no control characters
    field_name(Cs, [C|Acc]).

%%
%% Field values (FIXME)
%%
field_value([$ | Cs]) -> field_value(Cs);
field_value([$\t | Cs]) -> field_value(Cs);
field_value(Cs) -> field_value(Cs, []).

field_value([$\r,$\n | Cs], Acc) ->
    case Cs of
	[$\t | Cs1] -> field_value(skip_lwsp(Cs1), Acc);
	[$   | Cs1] -> field_value(skip_lwsp(Cs1), Acc);
	_ -> {reverse(Acc), Cs}
    end;
field_value([$\n | Cs], Acc) ->
    case Cs of
	[$\t | Cs1] -> field_value(skip_lwsp(Cs1), Acc);
	[$   | Cs1] -> field_value(skip_lwsp(Cs1), Acc);
	_ -> {reverse(Acc), Cs}
    end;
field_value([C|Cs], Acc) -> field_value(Cs, [C|Acc]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode http-date (RFC 2068). (MUST be send in RFC1123 date format)
%%          HTTP-date    = rfc1123-date | rfc850-date | asctime-date
%%          rfc1123-date = wkday "," SP date1 SP time SP "GMT"
%%          rfc850-date  = weekday "," SP date2 SP time SP "GMT"
%%          asctime-date = wkday SP date3 SP time SP 4DIGIT
%%
%%          date1        = 2DIGIT SP month SP 4DIGIT
%%                         ; day month year (e.g., 02 Jun 1982)
%%          date2        = 2DIGIT "-" month "-" 2DIGIT
%%                         ; day-month-year (e.g., 02-Jun-82)
%%          date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
%%                         ; month day (e.g., Jun  2)
%%
%%          time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
%%                         ; 00:00:00 - 23:59:59
%%
%%          wkday        = "Mon" | "Tue" | "Wed"
%%                       | "Thu" | "Fri" | "Sat" | "Sun"
%%
%%
%%          weekday      = "Monday" | "Tuesday" | "Wednesday"
%%                       | "Thursday" | "Friday" | "Saturday" | "Sunday"
%%
%%          month        = "Jan" | "Feb" | "Mar" | "Apr"
%%                       | "May" | "Jun" | "Jul" | "Aug"
%%                       | "Sep" | "Oct" | "Nov" | "Dec"
%%
%% decode date or crash!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_date(Line) -> dec_http_date(tolower(Line)).

dec_http_date([$m,$o,$n,$d,$a,$y,$  | Cs]) -> dec_date2(Cs);
dec_http_date([$t,$u,$e,$s,$d,$a,$y,$  | Cs]) -> dec_date2(Cs);
dec_http_date([$w,$e,$d,$n,$s,$d,$a,$y,$  | Cs]) -> dec_date2(Cs);
dec_http_date([$t,$h,$u,$r,$s,$d,$a,$y,$  | Cs]) -> dec_date2(Cs);
dec_http_date([$f,$r,$i,$d,$a,$y,$  | Cs]) -> dec_date2(Cs);
dec_http_date([$s,$a,$t,$u,$r,$d,$a,$y,$   | Cs]) -> dec_date2(Cs);
dec_http_date([$s,$u,$n,$d,$a,$y,$  | Cs]) -> dec_date2(Cs);
dec_http_date([$m,$o,$n,X | Cs]) -> dec_date13(X,Cs);
dec_http_date([$t,$u,$e,X  | Cs]) -> dec_date13(X,Cs);
dec_http_date([$w,$e,$d,X  | Cs]) -> dec_date13(X,Cs);
dec_http_date([$t,$h,$u,X  | Cs]) -> dec_date13(X,Cs);
dec_http_date([$f,$r,$i,X  | Cs]) -> dec_date13(X,Cs);
dec_http_date([$s,$a,$t,X  | Cs]) -> dec_date13(X,Cs);
dec_http_date([$s,$u,$n,X  | Cs]) -> dec_date13(X,Cs).

dec_date13($ , Cs) -> dec_date3(Cs);
dec_date13($,, [$ |Cs]) -> dec_date1(Cs).

%% date1
dec_date1([D1,D2,$ ,M1,M2,M3,$ ,Y1,Y2,Y3,Y4,$  | Cs]) ->
    M = dec_month([M1,M2,M3]),
    D = list_to_integer([D1,D2]),
    Y = list_to_integer([Y1,Y2,Y3,Y4]),
    {Time,[$ ,$g,$m,$t|Cs1]} = dec_time(Cs),
    { {{Y,M,D},Time}, Cs1}.

%% date2
dec_date2([D1,D2,$-,M1,M2,M3,$-,Y1,Y2 | Cs]) ->
    M = dec_month([M1,M2,M3]),
    D = list_to_integer([D1,D2]),
    Y = 1900 + list_to_integer([Y1,Y2]),
    {Time, [$ ,$g,$m,$t|Cs1]} = dec_time(Cs),
    {{{Y,M,D}, Time}, Cs1}.

%% date3
dec_date3([M1,M2,M3,$ ,D1,D2,$ | Cs]) ->
    M = dec_month([M1,M2,M3]),
    D = if D1 == $  -> list_to_integer([D2]);
	   true -> list_to_integer([D1,D2])
	end,
    {Time,[$ ,Y1,Y2,Y3,Y4|Cs1]} = dec_time(Cs),
    Y = list_to_integer([Y1,Y2,Y3,Y4]),
    { {{Y,M,D}, Time}, Cs1 }.

%% decode lowercase month
dec_month("jan") -> 1;
dec_month("feb") -> 2;
dec_month("mar") -> 3;
dec_month("apr") -> 4;
dec_month("may") -> 5;
dec_month("jun") -> 6;
dec_month("jul") -> 7;
dec_month("aug") -> 8;
dec_month("sep") -> 9;
dec_month("oct") -> 10;
dec_month("nov") -> 11;
dec_month("dec") -> 12.

%% decode time HH:MM:SS
dec_time([H1,H2,$:,M1,M2,$:,S1,S2|Cs]) ->
    { {list_to_integer([H1,H2]), 
       list_to_integer([M1,M2]),
       list_to_integer([S1,S2]) }, Cs}.

%% encode date into rfc1123-date (must be a GMT time!!!)
enc_date({{Y,M,D},{TH,TM,TS}}) ->
    WkDay = case calendar:day_of_the_week({Y,M,D}) of
		1 -> "Mon";
		2 -> "Tue";
		3 -> "Wed";
		4 -> "Thu";
		5 -> "Fri";
		6 -> "Sat";
		7 -> "Sun"
	    end,
    lists:flatten(io_lib:format("~s, ~2..0w ~s ~4..0w "
				"~2..0w:~2..0w:~2..0w GMT",
				[WkDay, D, enc_month(M), Y, TH, TM, TS])).

%% Used by erlet_admit_ctrl.erl
enc_current_date() ->
    enc_date(calendar:universal_time()).

%% decode lowercase month
enc_month(1) -> "Jan";
enc_month(2) -> "Feb";
enc_month(3) -> "Mar";
enc_month(4) -> "Apr";
enc_month(5) -> "May";
enc_month(6) -> "Jun";
enc_month(7) -> "Jul";
enc_month(8) -> "Aug";
enc_month(9) -> "Sep";
enc_month(10) -> "Oct";
enc_month(11) -> "Nov";
enc_month(12) -> "Dec".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encode/Decode the SIP Via field value
%%
%%     Via              = ( "Via" $|$ "v") ":" 1#( sent-protocol sent-by
%%                        *( ";" via-params ) [ comment ] )
%%     via-params       = via-hidden | via-ttl | via-received
%%                      | via-branch
%%     via-hidden       = "hidden"
%%     via-ttl          = "ttl" "=" ttl
%%     via-received   = "received" "=" host
%%     via-branch       = "branch" "=" token
%%     sent-protocol    = [ protocol-name "/" ] protocol-version
%%                        [ "/" transport ]
%%     protocol-name    = "SIP" $|$ token
%%     protocol-version = token
%%     transport        = "UDP" $|$ "TCP" $|$ token
%%     sent-by          = ( host [ ":" port ] ) $|$ ( concealed-host )
%%     concealed-host   = token
%%     ttl              = 1*3DIGIT     ; 0 to 255
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_sip_via(Value) ->
    lists:map(
      fun(Via) ->
	      [Sent | Params] = string:tokens(Via, ";"),
	      case string:tokens(Sent, "/") of
		  [Proto,Version,TransHost] -> 1;
		  [Version, TransHost] -> 2;
		  [VersionHost] -> 3;
		  _ -> 4
	      end
      end, string:tokens(Value, ",")).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UTILS:
%% tolower :     uppercase alpha to lower case alpha
%% qtext:        parse a quoted text (RFC822)
%% quoted_string: parse <"> *(qtext/quoted-pair) <">
%%
%%
%% 
-define(SPECIALS, "()<>@,;:\\\".[]").

parse_quoted_string([$" | Cs]) ->
    {String,[$"|Cs1]} = parse_qtext(Cs, []),
    {String, Cs1}.

parse_qtext([$" | Cs], Acc) -> {reverse(Acc), Cs};
parse_qtext([$\\,C | Cs], Acc) -> parse_qtext(Cs, [C|Acc]);
parse_qtext([C|Cs], Acc) -> parse_qtext(Cs, [C|Acc]).

parse_atom(Cs) ->
    parse_chars(Cs, [], ?SPECIALS).

parse_word([$"|Cs]) -> parse_quoted_string([$"|Cs]);
parse_word(Cs) -> parse_atom(Cs).

parse_phrase(Cs) ->
    parse_phrase(Cs, []).

parse_phrase([C|Cs], Words) ->
    if C < 32  -> { reverse(Words), [C|Cs] };
       true ->
	    case lists:member(C, ?SPECIALS--[$"]) of
		true -> { reverse(Words), [C|Cs] };
		false ->
		    case parse_word([C|Cs]) of
			{Word,[32 |Cs1]} ->
			    parse_phrase(Cs1,[Word|Words]);
			{Word,Cs1} ->
			    { reverse([Word|Words]), Cs1}
		    end
	    end
    end;
parse_phrase([], Words) ->
    { reverse(Words), [] }.

parse_comment([$( | Cs]) ->
    parse_comment(Cs, [$(], 1).

parse_comment([$( | Cs], Acc, N) ->
    parse_comment(Cs, [$(|Acc], N+1);
parse_comment([$) | Cs], Acc, N) ->
    if N == 1 -> { reverse([$)|Acc]), Cs};
       true -> parse_comment(Cs, [$)|Acc], N-1)
    end;
parse_comment([$\\,C|Cs], Acc, N) ->
    parse_comment(Cs, [C|Acc], N);
parse_comment([C|Cs], Acc, N) ->
    parse_comment(Cs, [C|Acc], N);
parse_comment([], Acc, N) ->
    %% this is an error (but)
    { reverse(Acc), [] }.


	    
parse_chars([C|Cs], Acc, Exclude) ->
    if C =< 32 -> {reverse(Acc),[C|Cs]};
       true ->
	    case lists:member(C, Exclude) of
		true -> {reverse(Acc),[C|Cs]};
		false -> parse_chars(Cs,[C|Acc],Exclude)
	    end
    end;
parse_chars([],Acc,_) -> {reverse(Acc),[]}.
	    
	    

    

tolower([C|Cs]) when C >= $A, C =< $Z ->
    [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs]) ->
    [C | tolower(Cs)];
tolower([]) -> [].
