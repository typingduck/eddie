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
%%% Mar, Apr 99 - jon@eddieware.org 
%%%

-module(erlet_admit_ctrl).
-author('patrik@punsch').
-vc('$Id: erlet_admit_ctrl.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').

-include("inet_server.hrl").

-export([load/2, store/2, do/2]).


load(Row, Data) ->
    ok.

store(Erlets_name, Erlet_config) ->
    ok.

do(State, Req) when list(Req) ->
    %?DEBUG("Request is ~p~n", [Req]),
    {State,
     case lists:flatten(Req) of
	 [$G,$E,$T,$ ,$/,$e, $d, $d, $i, $e, $_,$a,$d,$m,$i,$t,$_,$q,$u,$e,$u,$e
	  | Rest] ->
	     %?DEBUG("Req is ~p~n", [Req]),
	     Decoded =
		 case Rest of
		     [$?|Other] ->
			 decode_cgi(Other);
		     _ ->
			 decode_cgi([])
		 end,
	     %?DEBUG("CGI params ~p~n", [Decoded]),

	     Place =
		 case lists:keysearch(place, 1, Decoded) of
		     {_, {_, P}} ->
			 P;
		     _ ->
			 "unknown"
		 end,
	     Estimate =
		 case lists:keysearch(estimate, 1, Decoded) of
		     {_, {_, E}} ->
			 E;
		     _ ->
			 "unknown"
		 end,
	     Time =
		 case lists:keysearch(reload, 1, Decoded) of
		     {_, {_, T}} ->
			 T;
		     _ ->
			 "5"
		 end,
	     Estimate_phrase =
		 case Estimate of
		     "unknown" ->
			 "";
		     _ ->
			 lists:append(["<P>We estimate it will take ",
				       Estimate,
				       " seconds before you are admitted."])
		 end,

	     Mess =
		 lists:append(["<HTML>"
			       "<META HTTP-EQUIV=\"Refresh\" CONTENT=\"",
			       Time, "\">"
			       "<BODY BGCOLOR=\"#FFFFFF\">"
			       "<CENTER>"
			       "<FONT SIZE=\"7\">"
			       "<BR><BR><BR>"
			       "<P>This site is temporarily overloaded."
			       "<P>You are queued for entry to the site. ",
			       "<P>This page will automatically be refreshed."
			       "</FONT>"
			       "</CENTER>"
			       "</BODY>"
			       "</HTML>"]),

%		 lists:append(["<HTML>"
%			       "<META HTTP-EQUIV=\"Refresh\" CONTENT=\"",
%			       Time, "\">"
%			       "<BODY BGCOLOR=\"#FFFFFF\">"
%			       "<CENTER>"
%			       "<FONT SIZE=\"7\">"
%			       "<BR><BR><BR>"
%			       "<P>This site is temporarily overloaded."
%			       "<P>You are queued for entry to the site at position ",
%			       Place,
%			       ".",
%			       Estimate_phrase,
%			       "<P>This page will automatically be refreshed."
%			       "</FONT>"
%			       "</CENTER>"
%			       "</BODY>"
%			       "</HTML>"]),

	     Date = srv_parse:enc_current_date(),
	      {send, lists:append([
				   "HTTP/1.0 503 Service Unavailable\r\n"
				   "Server: Eddie-gateway/1.4\r\n"
				   "Connection: close\r\n"
				   "Date: ",
				   Date,
				   "\r\n"
				   "Expires: ",
				   Date,
				   "\r\n"
				   "Content-Type: text/html\r\n"
                                   "Content-Length: ",
				   integer_to_list(length(Mess)),
				   "\r\n\r\n",
				   Mess])};
	 [$G,$E,$T,$ ,$/,$l, $o, $d, $b, $r, $o, $k, $e, $r,$_,$a,$d,$m,$i,$t,$_,$r,$e,$j,$e,$c,$t,
	  $ ,$H,$T,$T,$P,$/,$1,$.,$0,$\r,$\n,$\r,$\n] ->
	     Mess =
		 lists:append(["<HTML>"
			       "<BODY BGCOLOR=\"#FFFFFF\">"
			       "<CENTER>"
			       "<FONT SIZE=\"7\">"
			       "<BR><BR><BR>"
			       "<P>This site is temporarily overloaded."
			       "<P>Please try connecting later.",
			       "</FONT>"
			       "</CENTER>"
			       "</BODY>"
			       "</HTML>"]),
	     Date = srv_parse:enc_current_date(),
	      {send, lists:append([
				   "HTTP/1.0 503 Service Unavailable\r\n"
				   "Server: Eddie-gateway/1.4\r\n"
				   "Connection: close\r\n"
				   "Date: ",
				   Date,
				   "\r\n"
				   "Expires: ",
				   Date,
				   "\r\n"
				   "Content-Type: text/html\r\n"
				   "Content-Length: ",
				   integer_to_list(length(Mess)),
				   "\r\n\r\n",
				   Mess])};
	 [$G,$E,$T,$ ,$/,$l, $o, $d, $b, $r, $o, $k, $e, $r,$_,$a,$d,$m,$i,$t,$_,$b,$l,$o,$c,$k,$e,$d,
	  $ ,$H,$T,$T,$P,$/,$1,$.,$0,$\r,$\n,$\r,$\n] ->
	     Mess =
		 lists:append(["<HTML>"
			       "<BODY BGCOLOR=\"#FFFFFF\">"
			       "<CENTER>"
			       "<FONT SIZE=\"7\">"
			       "<BR><BR><BR>"
			       "<P>You are blocked from this site."
			       "</FONT>"
			       "</CENTER>"
			       "</BODY>"
			       "</HTML>"
			       ]),
	     Date = srv_parse:enc_current_date(),
	      {send, lists:append([
				   "HTTP/1.0 503 Service Unavailable\r\n"
				   "Server: Eddie-gateway/1.4\r\n"
				   "Connection: close\r\n"
				   "Date: ",
				   Date,
				   "\r\n"
				   "Expires: ",
				   Date,
				   "\r\n"
				   "Content-Type: text/html\r\n"
				   "Content-Length: ",
				   integer_to_list(length(Mess)),
				   "\r\n\r\n",
				   Mess])};
	 _ ->
	     {error_out, "404"}
     end,
     next}.

decode_cgi(Cgi) ->
    decode_cgi(Cgi, []).

decode_cgi([$= | Rest], Var) ->
    decode_cgi2(Rest, Var, []);
decode_cgi([C | Rest], Var) ->
    decode_cgi(Rest, [C |Var]);
decode_cgi([], _) ->
    [].

decode_cgi2([$& | Rest], Var, Value) ->
    [{list_to_atom(lists:reverse(Var)), lists:reverse(Value)} |
     decode_cgi(Rest, [])];
decode_cgi2([$ | Rest], Var, Value) ->
    [{list_to_atom(lists:reverse(Var)), lists:reverse(Value)}];
decode_cgi2([C | Rest], Var, Value) ->
    decode_cgi2(Rest, Var, [C | Value]).





