%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.eddieware.org/EPL
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

-module(erlet_status).
-author('patrik@punsch').
-vc('$Id: erlet_status.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').

-include("inet_server.hrl").

-export([load/2, store/2, do/2]).


load(Row, Data) ->
    ok.

store(Erlets_name, Erlet_config) ->
    ok.

do(State, Data) ->
    case lists:flatten(Data) of
	[$G,$E,$T,$ ,$/,$e, $d, $d, $i, $e, _,$s,$t,$a,$t,$u,$s | Rest] ->
	    Decoded =
		case Rest of
		    [$?|Others] ->
			decode_cgi(Others);
		    _ ->
			decode_cgi([])
		end,

	    Reload =
		case lists:keysearch(reload, 1, Decoded) of
		    {_, {_, Reload_time}} ->
			Reload_time;
		    _ ->
			no_reload
		end,
	    Mess =
		lists:append(["<HTML>",
			      case Reload of
				  no_reload ->
				      "<META HTTP-EQUIV=\"Refresh\" CONTENT=\"10\">";
				  _ ->
				      "<META HTTP-EQUIV=\"Refresh\" CONTENT=\"" ++
					  Reload ++
					  "\">"
			      end,
			      "<BODY BGCOLOR=\"#FFFFFF\">",
			      "<PRE>",
			      inet_server:status(html),
			      "</PRE>",
			      "<CENTER>"
			      "<A HREF=\"http://www.eddieware.org\">"
			      "Site Load Control From Eddie</A><BR>"
			      "</CENTER>"
			      "</BODY>"
			      "</HTML>"]),

	    Date = srv_parse:enc_current_date(),
	    Length = integer_to_list(length(Mess)),
		{State,
		 {send, lists:append([
				      "HTTP/1.0 200 OK\r\n"
				      "Server: Eddie-gateway/1.4\r\n"
				      "Date: ",
				      Date,
				      "\r\n"
				      "Expires: ",
				      Date,
				      "\r\n"
				      "Connection: close\r\n",
				      "Content-Type: text/html\r\n",
				      "Content-Length: ",
				      integer_to_list(length(Mess)),"\r\n",
				      "\r\n",
				      Mess])}, next};
	_ ->
	    {State, {error_out, "404"}, next}
    end.

decode_cgi([]) ->
    [];
decode_cgi([$ |_]) ->
    [];
decode_cgi([$?|Rest])->
    decode_cgi1(Rest).

decode_cgi1(Cgi) ->
    decode_cgi1(Cgi, []).

decode_cgi1([$= | Rest], Var) ->
    decode_cgi2(Rest, Var, []);
decode_cgi1([C | Rest], Var) ->
    decode_cgi1(Rest, [C |Var]);
decode_cgi1([], _) ->
    [].

decode_cgi2([$& | Rest], Var, Value) ->
    [{list_to_atom(lists:reverse(Var)), lists:reverse(Value)} |
     decode_cgi1(Rest, [])];
decode_cgi2([$ | Rest], Var, Value) ->
    [{list_to_atom(lists:reverse(Var)), lists:reverse(Value)}];
decode_cgi2([C | Rest], Var, Value) ->
    decode_cgi2(Rest, Var, [C | Value]);
%% 
decode_cgi2([],Var,Value) ->
    [].




