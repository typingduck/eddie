-module(http_parse).
-author('patrik@erix.ericsson.se').
-modified_by('jon@eddiware.com'). % restructuring and updating to HTTP/1.1
-modified_by('eric.yeo@ericsson.com.au').
%%%----------------------------------------------------------------------
%%% File    : http_parse.erl
%%% Author  : Patrik Winroth <patrik@erix.ericsson.se>
%%% Created : 06 Oct 1998
%%% Purpose : Protocol Module for implementing an intelligent
%%%           Gateway for HTTP/1.0 and HTTP/1.1.
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
%%% Contributor(s): ______________________________________.
%%%----------------------------------------------------------------------
-id('$Id: http_parse.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/inet_server/src/http_parse.erl,v $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:25 $ ').
-state('$State: Exp $ ').

%% External exports
%% used by module is_core (through module http)
-export([schedule_match/2]).


%%----------------------------------------------------------------------
%% Function  : schedule_match/2
%% Arguments : Pattern (the parsed Method, Uri, Version and all
%%             Header key/value fields).
%%             Patterns (e.g. [{method, {["HTTP/1.0", "HTTP/1.1"], []}},
%%                             {uri, {["/eddie_status"], []}}]  ).
%% Returns   : true | false
%% Purpose   : checks whether one of {Key, Patterns} matches the parsed
%%             request.  
%%----------------------------------------------------------------------
schedule_match(Parsed_headers, List_of_patterns) ->
    %% Answers Q: It was not found | anti patterned.
    case lists:any(fun({Key, {Patterns, Anti_patterns}}) ->
			   check_key(Key,Parsed_headers,Patterns,Anti_patterns)
		   end, List_of_patterns) of
	true ->
	    false;
	_ ->
	    true
    end.

%% special case for URI. Eddie matches URI's with for example /eddie and
%% not http://www.somewhere.com/eddie. So we need to remove http://...
%% before checking the URI
check_key(uri,Parsed_headers,Patterns,Anti_patterns) ->
    case lists:keysearch(uri,1,Parsed_headers) of
	{_,{_,Uri}} ->
	    case regexp:sub(Uri,"http://[^/]*","") of
		{ok,New_Uri,_} ->
		    check_value_match(New_Uri,Patterns,Anti_patterns);
		_ -> % error occured
		    true
	    end;
	_ -> % uri key is not in the parsed headers (Does this _ever_ occur?)
	    check_value_match([],Patterns,Anti_patterns)
    end;
%% Check if the parsed headers contains the key, and if it does see if
%% it matched the Patterns and not the Anti_patterns
check_key(Key,Parsed_headers,Patterns,Anti_patterns) ->
    case lists:keysearch(Key, 1, Parsed_headers) of
	{_,{_, Value}} ->
	    check_value_match(Value,Patterns,Anti_patterns);
	_ -> %% this key is not in the parsed headers, check anyway
	    %% Because one might just anti-pattern something and thus
	    %% if the header is not there, you are not anti-patterned
	    %% The check has to be done because one can still pattern
	    %% specific patterns..
	    check_value_match([],Patterns,Anti_patterns)
    end.

%% Check if the value of the keys match the Patterns and not the
%% Anti_patterns
check_value_match(Value,Patterns,Anti_patterns) ->
    case lists:any(fun(Anti_1) ->
			   first_part(Anti_1, Value)
		   end, Anti_patterns) of
	true -> %% anti-pattern match = NO MATCH
	    true;
	_ ->
	    case lists:any(fun(Pattern1) ->
				   first_part(Pattern1, Value)
			   end, Patterns) of
		true -> %% pattern match and no anti-pattern match = MATCH
		    false;
		_ ->
		    %% We might have only anti-patterns (no patterns)
		    if Patterns==[] -> false; %% no anti-pattern match = MATCH
		       true -> %% does not match the patterns = NO MATCH
			    true
		    end
	    end
    end.


%% The parsed header values might be atoms, convert and check
first_part(P,V) when atom(V) ->
    first_part(P,atom_to_list(V));
%% The parsed header values might be tuples, convert and check
first_part(P,V) when tuple(V) ->
    first_part(P,tuple_to_list(V));
%% String (list) check
first_part([C1 | Rest1], [C1 | Rest2]) ->
    first_part(Rest1, Rest2);
first_part([], _)->
    true;
first_part(_, _) ->
    false.
