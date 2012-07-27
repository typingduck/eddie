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
%%% File    : config_file.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 23 Jun 1998 by Joakim G. <jocke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(config_file).
-author('jocke@erix.ericsson.se').
-export([parse/1]).

%%%----------------------------------------------------------------------
%%% Function : parse/1
%%% Usage    : config_file:parse(ConfigFile) -> {ok,Context} | {error,Reason}
%%% Types    : ConfigFile=file_path()
%%%            Context=[{Row,Key,[Value]}|{Row,Key,[Value],Context}]
%%%            Key=Value=Reason=string()
%%%            Row=int()
%%%----------------------------------------------------------------------

parse(ConfigFile) ->
  case file:open(ConfigFile,[read]) of
    {ok,Stream} ->
      read(Stream);
    {error,Reason} ->
      {error,file:format_error(Reason)}
  end.

read(Stream) ->
  put(row,0),
  case catch read(Stream,{0,root,[],[]}) of
    {error,Reason} ->
      {error,Reason};
    Body ->
      {ok,Body}
  end.

read(Stream,{Row,Key,Values,Body}) ->
  case encode(get_row(Stream,[])) of
    {same_context,NewKey,NewValues} ->
      read(Stream,{Row,Key,Values,Body++[{get(row),NewKey,NewValues}]});
    {start_context,NewKey,NewValues} ->
      read(Stream,{Row,Key,Values,Body++
		   [read(Stream,{get(row),NewKey,NewValues,[]})]});
    {stop_context,Key} ->
      {Row,Key,Values,Body};
    {stop_context,WrongKey} ->
      io:format("Error in nesting on line ~p~n", [integer_to_list(get(row))]),
      throw({error,
	     "Error in nesting on line "++integer_to_list(get(row))});
    skip ->
      read(Stream,{Row,Key,Values,Body});
    eof when Key == root ->
      Body;
    eof ->
      io:format("Unexpected end of file~n", []),
      throw({error,"Unexpected end of file."})
  end.

get_row(Stream,MultiLine) ->
  put(row,get(row)+1),
  case io:get_line(Stream,'') of
    eof ->
      eof;
    Line ->
      case lists:reverse(Line) of
	[$\n,$\\|Rest] ->
	  get_row(Stream,Rest++MultiLine);
	_ ->
	  lists:reverse(MultiLine)++Line
      end
  end.

encode(eof) ->
  eof;
encode([$#|Rest]) ->
  skip;
encode([$\n|Rest]) ->
  skip;
encode([$ |Rest]) ->
  encode(Rest);
encode([$\t|Rest]) ->
  encode(Rest);
encode([$<,$/|Rest]) ->
  {Key,_}=encode_key(Rest),
  {stop_context,Key};
encode([$<|Rest]) ->
  {Key,ValuesRest}=encode_key(Rest),
  {start_context,Key,encode_values(ValuesRest)};
encode([Char|Rest]) ->
  {Key,ValuesRest}=encode_key([Char|Rest]),
  {same_context,Key,encode_values(ValuesRest)}.

encode_key(Key) ->
  encode_key(Key,[]).

encode_key([],Key) ->
  {Key,[]};
encode_key([$ |Rest],Key) ->
  {Key,Rest};
encode_key([$\t|Rest],Key) ->
  {Key,Rest};
encode_key([$\n|Rest],Key) ->
  {Key,Rest};
encode_key([$>|Rest],Key) ->
  {Key,Rest};
encode_key([Char|Rest],Key) ->
  encode_key(Rest,Key++[tolower(Char)]).

encode_values(Values) ->
  encode_values(Values,word,[],[]).

encode_values([],word,CurrentValue,Values) ->
  lists:reverse(Values);
encode_values([],sentence,CurrentValue,Values) ->
  throw({error,"Runaway quotation on line "++integer_to_list(get(row))});
encode_values([$ |Rest],word,[],Values) ->
  encode_values(Rest,word,[],Values);
encode_values([$\t|Rest],word,[],Values) ->
  encode_values(Rest,word,[],Values);
encode_values([$\n|Rest],word,[],Values) ->
  encode_values(Rest,word,[],Values);
encode_values([$>|Rest],word,[],Values) ->
  encode_values(Rest,word,[],Values);
encode_values([$ |Rest],word,CurrentValue,Values) ->
  encode_values(Rest,word,[],[lists:reverse(CurrentValue)|Values]);
encode_values([$\t|Rest],word,CurrentValue,Values) ->
  encode_values(Rest,word,[],[lists:reverse(CurrentValue)|Values]);
encode_values([$\n|Rest],word,CurrentValue,Values) ->
  encode_values(Rest,word,[],[lists:reverse(CurrentValue)|Values]);
encode_values([$>|Rest],word,CurrentValue,Values) ->
  encode_values(Rest,word,[],[lists:reverse(CurrentValue)|Values]);
encode_values([$"|Rest],word,[],Values) ->
  encode_values(Rest,sentence,[$"],Values);
encode_values([$"|Rest],sentence,CurrentValue,Values) ->
  encode_values(Rest,word,[],[tl(lists:reverse(CurrentValue))|Values]);
encode_values([Char|Rest],WordOrSentence,CurrentValue,Values) ->
  encode_values(Rest,WordOrSentence,[Char|CurrentValue],Values).

tolower(C) when $A =< C, C =< $Z ->
  $a - $A + C;
tolower(C) ->
  C.
