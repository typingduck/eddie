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
%%%

%%%----------------------------------------------------------------------
%%% File    : misc.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created :  1 Jul 1998 by Joakim G. <jocke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(misc).
-author('jocke@erix.ericsson.se').
-export([uniq_sort/1,uniq_sort/2,uniq/1,uniq/2,multikeysearch/3,cmd/1]).

%% uniq_sort

uniq_sort(List) ->
  uniq(lists:sort(List)).

uniq_sort(N,List) ->
  uniq(N,lists:keysort(N,List)).

%% uniq

uniq([]) ->
  [];
uniq([First,First|Rest]) ->
  uniq([First|Rest]);
uniq([First|Rest]) ->
  [First|uniq(Rest)].

uniq(N,[]) ->
  [];
uniq(N,[First,Second|Rest]) when element(N,First) == element(N,Second) ->
  uniq(N,[First|Rest]);
uniq(N,[First|Rest]) ->
  [First|uniq(N,Rest)].

%% multikeysearch

multikeysearch(Keys,N,[]) ->
  false;
multikeysearch(Keys,N,[H|T]) ->
  case lists:member(element(N,H),Keys) of
    true ->
      {value,H};
    false ->
      multikeysearch(Keys,N,T)
  end.

%% cmd

cmd(String) -> % debug
  %%io:format("-- Command: ~s~n",[String]),
  "ok".

open_logger() ->
    Prog = filename:join(code:priv_dir(misc), "logger"),
    case catch open_port({spawn, Prog}, []) of
    Port when port(Port) -> {ok, Port};
    Error                -> {error, Error}
    end.

