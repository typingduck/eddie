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
%%% File    : db.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 23 Jun 1998 by Joakim G. <jocke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(db).
-author('jocke@erix.ericsson.se').
-export([eval/1,read/1,write/1,delete/1,delete_object/1,all_keys/1,
	 wait_for_tables/2]).

-include_lib("misc/include/logger.hrl").

%% eval

eval(Query) ->
  case mnesia:transaction(fun() -> mnemosyne:eval(Query) end) of
    {atomic,Value} ->
      Value;
    {aborted,Reason} ->
      ?FATAL(?F("Event: Transaction failed~n"
		"Reason: ~p", [mnesia:error_description(Reason)])),
      undefined
  end.

%% read

read(Oid) ->
  mnesia:dirty_read(Oid).

%% write

write(Object) ->
  mnesia:dirty_write(Object).

%% delete

delete(Oid) ->
  mnesia:dirty_delete(Oid).

%% delete_object

delete_object(Object) ->
  mnesia:dirty_delete_object(Object).

%% all_keys

all_keys(Tab) ->
  mnesia:dirty_all_keys(Tab).

%% wait_for_tables

wait_for_tables(Tables,Timeout) ->
  case mnesia:wait_for_tables(Tables,Timeout) of
    ok ->
      loaded;
    {timeout,BadTables} ->
      lists:foreach(fun(BadTable) ->
			case mnesia:force_load_table(BadTable) of
			  yes ->
			    ?INFO(?F("Event: Table ~w force loaded",
				     [BadTable])),
			    force_loaded;
			  ErrorDescription ->
			    ?FATAL(?F("Event: Table ~w not loaded",
				      [BadTable])),
			    not_loaded
			end
		    end,BadTables);
    {error,Reason} ->
      ?FATAL(?F("Event: Tables not loaded~n"
		"Reason: ~p",
		[Reason])),
      not_loaded
  end.
