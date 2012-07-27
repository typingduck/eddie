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

-module(is_service_db).
-author('patrik@elrond').

-define(DEFAULT_TYPE, bag).
-define(DEBUG(X), io:format(lists:concat([?MODULE, ":~p\n"]), [X])).
-export([new/1, new/2, insert/2, lookup/2, delete/1, delete/2]).

new(Name) ->
    new(Name, ?DEFAULT_TYPE).

new(Name, Types) ->
    ets:new(Name, Types).

insert(Database_ref, Value) ->
    ets:insert(Database_ref, Value).

lookup(Database_ref, Key) ->
    ets:lookup(Database_ref, Key).

delete(Database_ref, Key) ->
    ets:delete(Database_ref, Key).

delete(Database_ref) ->
    ets:delete(Database_ref).
