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
%%% The Original Code is Erfs-0.2b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (c), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : erfs.hrl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Erlang Replicated File System header file
%%% Created :  2 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------


-include_lib("kernel/include/file.hrl").


-record(erfs, {name, nodedefs, created, options=[], not_synced=[], state}).

-record(enode, {path, type, size, mtime, checksum=not_used, options=[],
		origin, state, latest, not_synced=[], content=[]}).

%%-define(DEBUG, Level).

-ifdef(DEBUG).
-define(debug(Lvl, Str, X),
	if ?DEBUG >= Lvl ->
		io:format("Erfs == Mod:~w line:~w ", [?MODULE, ?LINE]),
		io:format(Str, X);
	   true ->
		true
	end).
-else.
-define(debug(Lvl, X, Y), true).
-endif.


-define(error(Lvl, Reason), 
	?debug(Lvl, "~p~n", [Reason]),
	exit(Reason)).


-define(TMP_DIR, ".erfstemp").
