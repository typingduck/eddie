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
%%% File    : erfs_delete_fs.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Delete an Eddie Replicated File System
%%% Created :  8 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_delete_fs).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([delete_fs/2]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func    : delete_fs/4
%% Args    : Erfs = name of erfs, Atom()
%%           Server = The gen_server Pid()
%% Purpose : Delete an Eddie Replicated File System
%% Returns : -
%%----------------------------------------------------------------------
delete_fs(Erfs, Server) ->
    case erfs_db:lookup(erfs, Erfs) of
	{ok, [Entry]} ->
	    NewEntry = Entry#erfs{state=deleting},
	    erfs_db:insert(erfs, Erfs),
	    delete_root(Erfs, Entry, Server);
	{ok, []} ->
	    erfs_utils:reply({"No such erfs", Erfs}, Server);
	{error, Error} ->
	    ?error(1, {"DB error", Error})
    end.


delete_root(Erfs, Entry, Server) ->
    case erfs:delete(Erfs, "/") of
	ok ->
	    %% ADD SUPPORT FOR NOT_SYNCED NODES
	    %% LIKE SAVING THE ERFS BUT SETTING STATE TO DELETED
	    Nodes = [Node || {Node, Root} <- erfs_utils:nodedefs(Erfs)],
	    erfs_db:delete_table(Erfs, Nodes),
	    erfs_db:delete(erfs, Erfs),
	    erfs_utils:reply(ok, Server);
	{error, Error} ->
	    erfs_utils:reply({"Couldn't delete root dir", Error}, Server)
    end.

