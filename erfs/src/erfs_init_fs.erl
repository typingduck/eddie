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
%%% File    : erfs_init_fs.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Initiate a new Eddie Replicated File System
%%% Created :  7 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_init_fs).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([init_fs/4]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func    : init_fs/4
%% Args    : Erfs = name of erfs, Atom()
%%           NodeDefs = [{Node, Root}]
%%           Opts = not_used()
%%           Server = The gen_server Pid()
%% Purpose : Initiate a new Eddie Replicated File System
%% Returns : -
%%----------------------------------------------------------------------
init_fs(Erfs, NodeDefs, Opts, Server) ->
    case erfs_db:lookup(erfs, Erfs) of
	{ok, []} ->
	    Entry = #erfs{name=Erfs, nodedefs=NodeDefs, options=Opts,
			  created=erlang:localtime(), state={init, node()}},
	    erfs_db:insert(erfs, Entry),
	    Reply = check_nodedefs(Erfs, NodeDefs, Opts, Entry, Server),
	    erfs_utils:reply(Reply, Server);
	{ok, [Result]} ->
	    ?error(1, {erfs, Erfs, exists, Result});
	{error, Error} ->
	    ?error(1, {"DB error", Error})
    end.


check_nodedefs(Erfs, NodeDefs, Opts, Entry, Server) ->
    case fix_paths(NodeDefs, []) of
	{ok, NewNodeDefs} ->
	    create_fs(NewNodeDefs, Erfs, Opts, Entry, [], [], Server);
	{error, Error} ->
	    erfs_db:delete(erfs, Erfs),
	    Error
    end.


fix_paths([{Node, Path} | NodeDefs], NewNodeDefs) ->
    case erfs_utils:fix_path(Path) of
	{ok, NewPath} ->
	    fix_paths(NodeDefs, [{Node, NewPath} | NewNodeDefs]);
	{error, Error} ->
	    {error, Error}
    end;
fix_paths([], NewNodeDefs) ->
    {ok, NewNodeDefs}.


create_fs([NodeDef| NodeDefs], Erfs, Opts, Entry, Ok, NotOk, Server) ->
    case erfs_file:make_dir(NodeDef) of
	ok ->
	    Temp = erfs_file:join_paths(NodeDef, ?TMP_DIR),
	    case erfs_file:make_dir(Temp) of
		ok ->
		    create_fs(NodeDefs, Erfs, Opts, Entry, 
			      [NodeDef|Ok], NotOk, Server);
		{error, Error} ->
		    erfs_file:del_dir(NodeDef),
		    ?debug(2, "Couldn't make tmp dir ~p due to ~p~n", 
			   [NodeDef, Error]),
		    create_fs(NodeDefs, Erfs, Opts, Entry, 
			      Ok, [NodeDef| NotOk], Server)
	    end;
	{error, Error} ->
	    ?debug(2, "Couldn't make root dir ~p due to ~p~n", 
		   [NodeDef, Error]),
	    create_fs(NodeDefs, Erfs, Opts, Entry, 
		      Ok, [NodeDef| NotOk], Server)
    end;

create_fs([], Erfs, Opts, Entry, Ok, NotOk, Server) ->
    case erfs_db:create_table(Erfs, [Node || {Node, Root} <- Ok], 
			      enode, record_info(fields, enode)) of
	ok ->
	    Node = node(),
	    NewEntry = Entry#erfs{nodedefs=Ok, 
				  not_synced=NotOk,
				  state={created, Node}},
	    erfs_db:insert(erfs, NewEntry),
	    insert_enodes(Erfs, Node, Ok, NewEntry);
	{error, Reason} ->
	    erfs_db:delete(erfs, Erfs),
	    {"couldn't create table", Reason}
    end.


insert_enodes(Erfs, Node, NodeDefs, Entry) ->
    {value, {_, Root}} = lists:keysearch(Node, 1, NodeDefs),
    mk_enode(Erfs, "/", Root, Node, [?TMP_DIR]),
    TmpDir = erfs_file:join_paths("/", ?TMP_DIR),
    FullTmpDir = erfs_file:join_paths(Root, ?TMP_DIR),
    mk_enode(Erfs, TmpDir, FullTmpDir, Node, []),
    NewEntry = Entry#erfs{state=ok},
    erfs_db:insert(erfs, NewEntry),
    ok.
    
    
mk_enode(Erfs, Path, FullPath, Node, Content) ->    
    Info = erfs_file:read_info(FullPath),	% file_info record
    Type = Info#file_info.type,
    Size = Info#file_info.size,
    Mtime = Info#file_info.mtime,
    Enode = #enode{path=Path, type=Type, size=Size, mtime=Mtime,
		   content=Content, origin=Node, latest=Node, state=ok},
    erfs_db:insert(Erfs, Enode).
