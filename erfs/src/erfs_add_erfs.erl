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
%%% File    : erfs_add_erfs.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Add a node to an Eddie Replicated File System
%%% Created : 14 Oct 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_add_erfs).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([add_erfs/3]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func    : add_erfs/2
%% Args    : NodeDef = {Node, Root}, where
%%                     Node = name of node, Node()
%%                     Root = path to root directory, List()
%%           Erfs = name of erfs, Atom()
%%           Server = The gen_server Pid()
%% Purpose : Add a node to an erfs
%% Returns : -
%%----------------------------------------------------------------------
add_erfs(Erfs, {Node, Root}, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case net_adm:ping(Node) of
		pong ->
		    case erfs_utils:fix_path(Root) of
			{ok, NewRoot} ->
			    {ok, [Entry]} = erfs_db:lookup(erfs, Erfs),
			    case lists:keymember(Node, 1, 
						 Entry#erfs.nodedefs) of
				false ->
				    check_replica(Erfs, Node, NewRoot, Server);
				true ->
				    ?error(1, "Erfs exist")
			    end;
			{error, Error} ->
			    ?error(1, {"Path error", Error})
		    end;
		pang ->
		    ?error(2, {"Can't reach node", Node})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.


check_replica(Erfs, Node, Root, Server) ->
    case lists:member(Node, mnesia:system_info(running_db_nodes)) of
	true ->
	    ok_to_add_erfs(Erfs, Node, Root, Server);
	false ->
	    ?error(2, {"No replica on node", Node})
    end.


ok_to_add_erfs(Erfs, Node, Root, Server) ->
    case erfs_utils:check_and_set(Erfs, "/", ok, 
				  {{adding_node, Node}, node()}) of
	ok ->
	    cont(Erfs, Node, Root, Server);
	{Error, File} ->
	    ?error(1, {"Can't add the node ", Error})
    end.


cont(Erfs, Node, Root, Server) ->
    LRoot = erfs_utils:root_dir(Erfs, node()),
    replicate("/", Node, Root, LRoot, Erfs, Server),
    {ok, [Entry]} = erfs_db:lookup(erfs, Erfs),
    NewEntry = Entry#erfs{nodedefs=Entry#erfs.nodedefs ++ [{Node, Root}]},
    erfs_db:insert(erfs, NewEntry),
    erfs_utils:check_and_set(Erfs, "/", {{adding_node, Node}, node()}, ok),
    erfs_utils:reply(ok, Server).


replicate(Path, Node, Root, LRoot, Erfs, Server) ->
    {ok, [Enode]} =  erfs_db:lookup(Erfs, Path),
    case Enode#enode.type of
	directory ->
	    copy_dir(Enode, Path, Node, Root, LRoot, Erfs, Server);
	Type ->
	    copy_file(Enode, Path, Node, Root, LRoot, Erfs, Server)
    end.

replicate_dir([File | Rest], Path, Node, Root, LRoot, Erfs, Server) ->
    FilePath = erfs_file:join_paths(Path, File),
    replicate(FilePath, Node, Root, LRoot, Erfs, Server),
    replicate_dir(Rest, Path, Node, Root, LRoot, Erfs, Server);
replicate_dir([], Path, Node, Root, LRoot, Erfs, Server) ->
    ok.

    


copy_dir(Enode, Path, Node, Root, LRoot, Erfs, Server) ->
    FullPath = erfs_file:join_paths(Root, Path),
    case erfs_file:make_dir({Node, FullPath}) of
	ok ->
	    replicate_dir(Enode#enode.content, Path, Node, Root, LRoot, 
			  Erfs, Server);
	{error, Error} ->
	    erfs_file:del_dir({Node, Root}, recurse),
	    ?error(2, {"Couldn't copy dir", {Node, FullPath}, Error})
    end.


copy_file(Enode, Path, Node, Root, LRoot, Erfs, Server) ->
    FullPath = erfs_file:join_paths(Root, Path),
    LocalPath = erfs_file:join_paths(LRoot, Path),
    case erfs_file:copy(LocalPath, {Node, FullPath}) of
	ok ->
	    ok;
	{error, Error} ->
	    erfs_file:del_dir({Node, Root}, recurse),
	    ?error(2, {"Couldn't copy file", {Node, FullPath}, Error})
    end.
