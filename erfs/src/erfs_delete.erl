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
%%% File    : erfs_delete.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Delete a file or directory from an erfs
%%% Created : 14 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_delete).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([delete/3]).

-define(DEBUG, 1).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func    : delete/2
%% Args    : Erfs = name of erfs, Atom()
%%           Path = Path to a directory or file, List()
%%           Server = The gen_server Pid()
%% Purpose : Delete a file or directory from an erfs
%% Returns : -
%%----------------------------------------------------------------------

delete(Erfs, Path, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case erfs_utils:fix_path(Path) of
		{ok, NewPath} ->
		    Reply = ok_to_delete(Erfs, NewPath, Server),
		    Server ! {erfs, self(), Reply},
		    unlink(Server);
		{error, Error} ->
		    ?error(1, {"Path error", Error})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.


ok_to_delete(Erfs, Path, Server) ->
    case erfs_utils:check_and_set(Erfs, Path, ok, {deleting, node()}) of
	ok ->
	    cont(Erfs, Path, Server);
	{Error, File} ->
	    ?error(1, {"Can't delete file", File, Error})
    end.


cont(Erfs, Path, Server) ->
    {ok, [Enode]} = erfs_db:lookup(Erfs, Path),
    case Enode#enode.type of
	directory ->
	    delete_dir(Enode#enode.content, Erfs, Path, Server);
	Other ->
	    true
    end,
    NodeDefs = erfs_utils:nodedefs(Erfs),
    case Enode#enode.not_synced of
	[] ->
	    delete_object(NodeDefs, Path, Erfs, Server),
	    erfs_db:delete(Erfs, Path);
	NotSynced ->
	    NewNodeDefs = [{Node, Root} || 
			      {Node, Root} <- NodeDefs,
			      not(lists:member(Node, NotSynced))],
	    delete_object(NewNodeDefs, Path, Erfs, Server),
	    NewEnode = Enode#enode{state={deleted, node()}},
	    erfs_db:insert(Erfs, NewEnode)
    end,
    ok.


delete_object([{Node, Root}| Rest], Path, Erfs, Server) ->
    FilePath = erfs_file:join_paths(Root, Path),
    case erfs_file:delete({Node, FilePath}) of
	ok ->
	    delete_object(Rest, Path, Erfs, Server);
	{error, Error} ->
	    ?debug(3, "Couldn't delete ~p on node ~p due to ~p~n", 
		   [Path, Node, Error]),
	    delete_object(Rest, Path, Erfs, Server)
    end;
delete_object([], Path, Erfs, Server) ->
    update_parent(Erfs, Path),
    ok.


delete_dir([File| Rest], Erfs, Path, Server) ->
    FilePath = erfs_file:join_paths(Path, File),
    cont(Erfs, FilePath, Server),
    delete_dir(Rest, Erfs, Path, Server);
delete_dir([], Erfs, Path, Server) ->
    ok.

update_parent(Erfs, Path) ->
    FileName = filename:basename(Path),
    Parent = filename:dirname(Path),
    case erfs_db:lookup(Erfs, Parent) of
	{ok, [PEnode]} ->
	    Content = PEnode#enode.content,
	    NewPEnode = PEnode#enode{content=Content -- [FileName]},
	    erfs_db:insert(Erfs, NewPEnode);
	Error ->
	    ?debug(2, "Can't update parent ~p due to ~p~n", [Parent, Error])
    end.
