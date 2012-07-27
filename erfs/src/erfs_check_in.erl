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
%%% File    : erfs_check_in.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Check in a file or directory in an erfs
%%% Created : 14 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_check_in).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([check_in/3]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func    : check_in/3
%% Args    : Erfs = name of erfs, Atom()
%%           Path = Path to a directory or file, List()
%%           Server = The gen_server Pid()
%% Purpose : Check in a file or directory in an erfs
%% Returns : -
%%----------------------------------------------------------------------
check_in(Erfs, Path, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case erfs_utils:fix_path(Path) of
		{ok, NewPath} ->
		    Reply = ok_to_check_in(Erfs, NewPath, Server),
		    Server ! {erfs, self(), Reply},
		    unlink(Server);
		{error, Error} ->
		    ?error(1, {"Path error", Error})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.
    
    
ok_to_check_in(Erfs, Path, Server) ->
    Node = node(),
    case check_and_set(Erfs, Path, {checked_out, Node}, 
		       {checking_in, Node}) of
	ok ->
	    start(Erfs, Path, Node, Server);
	{Error, File} ->
	    ?error(1, {"Can't check in", File, Error})
    end.

start(Erfs, Path, Node, Server) ->
    RootDir = erfs_utils:root_dir(Erfs, Node),
    FullPath = erfs_file:join_paths(RootDir, Path),
    Info = erfs_file:read_info(FullPath),	% file_info record
    Type = Info#file_info.type,
    Size = Info#file_info.size,
    Mtime = Info#file_info.mtime,
    case erfs_db:lookup(Erfs, Path) of
	{ok, []} ->
	    Enode = #enode{path=Path, type=Type, size=Size, mtime=Mtime,
			   origin=Node, latest=Node, 
			   state={checking_in, Node}},
	    erfs_db:insert(Erfs, Enode),
	    update_parent(Erfs, Path, Enode),
	    if 
		Type == directory ->
		    NewEnode = make_dir(Erfs, Path, Enode, Node, Server),
		    erfs_db:insert(Erfs, NewEnode),
		    cont(Erfs, Path, NewEnode, FullPath, Node, Server);
		true ->
		    cont(Erfs, Path, Enode, FullPath, Node, Server)
	    end;
	{ok, [Enode]} ->
	    NewEnode = Enode#enode{type=Type, size=Size, mtime=Mtime,
				   latest=Node, 
				   state={checking_in, Node}},
	    erfs_db:insert(Erfs, NewEnode),
	    cont(Erfs, Path, NewEnode, FullPath, Node, Server)
    end.


make_dir(Erfs, Path, Enode, Node, Server) ->
    NodeDefs = [NodeDef || NodeDef <- erfs_utils:nodedefs(Erfs),
			   element(1, NodeDef) /= Node],
    {Ok, NotOk} = create_dir(NodeDefs, Path, [], []),
    NewEnode = Enode#enode{not_synced=NotOk},
    erfs_db:insert(Erfs, NewEnode),
    NewEnode.
    

create_dir([{Node, Root}| NodeDefs], Path, OkNodes, NotOkNodes) ->
    Dir = erfs_file:join_paths(Root, Path),
    case erfs_file:make_dir({Node, Dir}) of
	ok ->
	    create_dir(NodeDefs, Path, OkNodes ++ [Node], NotOkNodes);
	{error, Error} ->
	    ?debug(9, "~p~n", [Error]),
	    create_dir(NodeDefs, Path, OkNodes, NotOkNodes ++ [Node])
    end;
create_dir([], Path, OkNodes, NotOkNodes) ->
    {OkNodes, NotOkNodes}.


update_parent(Erfs, Path, Enode) -> 
    FileName = filename:basename(Path),
    Parent = filename:dirname(Path),
    {ok, [PEnode]} =  erfs_db:lookup(Erfs, Parent),
    Content = PEnode#enode.content,
    NewPEnode = PEnode#enode{content=Content ++ [FileName]},
    erfs_db:insert(Erfs, NewPEnode).


cont(Erfs, Path, Enode, FullPath, Node, Server) ->
    case Enode#enode.type of
	directory ->
	    check_in_dir(Erfs, Path, Enode, FullPath, Node, Server);
	Type ->
	    transfer(Erfs, Path, Enode, FullPath, Node, Server)
    end.


check_in_dir(Erfs, Path, Enode, FullPath, Node, Server) ->
    case erfs_file:list_dir(FullPath) of
	{ok, Content} ->
	    Purged = [X || X <- Enode#enode.content, 
			   not(lists:member(X, Content))],
	    purge_deleted(Purged, Erfs, Path, Node, Server),
	    check_in_content(Content, Erfs, Path, Node, Server),
	    NewEnode = Enode#enode{state=ok, content=Content},
	    erfs_db:insert(Erfs, NewEnode),
	    ok;
	{error, Error} ->
	    {"Can't process dir", Error}
    end.

purge_deleted([File| Rest], Erfs, Path, Node, Server) ->
    FilePath = erfs_file:join_paths(Path, File),
    check_and_set(Erfs, FilePath, {checking_in, Node}, ok),
    erfs:delete(Erfs, FilePath),
    purge_deleted(Rest, Erfs, Path, Node, Server);
purge_deleted([], Erfs, Path, Node, Server) ->
    true.
    



check_in_content([File| Rest], Erfs, Path, Node, Server) ->
    FilePath = erfs_file:join_paths(Path, File),
    start(Erfs, FilePath, Node, Server),
    check_in_content(Rest, Erfs, Path, Node, Server);
check_in_content([], Erfs, Path, Node, Server) ->
    true.


transfer(Erfs, Path, Enode, FullPath, Node, Server) ->
    NodeDefs = [NodeDef || NodeDef <- erfs_utils:nodedefs(Erfs),
			   element(1, NodeDef) /= Node],
    FileName = filename:basename(Path),
    {OkNodes, NotOkNodes} = to_tmp(NodeDefs, FullPath, FileName, [], []),
    NewEnode = Enode#enode{not_synced=NotOkNodes,
			   state={in_tmp, node()}},
    erfs_db:insert(Erfs, NewEnode),
    transfer_to_ord(Erfs, Path, Enode, FileName,
		    NodeDefs, OkNodes, NotOkNodes, Server).


transfer_to_ord(Erfs, Path, Enode, FileName, NodeDefs, Ok, NotOk, Server) ->
    NewNodeDefs = [NodeDef || NodeDef <- NodeDefs, 
			      lists:member(element(1, NodeDef), Ok)],
    {OkNodes, NotOkNodes} = to_ord(NewNodeDefs, Path, FileName, [], []),
    NewEnode = Enode#enode{not_synced=NotOk ++ NotOkNodes,
			   state={in_ord, node()}},
    erfs_db:insert(Erfs, NewEnode),
    last(Erfs, Path, Enode, Server).


to_tmp([{Node, Root} | NodeDefs], Path, FileName, Ok, NotOk) ->
    TmpFile = erfs_file:join_paths([Root, ?TMP_DIR, FileName]),
    case erfs_file:copy(Path, {Node, TmpFile}) of
	ok ->
	    to_tmp(NodeDefs, Path, FileName, [Node | Ok], NotOk);
	{error, Error} ->
	    ?debug(2, "~p~n", [Error]),
	    to_tmp(NodeDefs, Path, FileName, Ok, [Node | NotOk])
    end;
to_tmp([], Path, FileName, Ok, NotOk) ->
    {Ok, NotOk}.


to_ord([{Node, Root} | NodeDefs], Path, FileName, Ok, NotOk) ->
    TmpFile = erfs_file:join_paths([Root, ?TMP_DIR, FileName]),
    OrdFile = erfs_file:join_paths([Root, Path]),
    case erfs_file:copy({Node, TmpFile}, {Node, OrdFile}) of
	ok ->
	    erfs_file:delete({Node, TmpFile}),
	    to_ord(NodeDefs, Path, FileName, [Node | Ok], NotOk);
	{error, Error} ->
	    ?debug(2, "~p~n", [Error]),
	    to_ord(NodeDefs, Path, FileName, Ok, [Node | NotOk])
    end;
to_ord([], Path, FileName, Ok, NotOk) ->
    {Ok, NotOk}.


last(Erfs, Path, Enode, Server) ->
    NewEnode = Enode#enode{state=ok, latest=node()},
    erfs_db:insert(Erfs, NewEnode),
    ok.

%%----------------------------------------------------------------------
%% Func    : check_and_set/4
%% Args    : Erfs = name of erfs, Atom()
%%           Path = Path to a directory or file, List()
%%           Req = Required status
%%           New = New status
%% Purpose : Check that the current status is Req and set it to New
%% Returns : ok | {Error, Where}
%%----------------------------------------------------------------------
check_and_set(Erfs, Path, Req, New) ->
    case traverse(Erfs, Path, Req, New) of
	ok ->
	    ok;
	{Error, File} ->
	    traverse(Erfs, Path, New, Req),
	    {Error, File}
    end.


traverse(Erfs, Path, Req, New) ->	
    case check_and_set_db(Erfs, Path, Req, New) of
	{ok, Enode} when record(Enode, enode) ->
	    case Enode#enode.type of
		directory ->
		    trav(Enode#enode.content, Erfs, Path, Req, New);
		Type ->
		    ok
	    end;
	{error, non_existant} ->
	    ok;
	Error ->
	    {Error, Path}
    end.

trav([File| Rest], Erfs, Path, Req, New) ->
    FilePath = erfs_file:join_paths(Path, File),
    case traverse(Erfs, FilePath, Req, New) of
	ok ->
	    trav(Rest, Erfs, Path, Req, New);
	Other ->
	    Other
    end;
trav([], Erfs, Path, Req, New) ->
    ok.


check_and_set_db(Erfs, Path, Req, New) ->
    Fun = fun() -> 
	      case  mnesia:read(Erfs, Path, write) of
		  [Enode] when Enode#enode.state == Req ->
		      mnesia:write(Erfs, Enode#enode{state=New}, write),
		      {ok, Enode};
		  [Enode] ->
		      {error, Enode#enode.state};
		  [] ->
		      {error, non_existant}
	      end
	  end,
    case mnesia:transaction(Fun) of
	{atomic, Reply} ->
	    Reply;
	{aborted, Reason} ->
	    {error, mnesia:error_description(Reason)}
    end.
