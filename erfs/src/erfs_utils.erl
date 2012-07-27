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
%%% File    : erfs_utils.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Erlang Replicated File System utilities
%%% Created :  2 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_utils).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([fix_path/1, check_and_set/4, nodedefs/1, 
	 valid_erfs/1, reply/2, root_dir/2]).

-define(DEBUG, 1).

-include("erfs.hrl").


%%----------------------------------------------------------------------
%% Func    : valid_erfs/1
%% Args    : Erfs = An erfs, Atom()
%% Purpose : Check to see if Erfs is an erfs
%% Returns : valid | {invalid, Reason}
%%----------------------------------------------------------------------
valid_erfs(Erfs) ->
    case erfs_db:lookup(erfs, Erfs) of
	{ok, [Entry]} ->
	    valid;
	{ok, []} ->
	    {invalid, {"no such erfs", Erfs}};
	{error, Error} ->
	    {invalid, {"DB error", Error}}
    end.


%%----------------------------------------------------------------------
%% Func    : fix_path/1
%% Args    : Root = Path to a directory or file, List()
%% Purpose : Check and correct a path, should be absolute, with /
%%           and without a trailing slash
%% Returns : {ok, FixedPath} | {error, Error}
%%----------------------------------------------------------------------
fix_path(Path) ->		  
    case Path of
	[$/ | Rest] ->
	    {ok, rm_trailing_slash(Path)};
	Path ->
	    {error, {"relative_path", Path}}
    end.


replace_backslash([$/])   -> [];
replace_backslash([$\\])   -> [];
replace_backslash([E])    -> [E];
replace_backslash([$\\|T]) -> [$/ | replace_backslash(T)];
replace_backslash([H|T])  -> [H | replace_backslash(T)].


rm_trailing_slash([E])  -> [E];
rm_trailing_slash(Path) -> rts(Path).

rts([$/])  -> [];
rts([E])   -> [E];
rts([H|T]) -> [H | rm_trailing_slash(T)].


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


%%----------------------------------------------------------------------
%% Func    : nodedefs/1
%% Args    : Erfs = name of erfs, Atom()
%% Purpose : Extract the node definitions from the erfs def.
%% Returns : [NodeDef] | {error, Error}
%%----------------------------------------------------------------------
nodedefs(Erfs) ->
    case erfs_db:lookup(erfs, Erfs) of
	{ok, [Entry]} ->
	    Entry#erfs.nodedefs;
	{ok, []} ->
	    {error, {"Can't find erfs", Erfs}};
	{error, Error} ->
	    {error, {"DB error", Error}}
    end.


%%----------------------------------------------------------------------
%% Func    : root_dir/2
%% Args    : Erfs = name of erfs, Atom()
%%           Node = node()
%% Purpose : Extract the rootdir for a specific node
%% Returns : Path() | {error, Error}
%%----------------------------------------------------------------------
root_dir(Erfs, Node) ->
    case lists:keysearch(Node, 1, nodedefs(Erfs)) of
	{value, {_, Root}} ->
	    Root;
	Other ->
	    {error, Other}
    end.


%%----------------------------------------------------------------------
%% Func    : reply/2
%% Args    : Reply = The reply to the server, Any()
%%           Server = The gen_server Pid()
%% Purpose : Send a reply to the server and unlink
%% Returns : -
%%----------------------------------------------------------------------
reply(Reply, Server) ->	    
    Server ! {erfs, self(), Reply},
    unlink(Server).
