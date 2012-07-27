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
%%% File    : erfs_status.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Status of a file or directory in an erfs
%%% Created :  5 Dec 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_status).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([status/3]).

-define(DEBUG, 1).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func    : status/3
%% Args    : Erfs = name of erfs, Atom()
%%           Path = Path to a directory or file, List()
%%           Server = The gen_server Pid()
%% Purpose : Status of a file or directory in an erfs
%% Returns : -
%%----------------------------------------------------------------------

status(Erfs, Path, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case erfs_utils:fix_path(Path) of
		{ok, NewPath} ->
		    Reply = get_status(Erfs, NewPath, Server, []),
		    Server ! {erfs, self(), Reply},
		    unlink(Server);
		{error, Error} ->
		    ?error(1, {"Path error", Error})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.


get_status(Erfs, Path, Server, Status) ->
    {ok, [Enode]} = erfs_db:lookup(Erfs, Path),
    case Enode#enode.type of
	directory ->
	    NewStatus = add_status(Enode#enode.state, Path, Status),
	    get_status_dir(Enode#enode.content, Erfs, Path, Server, NewStatus);
	Other ->
	    add_status(Enode#enode.state, Path, Status)
    end.


get_status_dir([File| Rest], Erfs, Path, Server, Status) ->
    FilePath = erfs_file:join_paths(Path, File),
    NewStatus = get_status(Erfs, FilePath, Server, Status),
    get_status_dir(Rest, Erfs, Path, Server, NewStatus);
get_status_dir([], Erfs, Path, Server, Status) ->
    Status.



add_status(State, Path, Status) ->
    case lists:keysearch(State, 1, Status) of
	{value, {_, Paths}} ->
	    lists:keyreplace(State, 1, Status, {State, Paths ++ [Path]});
	Other ->
	    Status ++ [{State, [Path]}]
    end.

		       
