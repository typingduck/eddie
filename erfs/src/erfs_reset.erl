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
%%% File    : erfs_reset.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Reset the state of a file or directory in an erfs to ok
%%% Created :  5 Dec 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_reset).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([reset/3]).

-define(DEBUG, 1).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func    : reset/3
%% Args    : Erfs = name of erfs, Atom()
%%           Path = Path to a directory or file, List()
%%           Server = The gen_server Pid()
%% Purpose : Reset the state of a file or directory in an erfs to ok
%% Returns : -
%%----------------------------------------------------------------------

reset(Erfs, Path, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case erfs_utils:fix_path(Path) of
		{ok, NewPath} ->
		    Reply = reset_file(Erfs, NewPath, Server),
		    Server ! {erfs, self(), Reply},
		    unlink(Server);
		{error, Error} ->
		    ?error(1, {"Path error", Error})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.


reset_file(Erfs, Path, Server) ->
    {ok, [Enode]} = erfs_db:lookup(Erfs, Path),
    erfs_db:insert(Erfs, Enode#enode{state=ok}),
    case Enode#enode.type of
	directory ->
	    reset_dir(Enode#enode.content, Erfs, Path, Server);
	Other ->
	    ok
    end.


reset_dir([File| Rest], Erfs, Path, Server) ->
    FilePath = erfs_file:join_paths(Path, File),
    reset_file(Erfs, FilePath, Server),
    reset_dir(Rest, Erfs, Path, Server);
reset_dir([], Erfs, Path, Server) ->
    ok.

