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
%%% File    : erfs_check_out.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Check out a file or directory in an erfs
%%% Created : 14 Nov 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_check_out).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([check_out/3]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func    : check_out/3
%% Args    : Erfs = name of erfs, Atom()
%%           Path = Path to new directory, List()
%%           Server = The gen_server Pid()
%% Purpose : Check out a file or directory in an erfs
%% Returns : -
%%----------------------------------------------------------------------
check_out(Erfs, Path, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case erfs_utils:fix_path(Path) of
		{ok, NewPath} ->
		    Reply = ok_to_check_out(Erfs, NewPath, Server),
		    Server ! {erfs, self(), Reply},
		    unlink(Server);
		{error, Error} ->
		    ?error(1, {"Path error", Error})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.


ok_to_check_out(Erfs, Path, Server) ->
    case erfs_utils:check_and_set(Erfs, Path, ok, {checking_out, node()}) of
	ok ->
	    start(Erfs, Path, Server);
	{Error, File} ->
	    ?error(1, {"Can't check in file", File, Error})
    end.


start(Erfs, Path, Server) ->
    {ok, [Enode]} = erfs_db:lookup(Erfs, Path),
    case Enode#enode.type of
	directory ->
	    check_out_dir(Enode#enode.content, Erfs, Path, Server);
	Other ->
	    true
    end,
    NewEnode = Enode#enode{state={checked_out, node()}},
    erfs_db:insert(Erfs, NewEnode),
    ok.


check_out_dir([File | Rest], Erfs, Path, Server) ->
    FilePath = erfs_file:join_paths(Path, File),
    start(Erfs, FilePath, Server),
    check_out_dir(Rest, Erfs, Path, Server);
check_out_dir([], Erfs, Path, Server) ->
    true.
