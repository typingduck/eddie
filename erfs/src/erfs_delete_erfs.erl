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
%%% File    : erfs_delete_erfs.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Add a node to an Eddie Replicated File System
%%% Created :  9 Dec 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_delete_erfs).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([delete_erfs/3]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func    : delete_erfs/3
%% Args    : Node = Node()
%%           Erfs = name of erfs, Atom()
%%           Server = The gen_server Pid()
%% Purpose : Delete a node from an erfs
%% Returns : #data
%%----------------------------------------------------------------------
delete_erfs(Erfs, Node, Server) ->
    case erfs_utils:valid_erfs(Erfs) of
	valid ->
	    case net_adm:ping(Node) of
		pong ->
		    {ok, [Entry]} = erfs_db:lookup(erfs, Erfs),
		    case lists:keymember(Node, 1, Entry#erfs.nodedefs) of
			true ->
			    ok_to_delete_erfs(Erfs, Node, Server);
			false ->
			    ?error(1, "Erfs replica doesn't exist")
		    end;
		pang ->
		    ?error(2, {"Can't reach node", Node})
	    end;
	{invalid, Reason} ->
	    ?error(1, {"Invalid erfs", Reason})
    end.


ok_to_delete_erfs(Erfs, Node, Server) ->
    case erfs_utils:check_and_set(Erfs, "/", ok, 
				  {{deleting_node, Node}, node()}) of
	ok ->
	    delete(Erfs, Node, Server);
	{Error, File} ->
	    ?error(1, {"Can't delete the node ", Error})
    end.


delete(Erfs, Node, Server) ->
    Root = erfs_utils:root_dir(Erfs, Node),
    erfs_file:del_dir({Node, Root}, recurse),
    {ok, [Entry]} = erfs_db:lookup(erfs, Erfs),
    NewEntry = Entry#erfs{nodedefs=Entry#erfs.nodedefs -- [{Node, Root}]},
    erfs_db:insert(erfs, NewEntry),
    erfs_utils:check_and_set(Erfs, "/", {{deleting_node, Node}, node()}, ok),
    erfs_utils:reply(ok, Server).

