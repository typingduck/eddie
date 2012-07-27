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
%%% File    : erfs_delete_node.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Delete a node from an Eddie Replicated File System
%%% Created :  9 Dec 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_delete_node).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([delete_node/2]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func    : delete_node/3
%% Args    : Node = Node()
%%           Server = The gen_server Pid()
%% Purpose : Delete a node from an Eddie Replicated File System
%% Returns : #data
%%----------------------------------------------------------------------
delete_node(Node, Server) ->
    case rpc:call(Node, application, stop, [erfs]) of
	ok ->
	    del_table_copies(Node, Server);
	{error, Reason} ->
	    ?debug(1, "Couldn't stop erfs at ~p due to ~p~n", 
		   [Node, Reason]),
	    del_table_copies(Node, Server)
    end.

del_table_copies(Node, Server) ->
    %% Maybe we should use mnesia:delete_schema([Node])
    Tables = (mnesia:system_info(tables) -- [schema]) ++ [schema],
    del_table_copies(Tables, Node, Server).

del_table_copies([Table | Rest], Node, Server) ->
    case mnesia:del_table_copy(Table, Node) of
  	{atomic, ok} ->
  	    del_table_copies(Rest, Node, Server);
  	{aborted, Reason} ->
 	    Error = mnesia:error_description(Reason),
  	    ?debug(1, "Failed to delete table ~p due to ~p~n", 
 		   [Table, Error]),
 	    del_table_copies(Rest, Node, Server)
    end;
del_table_copies([], Node, Server) ->
    stop_mnesia(Node, Server).


stop_mnesia(Node, Server) ->
    case catch rpc:call(Node, application, stop, [mnesia]) of
	ok ->
	    delete_mnesia_dir(Node, Server);
	{error, Reason} ->
	    ?debug(1, "Couldn't stop mnesia at ~p due to ~p~n", 
		   [Node, Reason]),
	    delete_mnesia_dir(Node, Server)
    end.


delete_mnesia_dir(Node, Server) ->
    Dir = rpc:call(Node, mnesia, system_info, [directory]),
    case erfs_file:del_dir({Node, Dir}, recurse) of
	ok ->
	    stop_node(Node, Server);
	{error, Error} ->
  	    ?debug(1, "Failed to delete mnesia dir ~p at ~p due to ~p~n", 
 		   [Dir, Node, Error]),
	    stop_node(Node, Server)
    end.


stop_node(Node, Server) ->
    %% Must add to del_node script to stop node
    %% and delete the database directory
    case net_adm:ping(Node) of
	pong ->
	    rpc:call(Node, erlang, halt, []),
	    erfs_utils:reply(ok, Server);
	pang ->
	    erfs_utils:reply(ok, Server)
    end.




