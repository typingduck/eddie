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
%%% File    : erfs_add_node.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Add a node to an Eddie Replicated File System
%%% Created :  9 Dec 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_add_node).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([add_node/3]).

-define(DEBUG, 9).

-include("erfs.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Func    : add_node/3
%% Args    : Node = Node()
%%           Opts = not_used()
%%           Server = The gen_server Pid()
%% Purpose : Add a node to Eddie Replicated File System
%% Returns : -
%%----------------------------------------------------------------------
add_node(Node, Opts, Server) ->
    %% Must add to add_node script to start node 
    %% -mnesia dir Dir -mnesia extra_db_nodes [node()]
    case net_adm:ping(Node) of
	pong ->
	    check_for_db(Node, Opts, Server);
	pang ->
	    ?debug(2, "Can't reach node ~p~n", [Node]),
	    erfs_utils:reply({"Can't reach node", Node}, Server)
    end.


check_for_db(Node, Opts, Server) ->
    case lists:member(Node, mnesia:system_info(running_db_nodes)) of
	true ->
	    stop_and_delete_db(Node, Opts, Server);
	false ->
	    delete_db(Node, Opts, Server)
    end.


stop_and_delete_db(Node, Opts, Server) ->
    rpc:call(Node, application, stop, [mnesia]),
    delete_db(Node, Opts, Server).

delete_db(Node, Opts, Server) ->
    Dir = rpc:call(Node, mnesia, system_info, [directory]),
    case erfs_file:del_dir({Node, Dir}, recurse) of
	ok ->
	    start_db(Node, Opts, Server);
	{error, Error} ->
	    start_db(Node, Opts, Server)
    end.


start_db(Node, Opts, Server) ->
    case catch rpc:call(Node, application, start, [mnesia]) of
	ok ->
	    copy_all_tables(Node, Opts, Server);
	{error, Reason} ->
	    stop_node(Node, Opts, Server)
    end.
    
    
copy_all_tables(Node, Opts, Server) ->
    case rpc:call(Node, mnesia, table_info, [schema, storage_type]) of
	disc_copies ->
	    add_table_copies(Node, Opts, Server);
	Other ->
%  	    case rpc:call(Node, mnesia, change_table_copy_type, 
%  			  [schema, Node, disc_copies]) of
	    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
		{atomic, ok} ->
		    add_table_copies(Node, Opts, Server);
		{aborted, Reason} ->
		    Error = mnesia:error_description(Reason),
		    ?debug(1, "Can't change schema on node ~p ~p~n", 
			   [Node, Error]),
		    stop_db(Node, Opts, Server)
	    end
    end.


add_table_copies(Node, Opts, Server) ->
%     [mnesia:add_table_copy(Table, Node, disc_copies) || 
% 	Table <- mnesia:system_info(tables) -- [schema]].
    add_table_copies(mnesia:system_info(tables) -- [schema], 
		     Node, Opts, Server).
	    
add_table_copies([Table | Rest], Node, Opts, Server) ->
    case mnesia:add_table_copy(Table, Node, disc_copies) of
 	{atomic, ok} ->
 	    add_table_copies(Rest, Node, Opts, Server);
 	{aborted, Reason} ->
	    Error = mnesia:error_description(Reason),
 	    ?debug(1, "Failed to copy table ~p due to ~p~n", 
		   [Table, Error]),
	    del_table_copies(Node, Opts, Server)
    end;
add_table_copies([], Node, Opts, Server) ->
    start_erfs(Node, Opts, Server).


start_erfs(Node, Opts, Server) ->
    case rpc:call(Node, application, start, [erfs]) of
	ok ->
	    erfs_utils:reply(ok, Server);
	{error, Error} ->
 	    ?debug(1, "Failed to start erfs at ~p due to ~p~n", 
		   [Node, Error]),
	    erfs_delete_node:delete_node(Node, Server)
    end.



%%%=====================================================================
%%% The other direction
%%%=====================================================================
stop_node(Node, Opts, Server) ->
    %% Must add to del_node script to stop node 
    case net_adm:ping(Node) of
	pong ->
	    rpc:call(Node, erlang, halt, []),
	    erfs_utils:reply("Node not added", Server);
	pang ->
	    erfs_utils:reply("Node not added", Server)
    end.

stop_db(Node, Opts, Server) ->
    case catch rpc:call(Node, application, stop, [mnesia]) of
	ok ->
	    erfs_utils:reply("Node not added", Server);
	{error, Reason} ->
	    erfs_utils:reply({"couldn't stop mnesia at", Node}, Server)
    end.


del_table_copies(Node, Opts, Server) ->
    [mnesia:delete_table_copy(Table, Node, disc_copies) || 
	Table <- mnesia:system_info(tables) -- [schema]],
    {atomic, ok} = mnesia:del_table_copy(schema, Node),
    stop_db(Node, Opts, Server).

