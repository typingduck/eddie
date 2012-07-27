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
%%% File    : erfs_db.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Eddie Replicated File Systems database primitives
%%% Created : 27 Oct 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_db).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-export([insert/2, lookup/2, delete/2]).
-export([copy_all_tables/1, delete_all_tables/1]).
-export([start_db/1, stop_db/1, delete_db/1]).
-export([delete_table/2]).
-export([create_table/3, create_table/4]).

-define(DEBUG, 1).

-include("erfs.hrl").


%%----------------------------------------------------------------------
%% Func:    create_table/3
%% Args:    Name     Table name
%%          Nodes    List of nodes where this table should be replicated
%%          RecordType Name of record type used in this table
%% Purpose: Create a new Table
%% Returns: ok          |
%%          {error, Reason}
%%----------------------------------------------------------------------
create_table(Name, Nodes, Attributes) ->
    TabDef = [{disc_copies, Nodes},
	      {attributes, Attributes}],
    case mnesia:create_table(Name, TabDef) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, mnesia:error_description(Reason)}
    end.

create_table(Name, Nodes, Record, Attributes) ->
    TabDef = [{disc_copies, Nodes},
	      {record_name, Record},
	      {attributes, Attributes}],
    case mnesia:create_table(Name, TabDef) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, mnesia:error_description(Reason)}
    end.


%%----------------------------------------------------------------------
%% Func:    insert/2
%% Args:    Table    Mnesia table name
%%          Instance Instance to insert in the table
%% Purpose: Insert Instance into the Table 
%% Returns: ok          |
%%          {error, Reason}
%%----------------------------------------------------------------------
insert(Table, Instance) ->
    ?debug(9, "Inserting ~p in ~p~n", [Instance, Table]),
    Fun = fun() -> mnesia:write(Table, Instance, write) end,
    case mnesia:transaction(Fun) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, mnesia:error_description(Reason)}
    end.


%%----------------------------------------------------------------------
%% Func:    lookup/2
%% Args:    Table    Mnesia table name
%%          Key      
%% Purpose: Lookup Key in Table
%% Returns: {ok, Value}      |
%%          {error, Reason}
%%----------------------------------------------------------------------
lookup(Table, Key) ->
    Fun = fun() -> mnesia:read(Table, Key, read) end,
    case mnesia:transaction(Fun) of
	{atomic, Value} ->
	    {ok, Value};
	{aborted, Reason} ->
	    {error, mnesia:error_description(Reason)}
    end.


%%----------------------------------------------------------------------
%% Func:    delete/2
%% Args:    Table    Mnesia table name
%%          Key      
%% Purpose: Lookup Key in Table
%% Returns: {ok, Value}      |
%%          {error, Reason}
%%----------------------------------------------------------------------
delete(Table, Key) ->
    Fun = fun() -> mnesia:delete(Table, Key, write) end,
    case mnesia:transaction(Fun) of
	{atomic, Value} ->
	    {ok, Value};
	{aborted, Reason} ->
	    {error, mnesia:error_description(Reason)}
    end.


%%----------------------------------------------------------------------
%% Func:    delete_table/2
%% Args:    Table
%%          Node() | [Node()]
%% Purpose: Delete a table
%% Returns: ok | {error, Error}
%%----------------------------------------------------------------------
delete_table(Table, [Node| Nodes]) ->
    case delete_table(Table, Node) of
 	ok ->
 	    delete_table(Table, Nodes);
 	{error, Error} ->
	    {error, Error}
    end;
delete_table(Table, Node) ->
    case mnesia:del_table_copy(Table, Node) of
 	{atomic, ok} ->
	    ok;
 	{aborted, Reason} ->
	    Error = mnesia:error_description(Reason),
 	    ?debug(2, "Failed to delete table ~p due to ~p~n", 
		   [Table, Error]),
	    {error, {"Failed to delete table", Table, Error}}
    end.


%%----------------------------------------------------------------------
%% Func:    delete_all_tables/1
%% Args:    Node()
%% Purpose: Delete all local tables on a node
%% Returns: ok | {error, Error}
%%----------------------------------------------------------------------
delete_all_tables(Node) ->
%     [mnesia:delete_table_copy(Table, Node, disc_copies) || 
% 	Table <- mnesia:system_info(tables) -- [schema]].
    Tables = rpc:call(Node, mnesia, system_info, [local_tables]),
    delete_all_tables(Tables -- [schema], Node).
	    
delete_all_tables([Table | Rest], Node) ->
    case mnesia:del_table_copy(Table, Node) of
 	{atomic, ok} ->
 	    delete_all_tables(Rest, Node);
 	{aborted, Reason} ->
	    Error = mnesia:error_description(Reason),
 	    ?debug(1, "Failed to delete table ~p due to ~p~n", 
		   [Table, Error]),
 	    {error, {"Failed to delete table", Table, Error}}
    end;
delete_all_tables([], Node) ->
    {atomic, ok} = mnesia:del_table_copy(schema, Node),
    ok.


%%----------------------------------------------------------------------
%% Func:    delete_db/1
%% Args:    Node()
%% Purpose: Delete schema on a node
%% Returns: ok          |
%%          exit(Reason)
%%----------------------------------------------------------------------
delete_db(Node) ->
    case mnesia:delete_schema([Node]) of
 	{atomic, ok} ->
	    delete_mnesia_dir(Node),
 	    ok;
 	{aborted, Reason} ->
	    Error = mnesia:error_description(Reason),
 	    ?debug(1, "Failed to delete db due to ~p~n", 
		   [Error]),
 	    exit({"Failed to delete db", Error})
    end.

delete_mnesia_dir(Node) ->
    MnesiaDir = rpc:call(Node, mnesia, system_info, [directory]),
    rpc:call(Node, erfs_utils, delete_dir, [MnesiaDir]).


%%----------------------------------------------------------------------
%% Func:    start_sb/1
%% Args:    Node()
%% Purpose: Start mnesia on a node
%% Returns: ok          |
%%          exit(Reason)
%%----------------------------------------------------------------------
start_db(Node) ->
    case lists:member(Node, mnesia:system_info(running_db_nodes)) of
	true ->
	    ok;
	false ->
	    erfs_utils:start_app(mnesia, Node)
    end.


%%----------------------------------------------------------------------
%% Func:    stop_db/1
%% Args:    Node()
%% Purpose: Stop mnesia on a node
%% Returns: ok          |
%%          exit(Reason)
%%----------------------------------------------------------------------
stop_db(Node) ->
    erfs_utils:stop_app(mnesia, Node).


%%----------------------------------------------------------------------
%% Func:    copy_all_tables/1
%% Args:    Node()
%% Purpose: Copy all tables to a node
%% Returns: ok          |
%%          exit(Reason)
%%----------------------------------------------------------------------
copy_all_tables(Node) ->
    case rpc:call(Node, mnesia, table_info, [schema, storage_type]) of
	disc_copies ->
	    add_table_copies(Node);
	Other ->
 	    case rpc:call(Node, mnesia, change_table_copy_type, 
 			  [schema, Node, disc_copies]) of
%	    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
		{atomic, ok} ->
		    add_table_copies(Node);
		{aborted, Reason} ->
		    Error = mnesia:error_description(Reason),
		    ?debug(1, "Can't change schema on node ~p ~p~n", 
			   [Node, Error]),
		    exit({"Can't change schema", Node, Error})
	    end
    end.
   
	    
add_table_copies(Node) ->
%     [mnesia:add_table_copy(Table, Node, disc_copies) || 
% 	Table <- mnesia:system_info(tables) -- [schema]].
    add_table_copies(mnesia:system_info(tables) -- [schema], Node).
	    
add_table_copies([Table | Rest], Node) ->
    case mnesia:add_table_copy(Table, Node, disc_copies) of
 	{atomic, ok} ->
 	    add_table_copies(Rest, Node);
 	{aborted, Reason} ->
	    Error = mnesia:error_description(Reason),
 	    ?debug(1, "Failed to copy table ~p due to ~p~n", 
		   [Table, Error]),
 	    exit({"Failed to copy table", Table, Error})
    end;
add_table_copies([], Node) ->
    ok.
