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
%%% File    : erfs.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Eddie Replicated File System server
%%% Created : 14 Oct 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").

-behaviour(gen_server).


-export([start/0]).
-export([start_link/0, stop/0]).
-export([add_node/1, add_node/2, delete_node/1]).
-export([add_erfs/2, delete_erfs/2]).
-export([init_erfs/2, delete_erfs/1]).
-export([check_in/2, check_out/2, delete/2]).
-export([status/1, status/2, reset/1, reset/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-define(DEBUG, 9).

-include("erfs.hrl").

-define(NAME, ?MODULE).
-define(Timeout, infinity).

-record(data, {pending=[]}).


start() ->
    application:start(erfs).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).


stop() ->
    gen_server:call(?NAME, stop).


add_node(Node) ->
    add_node(Node, []).
add_node(Node, Opts) when atom(Node), list(Opts) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {add_node, Node, Opts}, Timeout).

add_erfs(Erfs, NodeDef) when atom(Erfs) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {add_erfs, Erfs, NodeDef}, Timeout).

delete_node(Node) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {delete_node, Node}, Timeout).

delete_erfs(Erfs, Node) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {delete_erfs, Erfs, Node}, Timeout).

init_erfs(Erfs, Nodes) ->
    init_erfs(Erfs, Nodes, []).

init_erfs(Erfs, Nodes, Opts) when atom(Erfs), list(Nodes) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {init_fs, Erfs, Nodes, Opts}, Timeout).

delete_erfs(Erfs) when atom(Erfs) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {delete_fs, Erfs}, Timeout).

check_in(Erfs, Path) when atom(Erfs), list(Path) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {check_in, Erfs, Path}, Timeout).

check_out(Erfs, Path) when atom(Erfs), list(Path) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {check_out, Erfs, Path}, Timeout).

delete(Erfs, Path) when atom(Erfs), list(Path) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {delete, Erfs, Path}, Timeout).

status(Erfs) ->
    status(Erfs, "/").

status(Erfs, Path) when atom(Erfs), list(Path) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {status, Erfs, Path}, Timeout).

reset(Erfs) ->
    reset(Erfs, "/").

reset(Erfs, Path) when atom(Erfs), list(Path) ->
    Timeout = ?Timeout,
    gen_server:call(?NAME, {reset, Erfs, Path}, Timeout).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(_Arg) ->
    process_flag(trap_exit, true),
    check_schema(node()).


check_schema(Node) ->
    case mnesia:table_info(schema, storage_type) of
	disc_copies ->
	    ensure_erfs_table(Node);
	Other ->
	    change_schema(Node)
    end.


change_schema(Node) ->
    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
	{atomic, ok} ->
	    ensure_erfs_table(Node);
	{aborted, Reason} ->
	    {stop, mnesia:error_description(Reason)}
    end.


ensure_erfs_table(Node) ->
    case lists:member(erfs, mnesia:system_info(tables)) of
	true ->
	    {ok, #data{}};
	false ->
	    create_erfs_table(Node)
    end.


create_erfs_table(Node) ->
    TabDef = [{disc_copies, [Node]}, 
	      {attributes, record_info(fields, erfs)}],
    case mnesia:create_table(erfs, TabDef) of
	{atomic, ok} ->
	    {ok, #data{}};
	{aborted, Reason} ->
	    {stop, mnesia:error_description(Reason)}
    end.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, Data}          |
%%          {reply, Reply, Data, Timeout} |
%%          {noreply, Data}               |
%%          {noreply, Data, Timeout}      |
%%          {stop, Reason, Reply, Data}   | (terminate/2 is called)
%%          {stop, Reason, Data}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, From, Data) ->
    {stop, normal, ok, Data};

handle_call({add_node, Node, Opts}, From, Data) ->
    Pid = spawn_link(erfs_add_node, add_node, [Node, Opts, self()]),
    NewData = add_call(Data, Pid, From, {add_node, Node, Opts}),
    {noreply, NewData};

handle_call({add_erfs, Erfs, NodeDef}, From, Data) ->
    Pid = spawn_link(erfs_add_erfs, add_erfs, [Erfs, NodeDef, self()]),
    NewData = add_call(Data, Pid, From, {add_erfs, Erfs, NodeDef}),
    {noreply, NewData};

handle_call({delete_node, Node}, From, Data) ->
    Pid = spawn_link(erfs_delete_node, delete_node, [Node, self()]),
    NewData = add_call(Data, Pid, From, {delete_node, Node}),
    {noreply, NewData};

handle_call({delete_erfs, Erfs, Node}, From, Data) ->
    Pid = spawn_link(erfs_delete_erfs, delete_erfs, [Erfs, Node, self()]),
    NewData = add_call(Data, Pid, From, {delete_erfs, Erfs, Node}),
    {noreply, NewData};

handle_call({init_fs, Erfs, Nodes, Opts}, From, Data) ->
    Pid = spawn_link(erfs_init_fs, init_fs, [Erfs, Nodes, Opts, self()]),
    NewData = add_call(Data, Pid, From, {init_fs, Erfs, Nodes, Opts}),
    {noreply, NewData};

handle_call({delete_fs, Erfs}, From, Data) ->
    Pid = spawn_link(erfs_delete_fs, delete_fs, [Erfs, self()]),
    NewData = add_call(Data, Pid, From, {delete_fs, Erfs}),
    {noreply, NewData};

handle_call({check_in, Erfs, Path}, From, Data) ->
    Pid = spawn_link(erfs_check_in, check_in, [Erfs, Path, self()]),
    NewData = add_call(Data, Pid, From, {check_in, Erfs, Path}),
    {noreply, NewData};

handle_call({check_out, Erfs, Path}, From, Data) ->
    Pid = spawn_link(erfs_check_out, check_out, [Erfs, Path, self()]),
    NewData = add_call(Data, Pid, From, {check_out, Erfs, Path}),
    {noreply, NewData};

handle_call({delete, Erfs, Path}, From, Data) ->
    Pid = spawn_link(erfs_delete, delete, [Erfs, Path, self()]),
    NewData = add_call(Data, Pid, From, {delete, Erfs, Path}),
    {noreply, NewData};

handle_call({status, Erfs, Path}, From, Data) ->
    Pid = spawn_link(erfs_status, status, [Erfs, Path, self()]),
    NewData = add_call(Data, Pid, From, {status, Erfs, Path}),
    {noreply, NewData};

handle_call({reset, Erfs, Path}, From, Data) ->
    Pid = spawn_link(erfs_reset, reset, [Erfs, Path, self()]),
    NewData = add_call(Data, Pid, From, {reset, Erfs, Path}),
    {noreply, NewData}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, Data}          |
%%          {noreply, Data, Timeout} |
%%          {stop, Reason, Data}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, Data) ->
    {noreply, Data}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, Data}          |
%%          {noreply, Data, Timeout} |
%%          {stop, Reason, Data}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, Data) ->
    io:format("Have you forgotten something?~n"),
    {noreply, Data};

handle_info({'EXIT', Pid, Reason}, Data) ->
    {{_, From, Action}, NewData} = delete_call(Pid, Data),
    gen_server:reply(From, {error, Reason}),
    {noreply, NewData};
    
handle_info({erfs, Pid, Reply}, Data) ->
    {{_, From, Action}, NewData} = delete_call(Pid, Data),
    gen_server:reply(From, Reply),
    {noreply, NewData};
    
handle_info(Info, Data) ->
    io:format("Got ~p~n", [Info]),
    {noreply, Data}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, Data) ->
    io:format("Got terminate reason ~p~n", [Reason]),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

add_call(Data, Pid, From, Action) ->
    Data#data{pending=Data#data.pending ++ [{Pid, From, Action}]}.

delete_call(Pid, Data) ->
    {Call, Pending} = del_call(Data#data.pending, Pid, []),
    {Call, Data#data{pending=Pending}}.

del_call([{Pid, From, Action} | Rest], Pid, Pending) ->
    {{Pid, From, Action}, Pending ++ Rest};
del_call([Call | Rest], Pid, Pending) ->
    del_call(Rest, Pid, Pending ++ [Call]);
del_call([], Pid, Pending) ->
    {no_call, Pending}.

	    
