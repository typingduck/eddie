%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.eddieware.org/EPL
%%%
%%% The contents of this file are subject to the Erlang Public License
%%% License, Version 1.0, (the "License"); you may not use this file
%%% except in compliance with the License. You may obtain a copy of the
%%% License at http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is Eddie-0.83b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : srv_table.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : Server managing server tables
%%% Created : 10 Aug 1998 by Tony Rogvall <tony@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(srv_table).
-author('tony@erix.ericsson.se').
-modified_by('patrik@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([load_table/1, unload_table/1, reload_table/1]).
-export([lookup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {}).

-define(SERVER_NAME,  srv_mgr).
-define(SERVER_TABLE, srv_table).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, ?MODULE, [], []).

load_table(Name) when atom(Name) ->
    gen_server:call(?SERVER_NAME, {load, Name}).

unload_table(Name) when atom(Name) ->
    gen_server:call(?SERVER_NAME, {unload, Name}).    

reload_table(Name) when atom(Name) ->
    gen_server:call(?SERVER_NAME, {reload, Name}).    

lookup(Key) ->
    case ets:lookup(?SERVER_TABLE, Key) of
	[] -> undefined;
	[Tuple] -> Tuple
    end.

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
init([]) ->
    ets:new(?SERVER_TABLE, [named_table]),  %% read access
    {ok, #state{}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call({load, T}, From, State) ->
    {reply, do_load(T, ?SERVER_TABLE), State};

handle_call({unload, T}, From, State) ->
    {reply, do_unload(T, ?SERVER_TABLE), State};
    
handle_call({reload, T}, From, State) ->
    {reply, do_reload(T, ?SERVER_TABLE), State};

handle_call(Request, From, State) ->
    {reply, {error, bad_request}, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_load(Name, Table) ->
    case ets:lookup(Table, Name) of
	[{_, true}] -> ok;
	[] ->
	    case catch apply(Name, load, [Table]) of
		{'EXIT', Reason} -> {error, Reason};
		Reply ->
		    ets:insert(Table, {Name, true}),
		    Reply
	    end;
	_ -> {error, bad_name}
    end.

do_unload(Name, Table) ->
    case ets:lookup(Table, Name) of
	[] -> ok;
	[{_, true}] ->
	    case catch apply(Name, unload, [Table]) of
		{'EXIT', Reason} -> {error, Reason};
		Reply ->
		    ets:delete(Table, Name),
		    Reply
	    end;
	_ -> {error, bad_name}
    end.
	    
do_reload(Name, Table) ->
    case do_unload(Name, Table) of
	{error, Reason} -> {error, Reason};
	Reply ->
	    do_load(Name, Table)
    end.

    



		    

	    
	    
