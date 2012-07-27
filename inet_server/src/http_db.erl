-module(http_db).
-author('jon@eddiware.com').
%%%----------------------------------------------------------------------
%%% File    : http_db.erl
%%% Author  : jon@eddiware.com
%%% Created : 01 Apr 1999
%%% Purpose : 
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
%%% AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%----------------------------------------------------------------------
-id('$Id: http_db.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/inet_server/src/http_db.erl,v $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:25 $ ').
-state('$State: Exp $ ').

-include("inet_server.hrl"). % The session time (the time to remember a client),
                             % is in the admit_ctrl record

%% External exports
-export([new/0,delete/1,
	 add_BE_node/4,remove_BE_node/4,get_BE_nodes/3,get_BE_nodes/4,
	 server/1,add_BE_node/5,remove_BE_node/5]).

-define(DB_PID_KEY,db_pid).
-define(TIMER_OUT,timer_out).

%% Put the db_server in the database, to be able to change the timer of the
%% added entries, the entries only live for the configured session time
new() ->
    Db = ets:new(session,[set,public]),
    Pid = spawn_link(?MODULE,server,[Db]),
    put_key(Db,?DB_PID_KEY,Pid),
    Db.
    
delete(Db) ->
    [{_,Db_server}] = get_key(Db,?DB_PID_KEY),
    Db_server ! quit,
    ets:delete(Db).


%% Get the BE nodes names of the BE's that the clients earlier requests has been
%% sent to within the session time
get_BE_nodes(Db,Frontend_name,{_,Client_socket}) ->
    case inet:peername(Client_socket) of
	{ok,{Ip,Port}} ->
	    case get_client(Db,Ip) of
		{Timer_ref,Node_list} ->
		    cancel_timer(Timer_ref), % cancel old timer
		    put_client(Db,Frontend_name,Ip,Node_list), % set a new one
		    Node_list;
		_ ->
		    []
	    end;
	_ ->
	    error
    end.

%% Same as above but with explicit Ip and Port instead of extracting
%% them from a Socket.
%%
get_BE_nodes(Db, Frontend_name, Ip, Port) ->
    case get_client(Db, Ip) of
        {Timer_ref, Node_list} ->
            cancel_timer(Timer_ref),
            put_client(Db, Frontend_name, Ip, Node_list),
            Node_list;
        _ ->
            []
    end.


%% Add a BE node to the clients BE-node list, and set the timer again
add_BE_node(Db,Frontend_name,{_,Client_socket},BE_node) ->
    case inet:peername(Client_socket) of
	{ok,{Ip,Port}} ->
	    case get_client(Db,Ip) of
		{Timer_ref,BE_node_list} ->
		    cancel_timer(Timer_ref), % cancel old timer
		    New_node_list = case lists:member(BE_node,BE_node_list) of
				      true ->
					    BE_node_list; % Already in BE_list
				      false ->
					  lists:append(BE_node_list,[BE_node])
				  end,
		    put_client(Db,Frontend_name,Ip,New_node_list), % add BEn, and set new timer
		    New_node_list;
		[] ->
		    put_client(Db,Frontend_name,Ip,[BE_node]),
		    [BE_node]
	    end;
	_ ->
	    error
    end.

%% Add a BE node to the clients BE-node list, and set the timer again
add_BE_node(Db,Frontend_name,Ip,Port,BE_node) ->
    case get_client(Db,Ip) of
	{Timer_ref,BE_node_list} ->
	    cancel_timer(Timer_ref), % cancel old timer
	    New_node_list = case lists:member(BE_node,BE_node_list) of
				true ->
				    BE_node_list; % Already in BE_list
				false ->
				    lists:append(BE_node_list,[BE_node])
			    end,
	    put_client(Db,Frontend_name,Ip,New_node_list), % add BEn, and set new timer
	    New_node_list;
	[] ->
	    put_client(Db,Frontend_name,Ip,[BE_node]),
	    [BE_node]
    end.


%% remove a BE node from a clients BE-node list,
remove_BE_node(Db,Frontend_name,{_,Client_socket},BE_node) ->
    case inet:peername(Client_socket) of
	{ok,{Ip,Port}} ->
	    case get_client(Db,Ip) of
		{Timer_ref,BE_node_list} ->
		    cancel_timer(Timer_ref),
		    New_node_list = case lists:member(BE_node,BE_node_list) of
					true ->
					    lists:delete(BE_node,BE_node_list);
					false ->
					    BE_node_list % Not in BE_list
				    end,
		    case New_node_list of
			[]    -> remove_client(Db,Ip); % No more BE's in clients list
			Other -> put_client(Db,Frontend_name,Ip,New_node_list)
		    end,
		    New_node_list;
		[] -> []
	    end;
	_ ->
	    error
    end.


%% remove a BE node from a clients BE-node list,
remove_BE_node(Db,Frontend_name,Ip,Port,BE_node) ->
    case get_client(Db,Ip) of
	{Timer_ref,BE_node_list} ->
	    cancel_timer(Timer_ref),
	    New_node_list = case lists:member(BE_node,BE_node_list) of
				true ->
				    lists:delete(BE_node,BE_node_list);
				false ->
				    BE_node_list % Not in BE_list
			    end,
	    case New_node_list of
		[]    -> remove_client(Db,Ip); % No more BE's in clients list
		Other -> put_client(Db,Frontend_name,Ip,New_node_list)
	    end,
	    New_node_list;
	[] -> []
    end.

%% Cancel a timer, remove timer event if the timer already has triggered
cancel_timer(Timer_ref) ->
    case erlang:cancel_timer(Timer_ref) of
	false -> %% timer already trigged
	    receive
		{timeout,Timer_ref,_} -> %% remove timer event
		    true
	    after 0 ->
		    true
	    end;
	TimeLeft ->
	    true
    end.

%%
%% Rember the backends that the clients old requests have been sent to within a
%% time period (specified in the config file). It is important to do that if a
%% state is kept in the backend.
%%
put_client(Db,Frontend_name,Ip,BE_list) ->
    [Admit_ctrl] = is_db:read({admit_ctrl, Frontend_name}),
    [{_,Db_server}] = get_key(Db,?DB_PID_KEY),
    put_key(Db,    %% set new timer (cancelled earlier)
	    Ip,
	    {erlang:send_after(Admit_ctrl#admit_ctrl.time,
			       Db_server,
			       {?TIMER_OUT,Ip}),
	     BE_list}).

get_client(Db,Ip) -> 
    case get_key(Db,Ip) of
       [{_,Value}] -> Value;
       [] -> []
    end.

remove_client(Db,Ip) ->
    remove_key(Db,Ip).

%%
%% Internal
%%
put_key(Db,Key,Value) -> ets:insert(Db,{Key,Value}).

get_key(Db,Key) -> ets:lookup(Db,Key).

remove_key(Db,Key)  -> ets:delete(Db,Key).

%%
%% Server used to remove entries after the time period is ended.
%%
server(Db) ->
    receive
	{?TIMER_OUT,Ip} ->
	    remove_client(Db,Ip),
	    server(Db);
	quit ->
	    ok;
	_ ->
	    server(Db)
    end.

