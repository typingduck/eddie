-module(sync_nodes_server).
-author('jocke@.ericsson.se').
%%%----------------------------------------------------------------------
%%% File    : sync_nodes_server.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created :  1 Oct 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%
%%% Modified: 14 Apr 1999 by tobbe@eddieware.org
%%%           Removed the opening of /etc/hosts and instead uses the 
%%%           info we have stored in the database.
%%%
%%% Modified  28/07/00 by clement.lyons@eddieware.org
%%%           Modified so that if the network connection to a node 
%%%           goes down and then comes back up the node will be detected 
%%%           by pinging it again.
%%% 
%%%----------------------------------------------------------------------
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
%%% The Original Code is Eddie-0.83b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%% Apr 99 - maurice@eddieware.org
%%%
%%%----------------------------------------------------------------------
-vc('$Id: sync_nodes_server.erl,v 1.1 2000/10/27 22:20:27 dredd Exp $ ').
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

-include("db.hrl").
-include("logger.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").

-define(PING_TIMEOUT, 5000).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?MODULE,[],[]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%% The original implementation did open /etc/hosts which was
%%% very undesireable. Some systems might not even have a /etc/hosts.
%%% Instead we makes use of the information we already have stored
%%% in the database.

init([]) ->
    case get_nodes() of
	{ok,Nodes} ->
	    F = fun(Node) ->
			Result = ping_result(net_adm:ping(Node))
		end,
	    lists:foreach(F,Nodes),
	    mnesia:subscribe({table,node}),
	    erlang:send_after(?PING_TIMEOUT, self(), ping_timeout),
	    {ok, Nodes};
	{error,Reason} ->
	    {stop,Reason}
    end.

ping_result(pong) -> "succeeded";
ping_result(pang) -> "failed".

%% Get the involved nodes from Mnesia.

get_nodes() ->
    case wait_for_node_table() of
	true -> get_the_nodes();
	Else -> Else
    end.

get_the_nodes() ->
%%    Get list of other nodes.    
    F = fun() -> mnemosyne:eval(query [N.name || N <- table(node), N.name /= node()] end) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    {error,?F("~w(~w): ~p",[?MODULE,?LINE,
				    mnesia:error_description(Reason)])};
	{atomic,Nodes} ->
	    {ok,Nodes}
    end.

%% Since we are doing this on every node at start up.
%% Mnesia might think that the node table isn't up to date.
%% However, we know that the stuff in the table is what
%% we want so we will in that case load it by force.

wait_for_node_table() ->
    case mnesia:wait_for_tables([node],5000) of
	ok -> true;
	_  -> do_force_load()
    end.

do_force_load() ->
    case mnesia:force_load_table(node) of
	yes -> true;
	_   -> {error,?F("~w(~w): table 'node' couldn't be loaded",
			 [?MODULE,?LINE])}
    end.

%% ------------
%% handle_call

handle_call(Request,From,State) ->
  {reply,ok,State}.

%% handle_cast

handle_cast(Msg,State) ->
  {noreply,State}.

%% handle_info
handle_info(ping_timeout, Nodes) ->
    F = fun(Node) ->
		Result = ping_result(net_adm:ping(Node))
	end,
    lists:foreach(F,Nodes),
    erlang:send_after(?PING_TIMEOUT, self(), ping_timeout),
    {noreply, Nodes};

handle_info({mnesia_table_event,{Operation,Record,ActivityID}},State) ->
    case get_the_nodes() of
	{ok, Nodes} -> {noreply, Nodes};
	{error, Reason} -> ?FATAL(Reason),
			   {noreply, State}
    end;

handle_info(Info,State) ->
  {noreply,State}.

%% terminate

terminate(Reason,State) ->
  ok.
