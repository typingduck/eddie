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
%%% The Original Code is Eddie 1.3.1
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%
%%%----------------------------------------------------------------------
%%% File    : dns_recurse_udp_tracker.erl
%%% Author  : Pekka Hedqvist <pekka@eddieware.org>
%%% Purpose : Keep track and limit the no of udp ports used  in recursive 
%%%           queries from dns_recurse.erl. De allocs udp when caller exits.
%%% Created : 11 Nov 1999 by Pekka Hedqvist <Pekka@eddieware.org>
%%%----------------------------------------------------------------------

-module(dns_recurse_udp_tracker).
-author('pekka@eddieware.org').
-behaviour(gen_server).

%% External exports
%-compile(export_all).
-export([start_link/0, open_udp/0, close_udp/1]).

%% gen_server callbacks (internal exports)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include_lib("../../misc/include/logger.hrl").

%-define(ERROR(X), X).
%-define(F(Z,Y), io:format(Z,Y)).
%-define(P(X), io:format("~p state: ~p~n", [self(), X])).

-define(N_UDPS, 128).  %% number of concurrent UDP:s to dns_recurse
-define(TRIES, 1).    %% how many retries when limit udp is reached 
                      %% before failing.

-record(state, {allocs, %% holder of allocs, table for now
		parent, %% parent PID
		n_udps = ?N_UDPS %% Number of udp:s used.
	       }).

-record(res, {udp, pid}).

-define(RES_TIMEOUT, 250).
-define(PAUSE(), receive after ?RES_TIMEOUT -> ok end).
-define(MATCH_PID(UDP),  #res{pid = '_', udp = UDP}).
-define(MATCH_UDP(PID),  #res{pid = PID, udp = '_'}).
-define(MATCHALL(),      #res{udp = '_', pid = '_'}).
-define(MATCH(PID, UDP), #res{udp = UDP, pid = PID}).

-define(ASSERT_NUDPS(NUDPS),
	if NUDPS >= 0 -> true;
	   true       -> exit({invalid_n_udps, NUDPS})
	end
	).

% test() ->   gen_server:start({local, ?MODULE}, ?MODULE, [self()], []).
	    
% test_s(A) -> 
%     receive 
% 	alloc ->
% 	    X = open_udp(),
% 	    io:format("~p alloc ~p~n", [self(), [X]++A]),
% 	    test_s([X] ++ A);
% 	de_alloc ->
% 	    io:format("~p de_alloc ~p~n", [self(), A]),
% 	    close_udp(hd(A)),
% 	    test_s(tl(A));
% 	die ->
% 	    exit(aargh)
%     end.
	    
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [self()], []).

open_udp() -> open_udp(?TRIES). %% 

close_udp(UDP) ->
    gen_server:call(?MODULE, {close_udp, UDP}).

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
init([PID]) ->
    process_flag(trap_exit, true),
    Tab = ets:new(?MODULE, [named_table, {keypos, #res.udp}]),
    State = #state{parent = PID, allocs = Tab},
    {ok, #state{parent = PID, allocs = Tab}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(open_udp, {PID,_TAG}, State) when State#state.n_udps == 0 ->
    case member_pid(PID, State#state.allocs) of
	true  -> ok; 
	false -> unlink(PID) 
    end,
    {reply, limit, State};

handle_call(open_udp, From, State) ->
    {PID, _Tag} = From,
    #state{n_udps = NUDPS, allocs = Allocs0} = State,
    case gen_udp:open(0, [binary]) of
	{ok, Udp} ->
	    gen_udp:controlling_process(Udp, PID),
	    Allocs = add_alloc(Udp, PID, Allocs0),
	    {reply, {ok, Udp}, State#state{n_udps = NUDPS - 1,
					   allocs = Allocs}};
	Error ->
	    case member_pid(PID, Allocs0) of
		true  -> ok; 
		false -> unlink(PID) 
	    end,
	    {reply, {error, Error}, State}
    end;

handle_call({close_udp, UDP}, {PID, _Tag}, State) ->
    #state{n_udps = NUDPS0, allocs = Allocs0} = State,
    {RemovedBool, Allocs} = remove_alloc(UDP, Allocs0),
    if RemovedBool == true ->
	    gen_udp:close(UDP),
	    case member_pid(PID, Allocs) of
		true  -> ok; 
		false -> unlink(PID) %% only unlink if last PID resource
	    end,
	    {reply, ok, State#state{n_udps = NUDPS0 + 1,
				    allocs = Allocs}};
       true ->
	    %% someone tried to close a nonexistent socket
	    %% ignore this
	    {reply, ok, State}
    end.
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
handle_info({'EXIT', Pid, _}, State) when State#state.parent == Pid ->
    {stop, parent_exit, State};

handle_info({'EXIT', Pid, _}, State) ->
    #state{allocs = Allocs0, n_udps = NUDPS0} = State,
    {Socks, NAllocs} = remove_all_sockets(Pid, Allocs0),
    lists:foreach(fun(UDP) -> gen_udp:close(UDP) end,  Socks),
    NUDPS = NUDPS0 + length(Socks),
    ?ASSERT_NUDPS(NUDPS),
    {noreply, State#state{allocs = NAllocs,
			  n_udps = NUDPS}};

handle_info({udp, UDP, IP, InPortNo, Data}, State) ->
    #state{allocs = Allocs0} = State,
    %% should not happen but handle it anyway
    case get_pid(UDP, Allocs0) of 
	[] -> % throw away UDP msg, should not happen
	    {noreply, State};
	PID ->
	    PID ! {udp, UDP, IP, InPortNo, Data},
	    {noreply, State}
    end;

handle_info({udp_closed, UDP}, State) ->
    %% should not happen, but..
    #state{allocs = Allocs0, n_udps = NUDPS0} = State,
    case get_pid(UDP, State#state.allocs) of 
	[] -> % throw away UDP msg, probably old junk
	    {noreply, State#state{n_udps = NUDPS0 + 1}};
	PID ->
	    PID ! {udp_closed, UDP},
	    {noreply, State#state{n_udps = NUDPS0 + 1}}
    end;

handle_info(info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    #state{allocs = Allocs} = State,
    All = all_alloc(Allocs),
    lists:foreach(fun(RES) -> gen_udp:close(RES#res.udp) end, All),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
member_pid(PID, Allocs) ->
    case ets:match_object(Allocs, ?MATCH_UDP(PID)) of
	[] -> false;
	_  -> true
    end.
    
member_udp(UDP, Allocs) ->
    case ets:lookup(Allocs, UDP) of
	[] -> false;
	_  -> true
    end.

get_udps(PID, Allocs) -> ets:match_object(Allocs, ?MATCH_UDP(PID)).
get_pid(UDP, Allocs)  ->
    case ets:lookup(Allocs, UDP, #res.pid) of
	[PID] -> PID;
	_ -> []
    end.

all_alloc(Allocs)     -> ets:match_object(Allocs, ?MATCHALL()).
add_alloc(UDP, PID, Allocs) ->
    ets:insert(Allocs, #res{pid = PID, udp = UDP}),
    Allocs.

remove_alloc(UDP, Allocs) ->
    R = member_udp(UDP, Allocs),
    if R == false -> {false, Allocs};
       true       -> ets:match_delete(Allocs, ?MATCH_PID(UDP)),
		     {true, Allocs}
    end.

remove_all_sockets(PID, Allocs) ->
    case ets:match_object(Allocs, ?MATCH_UDP(PID)) of
	[] ->   {[], Allocs};
	ResL -> ets:match_delete(Allocs, ?MATCH_UDP(PID)),
		{[element(#res.udp, X) || X <- ResL], Allocs}
    end.
%%%--------------------------------------------------------------------
%%% Executed by the callet of open_udp(). Tries ?TRIES times to get a 
%%% socket. Between each try it waits a little while. When ReTried=0 and 
%%% and it fails to open udp if fails, it also fails if the the socket 
%%% cannot be opened by the tracker process.
%%%--------------------------------------------------------------------
open_udp(ReTried) -> %% nonexported
    link(whereis(?MODULE)),
    case gen_server:call(?MODULE, open_udp) of
	{ok, UDP} -> 
	    UDP;
	limit when ReTried >= 0 ->
	    ?PAUSE(), %% pause for a short while
	    open_udp(ReTried - 1);
	limit -> %% when ReTried <= 0
	    ?ERROR(?F("Limit reached. "
		      "Can't open udp for recursive question",
		      [])),
	    exit(udp_limit_reached);
	{error, Error} ->
	    ?ERROR(?F("Can't open udp for recursive question - ~p",
		      [Error])),
	    exit(Error)
    end.


%%%----------------------------------------------------------------------
%%% EOF
%%%----------------------------------------------------------------------













