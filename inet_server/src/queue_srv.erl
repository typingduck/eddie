-module(queue_srv).
%%%----------------------------------------------------------------------
%%% File    : queue_srv.erl
%%% Author  : Patrik Winroth <patrik@erix.ericsson.se>
%%% Created : 22 Apr 1999 by tobbe@eddieware.org
%%% Purpose : Ripped this out of: inet_server.erl
%%%----------------------------------------------------------------------
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
%%% Mar, Apr 99 - jon@eddieware.org
%%%
%%%----------------------------------------------------------------------

-vc('$Id: queue_srv.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-export([start/2,in/3,out/1,get_client_queue_name/2,
	 get_queue_sizes/1]).
%% Internal
-export([init/2]).
-modified_by('eric.yeo@ericsson.com.au').
-include("inet_server.hrl"). %% Could be removed later

-record(queue,
	{db,
	 queues,
	 size,
	 cookie_name,
	 min_queue}).

%%
%% Exported interface
%% 

%%
%% Name: start/1 
%% Purpose: starts the queue server process and returns its Pid
%%

start(Admit_db, Qos_config) ->
    spawn_link(?MODULE,init,[Admit_db, Qos_config]).

%%
%% Name: in/3 
%% Purpose: inserts a session into the queue
%% Returns:
%%      ok - successful insertion
%%      full - queue is full
%%      timeout - failed to contact the queue server
%%

in(Pid, N, Max_queue) when pid(Pid) ->
    call(Pid,{insert, N, Max_queue}).

%%
%% Name: out/1 
%% Purpose: remove a session from one of the queues (best choice).
%% Returns: 
%%      Element 
%%      empty - queues are all empty
%%      timeout - failed to contact the queue server
%%

out(Pid) ->
    call(Pid,out).

%%
%% Given the cookie value, determine the queue associated with that
%% cookie value. If not found, return not_found.
%%
get_client_queue_name(Pid, Cookie_value) ->
    call(Pid, {get_client_queue_name, Cookie_value}).

%% Return a list of queue names and their sizes.
get_queue_sizes(Pid) ->
    call(Pid, get_queue_sizes).

%%
%% Queue server for Admission control.
%%

init(Admit_db, {Cookie_name, [ H | T ]}) ->
    { S1, S2, S3 } = erlang:now(),
    random:seed(S3,S2,S1),
    %random:seed(),
    { Queue_name, Weight, Cookie_value } = H,
    Admit_queue = [ {Queue_name, Weight, Cookie_value, queue:new(), 0} ],
    State = #queue{db = Admit_db, queues = Admit_queue,
           size = 0, cookie_name = Cookie_name,
           min_queue = Queue_name },
    loop(State).

loop(State) ->
    receive
	{Pid, out}  ->
	    NewState = do_out(State,Pid),
	    loop(NewState);

	{Pid, {insert, Client_id, Max_queue}} ->
	    NewState = do_in(State,Pid,Client_id,Max_queue),
	    loop(NewState);

    {Pid, {get_client_queue_name, Cookie_value}} ->
        send(Pid, State#queue.min_queue),
        loop(State);

	{Pid, get_queue_sizes} ->
	    send(Pid, lists:map(fun ({Queue_name, _, _, _, Size}) ->
					{Queue_name, Size}
				end, State#queue.queues)),
	    loop(State)

    end.

do_out(State,Pid) -> 
    case find_best_queue(State#queue.queues) of
	{ok, Queue_name} ->
	    case do_dequeue(Queue_name, State#queue.queues) of
		{{value, Element}, New_queues} ->
		    is_db:update_queue_size(State#queue.db, Queue_name, -1),
		    send(Pid,Element),
		    New_queue_size = State#queue.size - 1,
		    Elements_in_queue = is_db:get_queue_size(State#queue.db,
							     Queue_name),
		    if Elements_in_queue == 0 -> 
			    is_db:reset_dequeued_size(State#queue.db,
						      Queue_name);
		       true ->
			    is_db:update_dequeued_size(State#queue.db,
						       Queue_name, 1)
		    end,
		    State#queue{queues = New_queues,
				size = New_queue_size};
		{_, New_queues} ->
		    send(Pid,empty),
		    State#queue{queues = New_queues,
				size = 0}
	    end;
	_ ->
	    send(Pid, empty),
	    State
    end.

do_in(State,Pid,Client_id,Max_queue)
  when State#queue.size < Max_queue ->
    {_, Class} = Client_id,
    is_db:update_queue_size(State#queue.db,Class,1),
    N = State#queue.size + 1,
    case do_enqueue(State#queue.min_queue, State#queue.queues, Client_id) of
	{New_queues, Size} ->
	    send(Pid,Size),
	    State#queue{queues = New_queues, size = N};
	[] ->
	    send(Pid, error),
	    State
    end;
do_in(State,Pid,Client_id,Max_queue) ->
    send(Pid,full),
    State.

%%
%% Given the cookie specification, find an appropriate queue to
%% put the Ip address in.
%%

do_enqueue(Min_queue, Queues, Client_id) ->
    {_, Cookie_value} = Client_id,
    case Cookie_value of
	not_found ->    %% No cookies, put in the minimum weighted queue
	    do_enqueue2(Min_queue, Queues, Client_id);
	_ -> %% Try to put into the right queue
	    case do_enqueue3(Queues, Client_id) of
		[] ->   %% No such queue found, put in the minimum weight one.
		    do_enqueue2(Min_queue, Queues, Client_id);
		Return_value ->  %% Item inserted
		    Return_value
	    end
    end.

%%
%% Given the queue name, insert the Ip address into the
%% queue specified.
%%

do_enqueue2(Queue_name,
	    [{Queue_name, Weight, Cookie_value, Queue, Size} | Rest],
	    Client_id) ->
    {[{Queue_name, Weight, Cookie_value, queue:in(Client_id, Queue), Size+1}
     | Rest], Size+1};
do_enqueue2(Queue_name, [Queue_item | Rest], Client_id) ->
    case do_enqueue2(Queue_name, Rest, Client_id) of
	{New_queues, Size} ->
	    {[Queue_item | New_queues], Size};
	[] ->
	    []
    end;
do_enqueue2(Queue_name, [], _) ->  %% This case should not happen
    [].

%%
%% Given the cookie value, find the right queue to put the Ip in.
%% If not found, return an empty list.
%%

do_enqueue3([{Queue_name, Weight, Cookie_value, Queue, Size}|Rest],
	    {Ip, Queue_name}) ->
    {[{Queue_name, Weight, Cookie_value, queue:in({Ip, Queue_name}, Queue), Size+1} | Rest], Size+1};
do_enqueue3([Queue_item | Rest], Client_id) ->
    case do_enqueue3(Rest, Client_id) of
	{New_queues, Size} ->
	    {[Queue_item|New_queues], Size};
	[] ->
	    []
    end;
do_enqueue3([], _) ->
    [].

%%
%% Given the queue name, dequeue an item from the specified queue.
%%

do_dequeue(Queue_name, [{Queue_name, Weight, Cookie_value, Queue, Size} | Rest]) ->
    {Result, New_queue} = queue:out(Queue),
    {Result, [{Queue_name, Weight, Cookie_value, New_queue, Size-1}|Rest]};
do_dequeue(Queue_name, [Queue_item | Rest]) ->
    {Result, New_queues} = do_dequeue(Queue_name, Rest),
    {Result, [Queue_item|New_queues]};
do_dequeue(_, []) ->  %% This case should not occur.
    {error, []}.


%%
%% Misc routines
%%

call(Pid,Msg) ->
    call(Pid,Msg,2500).

call(Pid,Msg,Timeout) ->
    send(Pid,Msg),
    recv(Pid,Timeout).

send(Pid,Msg) -> Pid ! {self(),Msg}.

recv(Pid,Timeout) ->
    receive
	{Pid,Data} -> Data
    after Timeout  -> timeout
    end.

%%
%% Return the name of the best queue to dequeue.
%%

find_best_queue(Queues) ->
    case get_nonempty_queues(Queues, [], 0) of
	{0, []} ->
	    not_found;
	{Total_weight, Queue_list} ->
	    get_random_queue(random:uniform(Total_weight), Queue_list, 0)
    end.

%%
%% Construct a list of {Queue_name, Weight} pairs that represents
%% the queues that are non-empty. Return it together with the sum of
%% the weights of these queues.
%%

get_nonempty_queues([{Queue_name, Weight, _, Queue, _} | Rest],
		   List_so_far,
		   Total_weight) ->
    if
	(Queue == {[], []}) ->
	    get_nonempty_queues(Rest, List_so_far, Total_weight);
	true ->
	    New_total = Total_weight + Weight,
	    get_nonempty_queues(Rest,
			       [{Queue_name, Weight} | List_so_far],
			       New_total)
    end;
get_nonempty_queues([], Queue_list, Total_weight) ->
    {Total_weight, Queue_list}.

%%
%% Base on the weights, randomly choose a queue to return.
%%

get_random_queue(Random, [{Queue_name, Weight} | Rest], Sum) ->
    New_sum = Sum + Weight,
    if
	(New_sum < Random) ->
	    get_random_queue(Random, Rest, New_sum);
	true ->
	    {ok, Queue_name}
    end;
get_random_queue(_, [], _) ->  %% this case should not happen
    error.
