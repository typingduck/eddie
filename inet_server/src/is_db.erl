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

-module(is_db).
-author('patrik@elrond').
-modified_by('jon@eddieware.org').
-modified_by('geoff@eddieware.org').

-export([create_table/2,read/1,write/1,
	 new_adb/1,delete_adb/1,
	 put_session/2,get_session/2,remove_session/2,
	 put_queued_session/5,
	 reset_sessions_size/2,update_sessions_size/2,get_sessions_size/1,
	 reset_dequeued_size/2,get_dequeued_size/2,update_dequeued_size/3,
	 get_queue_size/2,update_queue_size/3,
	 reset_queue_counter/1,get_queue_counter/1,update_queue_counter/2,
	 reset_reject_counter/1,get_reject_counter/1,update_reject_counter/2,
	 increment_total_sessions/1, get_total_sessions/1,
	 get_total_queue/1, get_total_reject/1]).

create_table(Table, Args) ->
    mnesia:create_table(Table, Args).

read(Oid) ->
    mnesia:dirty_read(Oid).

write(Object) -> 
    {atomic,ok} = 
	mnesia:transaction(fun() -> 
				   mnesia:write(Object) 
			   end).

%% mnesia:dirty_write(Object).

%% We don't care about the aborted transaction. If the object
%% does not exist, we are happy too.
%%
delete(Object) ->
    mnesia:transaction(fun() ->
			       mnesia:delete(Object)
		       end).


%%
%% Admit database, used for admission control

new_adb(Queue_names) -> 
    Db = ets:new(admit, [set, public]),
    reset_sessions_size(Db,0),
    reset_queue_counter(Db),
    reset_reject_counter(Db),
    put_counter(Db, total_reject_counter, 0),
    put_counter(Db, total_sessions_counter, 0),
    put_counter(Db, total_queue_counter, 0),
    lists:map(fun(Queue_name) ->
		      reset_dequeued_size(Db, Queue_name),
		      reset_queue_size(Db, Queue_name)
	      end, Queue_names),
    Db.

delete_adb(Db) ->
    ets:delete(Db).

%%
%% Active entries
%%
put_session(Db,Ip) ->
    put_key(Db,Ip,{element(1,statistics(wall_clock))}).

get_session(Db,Ip) ->
    get_value(Db,Ip).

remove_session(Db,Ip) ->
    remove_key(Db,Ip).

%%
%% Total sessions admitted

increment_total_sessions(Db) ->
    update_counter(Db,total_sessions_counter,1).

get_total_sessions(Db) ->
    get_counter(Db, total_sessions_counter).

%%
%% Queued entries, removed by moving queued session to an active session
%% (Db is [set,public])
%%

put_queued_session(Db,Ip,Place,Queue_relative,Sum_reload_time) ->
    put_key(Db,Ip,{Place,Queue_relative,Sum_reload_time}).

%% Number of active sessions key
reset_sessions_size(Db,Size) ->
    put_counter(Db,admit_sessions,Size).
update_sessions_size(Db,Change) ->
    update_counter(Db,admit_sessions,Change).
get_sessions_size(Db) ->
    get_counter(Db,admit_sessions).

%% Number of dequeued sessions key
reset_dequeued_size(Db, Queue_name) ->
    put_counter(Db,{admit_dequeued, Queue_name},0).  
get_dequeued_size(Db, Queue_name) ->
    get_counter(Db,{admit_dequeued, Queue_name}).

update_dequeued_size(Db,Queue_name, Change) ->
    update_counter(Db,{admit_dequeued, Queue_name},Change).

%% Queue key
reset_queue_size(Db, Queue_name) ->
    put_counter(Db,{admit_queue, Queue_name},0).
get_queue_size(Db, Queue_name) ->                      %% is
    get_counter(Db,{admit_queue, Queue_name}).

update_queue_size(Db,Queue_name, Change) ->            %% is
    update_counter(Db,{admit_queue, Queue_name},Change).

%%
%% This counter is cumulative for the number of clients queued until log time.
%%

reset_queue_counter(Db) ->
    put_counter(Db, cumulative_queue_counter, 0).
get_queue_counter(Db) ->
    get_counter(Db, cumulative_queue_counter).

update_queue_counter(Db, Change) ->
    update_counter(Db, total_queue_counter, Change),
    update_counter(Db, cumulative_queue_counter, Change).

get_total_queue(Db) ->
    get_counter(Db, total_queue_counter).

%%
%% This counter is cumulative for the number of clients rejected until
%% log time.

reset_reject_counter(Db) ->
    put_counter(Db, cumulative_reject_counter, 0).
get_reject_counter(Db) ->
    get_counter(Db, cumulative_reject_counter).
update_reject_counter(Db, Change) ->
    update_counter(Db, total_reject_counter, Change),
    update_counter(Db, cumulative_reject_counter, Change).

get_total_reject(Db) ->
    get_counter(Db, total_reject_counter).
    

%%
%% Internal functions
%%

put_key(Db,Key,Value) ->
    ets:insert(Db,{Key,Value}).

get_value(Db,Key) ->
    case ets:lookup(Db,Key) of
        [{_,Value}] -> Value;
        [] -> []
    end.

remove_key(Db,Key) ->
    ets:delete(Db,Key).

put_counter(Db,Key,Counter) ->
    ets:insert(Db,{Key,Counter}).

get_counter(Db,Key) ->
    [{_,Counter}] = ets:lookup(Db,Key),
    Counter.

update_counter(Db,Key,Change) ->
    ets:update_counter(Db, Key, Change).



