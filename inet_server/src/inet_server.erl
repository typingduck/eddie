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
%%% File    : inet_server.erl
%%% Author  : Patrik Winroth <patrik@erix.ericsson.se>
%%% Purpose : Generic server to be used for inet services.
%%%           This generic inet server is intended to be added modules
%%%           that, each one, implements a protocol. E.g. HTTP, DNS and FTP. 
%%%          
%%% Created : 2 Mar 1998
%%%
%%% Modified: Keep track of the arrival rate of inet connections.
%%%           When the rejection rate goes from 0 to non-zero,
%%%           we use the arrival rate at this point in time
%%%           together with the rejection rate in admission control.
%%%           The admission rate will be calculated as
%%%           last_good_arrival_rate * (1 - reject_rate)
%%%----------------------------------------------------------------------
-module(inet_server).
-author('patrik@elrond').
-modified_by('jon@eddieware.org').
-modified_by('eric.yeo@ericsson.com.au').
-behavior(gen_server).

-include("inet_server.hrl").
-include("erlet.hrl").
-include("logger.hrl").


%% Don't use the fdsrv when debugging.
-define(USE_FDSRV, true).
-ifdef(USE_FDSRV).
-define(FD, case os:type() of
            {win32,nt} -> [];
            _          -> [{fd, bind_fd(Sock_type, Ip_address, Used_port)}]
            end).
-else.
-define(FD, []).
-endif.


-define(STATISTICS_INTERVAL, 1000).

%% Logging parameters.
-define(LOG_INTERVAL, 300000).
-define(LOG_QUEUE_COUNTER, log_queue_counter).

%% Statistics are based on this interval, check handle_info(statistics...)
%% before changing.
-define(UPDATE_STATISTICS, update_statistics).

%% This is the TCP port we listen to for control connections.
-define(CONTROL_PORT, 3456).

%% Starting preferred admission rate.
-define(INIT_RATE_DELTA, 0.03).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2 ]).
%% API
-export([stop/1,
     admission_rate_check/1,
	 init_database/2,
	 start_link/2,
	 create_tables/1,
	 load/1,
	 db_tables/0,
	 get_status/1,
	 status/0,
	 status/1,
	 status_node/0,
	 status_nodes/0,
     gather_site_status/0]).
%%
%% Starting of a server with database (mnesia) data already intitialized.
%%
start_link(Frontend_name, Ip_address) ->
    gen_server:start_link(?MODULE, [Frontend_name, Ip_address], []).

%%%----------------------------------------------------------------------
%%% API to the inet server
%%%----------------------------------------------------------------------
stop(Server_ref) ->
    gen_server:cast(Server_ref, stop).

get_status(Server_ref) ->
    gen_server:call(Server_ref, statistics).


%%
%% Server call-back functions.
%%
%% At time for init the database most already have been setup.
%%
init([Frontend_name, Ip_address_in]) ->
    process_flag(trap_exit, true),
    { X,Y,Z } = erlang:now(),
    random:seed(Z,Y,X),
    {ok, Ip_address} = inet:getaddr(Ip_address_in, inet),
    is_config:wait_for_tables(5000),
    %% Subscribe the tables that must be monitored for dynamic updates.
    lists:foreach(fun(Table) -> mnesia:subscribe({table, Table}) end, 
		  is_config:subscribed_tables()),
    Pid_db = is_service_db:new(Frontend_name, [set, private]),
    Control_pid_db = is_service_db:new(Frontend_name, [set, private]),
    Admit_ctrl = get_admit_ctrl(Frontend_name),
    {Cookie_name, Queue_spec} = Admit_ctrl#admit_ctrl.qos_config,
    Admit_db = is_db:new_adb(get_queue_names(Queue_spec)),
    Timer_pid = queue_timer:start(Admit_db, Admit_ctrl#admit_ctrl.time),
    Queue_pid = queue_srv:start(Admit_db, Admit_ctrl#admit_ctrl.qos_config),
    srv_table:load_table(http_fields),
    init_2(Frontend_name,Ip_address,Pid_db,
	   Control_pid_db,Admit_db,Queue_pid, Cookie_name).

init_2(Frontend_name,Ip_address,Pid_db,
       Control_pid_db,Admit_db,Queue_pid,Cookie_name) ->
    Fend = get_the_frontend(Frontend_name),
    Protocol_module = Fend#frontend.protocol_module,
    Endpoints_config = Fend#frontend.endpoints_config,
    Admit_on = Fend#frontend.admit_ctrl,
    Arrival_counting_period = Fend#frontend.arrival_counting_period,
    Backend_db = Protocol_module:db_new(),
    Server_core = Protocol_module:server(),
    case lists:keysearch(endpoints, 1, Server_core) of
	%% Configuration given in the protocol module
	{value, {endpoints, Endpoints}} ->
	    %% Set up sockets to listen on the socket endpoints, and spawn a process
	    %% that starts accepting incoming messages (requests).
	    Endpoints_state = endpoints(Frontend_name,
					Protocol_module, 
					Ip_address, 
					Endpoints,
					Endpoints_config,
					Pid_db,
					Control_pid_db,
					Admit_db,
					Admit_on,
					Queue_pid,
					Backend_db,
					Cookie_name),

	    %% Start subscribtion on reject percentages (for the admission control).
	    load_server:subscribe_reject(),

	    erlang:send_after(?STATISTICS_INTERVAL, self(), ?UPDATE_STATISTICS),
	    erlang:send_after(?LOG_INTERVAL, self(), ?LOG_QUEUE_COUNTER),
            erlang:send_after(Arrival_counting_period,
                              self(), calculate_arrival_rate),
	    {ok, #server_state{frontend_name = Frontend_name,
			       db_values = Endpoints_config,
			       endpoint_states = Endpoints_state,
			       pid_db = Pid_db,
			       control_pid_db = Control_pid_db,
			       admit_db = Admit_db,
			       queue_pid = Queue_pid,
			       reject_rate = 0.0,
			       backend_db = Backend_db,
                               arrival_rate = 0.0,
                               arrival_count = 0,
                               start_time = element(1,statistics(wall_clock)),
                               preferred_admit_rate = 0.0,
                               admit_rate_delta = ?INIT_RATE_DELTA,
                               admitted_rate = 0.0,
                               admitted_count = 0,
                               arrival_counting_period =
                                   Arrival_counting_period,
			       cookie_name = Cookie_name,
			       protocol_module = Protocol_module}};
	_ ->
	    ?FATAL(?F("No endpoints specified in ~p", [Protocol_module])),
	    {stop, lists:concat(["No endpoints specified in protocol module ", 
				 Protocol_module])}
    end.

get_the_frontend(Frontend_name) ->
    case get_frontend(Frontend_name) of
	{error, Reason} ->
	    ?FATAL(?F("Failed reading frontend data from database, reason: ~p", [Reason])),
	    exit(dberror);
	[] ->
	    ?FATAL(?F("Didn't find the frontend ~p, in the database", [Frontend_name])),
	    exit(dberror);
	Else ->
	    hd(Else)
    end.

%% Returns a list of {endpoint_name, listen_socket}
%% Endpoints_config is given in the frontend
endpoints(_, _, _, [], _, _, _, _, _, _, _, _) ->
    [];
endpoints(Frontend_name, Protocol_module, Ip_address, [EP | Rest],
	  Endpoints_config, Pid_db, Control_pid_db,
	  Admit_db, Admit_on, Queue_pid,
	  Backend_db, Cookie_name) ->
    
    {Endpoint_name, Sock_type, Default_port, Default_read_timeout,
     Default_keep_alive_timeout, Default_opts} = EP,
    
    case find_record(Endpoint_name, Endpoints_config) of
	not_found ->
	    endpoints(Frontend_name, Protocol_module, Ip_address, Rest,
		      Endpoints_config, Pid_db, Control_pid_db,
		      Admit_db, Admit_on, Queue_pid,
		      Backend_db, Cookie_name);
	Endpoint ->
	    {Used_port, Read_timeout, Keep_alive_timeout} =
		decide_conf(Endpoint_name,
			    Endpoints_config,
			    Default_port,
			    Default_read_timeout,
			    Default_keep_alive_timeout),
	    Initial_state =
		case is_core:spawn_control(
		       self(),
		       Protocol_module,
		       Endpoint_name,
		       Frontend_name,
		       Admit_db,
		       case Admit_on of
			   true ->
			       0;
			   _ ->
			       false
		       end,
		       Queue_pid,
		       Backend_db,
		       Ip_address,
		       Used_port,
		       Endpoint#endpoint_config.external_proxy,
		       Cookie_name) of
		    {Port, Pid, LSock} ->
			is_service_db:insert(Control_pid_db,
					     {Pid, Endpoint_name}),
			#endpoint_state{endpoint_name = Endpoint_name,
					ip_address = Ip_address,
					port = Used_port,
					read_timeout = Read_timeout,
					keep_alive_timeout =
					    Keep_alive_timeout,
					control_pid = Pid,
					control_socket = LSock,
					cmd =
                                      Endpoint#endpoint_config.external_proxy};
		    {error, Reason} ->
			exit(Reason)
		end,
	    [Initial_state |
	     endpoints(Frontend_name, Protocol_module, Ip_address, Rest,
		       Endpoints_config, Pid_db, Control_pid_db,
		       Admit_db, Admit_on, Queue_pid,
		       Backend_db, Cookie_name)]
    end.


decide_conf(Endpoint_name,
	    Endpoints_config,
	    Default_port,
	    Default_read_timeout,
	    Default_keep_alive_timeout) -> 
    Ep = find_record(Endpoint_name, Endpoints_config),
    {case Ep#endpoint_config.port of
	 default -> Default_port;
	 E -> E
     end,
     case Ep#endpoint_config.read_timeout of
	 default -> Default_read_timeout;
	 E -> E
     end,
     case Ep#endpoint_config.keep_alive_timeout of
	 default -> Default_keep_alive_timeout;
	 E -> E
     end}.

%%
%% Gen_server Handle Hooks
%%
%% handle_call 
handle_call(statistics, _, State) ->
    Admission_on = case catch get_frontend(State#server_state.frontend_name) of
		       [#frontend{admit_ctrl = Admit_ctrl}] ->
			   Admit_ctrl;
		       _ ->
			   dberror
		   end,
    F = fun(Endpoint_state) ->
		#statistics{frontend_name = State#server_state.frontend_name,
			    endpoint_name = Endpoint_state#endpoint_state.endpoint_name,
			    node_name = node(),
			    ip = Endpoint_state#endpoint_state.ip_address,
			    c_per_s = State#server_state.arrival_rate,
			    a_per_s = State#server_state.admitted_rate,
%%			    c_per_s = Endpoint_state#endpoint_state.c_per_s,
%%			    threads = Endpoint_state#endpoint_state.threads,
%%			    ratio_local = Endpoint_state#endpoint_state.ratio_local,
%%			    ratio_total = Endpoint_state#endpoint_state.ratio_total,
%%			    admit_reject = Endpoint_state#endpoint_state.admit_reject,
%%			    admit_total = Endpoint_state#endpoint_state.admit_total,
			    admission_ctrl = Admission_on,
			    admit_reject = is_db:get_total_reject(State#server_state.admit_db),
			    admit_total = is_db:get_sessions_size(State#server_state.admit_db),
			    admit_queued = queue_srv:get_queue_sizes(
			      State#server_state.queue_pid),
			    rejection_rate = State#server_state.reject_rate,
                            preferred_admit_rate =
                                State#server_state.preferred_admit_rate}
	end,
    {reply, lists:map(F,State#server_state.endpoint_states), State};
handle_call(check_admit_rate, _, Server_state) ->
    Result = admission_rate_check(Server_state),
    {reply, Result, Server_state};
handle_call(Request, From, State) ->
    {reply, ok, State}.

%% handle_cast
%% new_receiver is used by a TCP endpoint to start a new accepting process. 
handle_cast(stop, State) ->
    {stop, shutdown, State};
handle_cast(_, State) ->
    {noreply, State}.

%% handle_info
%% Receive rejection rate from the load server.
%% We also calculate the preferred admission rate.
%%
handle_info({reject, false}, State) -> %%%% No admission control
    %?INFO("inet_server:reject 1"),
    {noreply, State#server_state{reject_rate = false}}
    ;
handle_info({reject, P}, State) when P == 0.0 ->
    %?INFO("inet_server:reject 2"),
    {noreply, State#server_state{reject_rate = 0.0}}
    ;
%% Determine initial admission rate
handle_info({reject, P}, State)        
        when State#server_state.preferred_admit_rate == 0.0 ->
    %?INFO("inet_server:reject 3"),
    {Now, _} = statistics(wall_clock),
    Time_interval = Now - State#server_state.start_time,
    case Time_interval of
	0 ->
	    {noreply, State#server_state{reject_rate = P}};
	_ ->
	    Admit_rate =
		if
		    State#server_state.arrival_rate == 0.0 ->
			(State#server_state.arrival_count * 1000) /
			    Time_interval;
		    true ->
			    State#server_state.arrival_rate
		end,
	    {noreply, State#server_state{reject_rate = P,
					 preferred_admit_rate = Admit_rate}}
    end;
%%
%% Adjust admission rate
%% If we see the rejection rate decreasing then we reduce the number
%% of people to be admitted. If we see it increasing then we increase
%% it. The rate can be adjusted by adjusting 
%%
handle_info({reject, P}, State) ->     
    Admit_rate =
        if
            (P > State#server_state.reject_rate) ->
                State#server_state.preferred_admit_rate *
                    (1 - State#server_state.admit_rate_delta);
            (P < State#server_state.reject_rate) ->
                State#server_state.preferred_admit_rate *
                    (1 + State#server_state.admit_rate_delta);
            true ->
                State#server_state.preferred_admit_rate
        end,
    ?TEMP_INFO(?F("inet_server:Admit_rate = ~p, Reject_rate = ~p", [ Admit_rate, P ])),
    {noreply, State#server_state{reject_rate = P,
                                 preferred_admit_rate = Admit_rate}};

%% This message is used to inform the server that a client connection
%% has occurred and the server should increment the arrival_count.
%%
handle_info({increment_arrival_count, Count}, Server_state) ->
    {noreply, Server_state#server_state{arrival_count =
                          Server_state#server_state.arrival_count + Count}};

%%
%% This message is used to inform the server that some clients have
%% been admitted that the server should increment the admitted_count.
%%
handle_info({increment_admitted_count, Count}, Server_state) ->
    is_db:increment_total_sessions(Server_state#server_state.admit_db),
    {noreply, Server_state#server_state{admitted_count =
                          Server_state#server_state.admitted_count + Count}};

%% A client was queued.
%%
handle_info({report_queued, Endpoint_name}, State) ->
    {noreply,
     State#server_state{
       endpoint_states =
       lists:map(
	 fun(Es) when Es#endpoint_state.endpoint_name == Endpoint_name ->
		 Es#endpoint_state{
		   connections = Es#endpoint_state.connections + 1,
		   admit_total = Es#endpoint_state.admit_total + 1}
	 end,
	 State#server_state.endpoint_states)}};
 
%%
%% A client was admitted and got sent to the specified Node.
%%
handle_info({report_admitted, Endpoint_name, Node}, State) ->
    {noreply,
     State#server_state{
       endpoint_states =
       lists:map(
	 fun(Es) when Es#endpoint_state.endpoint_name == Endpoint_name ->
		 Es#endpoint_state{
		   connections = Es#endpoint_state.connections + 1,
		   admit_total = Es#endpoint_state.admit_total + 1,
		   ratio_total = Es#endpoint_state.ratio_total + 1,
		   ratio_local = case self() of
				    Node ->
					Es#endpoint_state.ratio_local + 1;
				    _ ->
					Es#endpoint_state.ratio_local
				 end};
            (Es) ->
                Es
	 end,
	 State#server_state.endpoint_states)}};

%%
%% A client that was rejected
%%
handle_info({report_reject, Endpoint_name}, State) ->
    {noreply,
     State#server_state{
       endpoint_states =
       lists:map(
	 fun
	     (Es) when Es#endpoint_state.endpoint_name == Endpoint_name ->
		 Es#endpoint_state{
		   connections = Es#endpoint_state.connections + 1};
	     (Es) ->
		 Es
	 end,
	 State#server_state.endpoint_states)}};

%%
%% This message is received every counting period.
%% We send ourselves another message in the same period of time
%% and we set the arrival_count and admitted_count to 0.
%%
handle_info(calculate_arrival_rate, Server_state) ->
    {Now, _} = statistics(wall_clock),
    erlang:send_after(Server_state#server_state.arrival_counting_period,
                      self(), calculate_arrival_rate),
    Admitted_rate = (Server_state#server_state.admitted_count * 1000.0) /
                       (Now - Server_state#server_state.start_time),
    Arrival_rate = (Server_state#server_state.arrival_count * 1000.0) /
                       (Now - Server_state#server_state.start_time),
    {noreply, Server_state#server_state{
		arrival_count = 0,
		admitted_count = 0,
		start_time = Now,
		admitted_rate = Admitted_rate,
		arrival_rate = Arrival_rate}};

%%
%% This message is received as a request to check if a client should
%% be admitted at this point in time.
%%
handle_info({check_admit_rate, Pid}, Server_state) ->
    Result = admission_rate_check(Server_state),
    Pid ! {Pid, Result},
    {noreply, Server_state};

handle_info(?UPDATE_STATISTICS, Server_state) ->
    erlang:send_after(?STATISTICS_INTERVAL, self(), ?UPDATE_STATISTICS),
    {noreply,
     Server_state#server_state
     {endpoint_states =
      lists:map(fun(Endpoint_state) ->
			Endpoint_state#
			    endpoint_state{connections = 0,
					   c_per_s = 
					   Endpoint_state#endpoint_state.connections}
		end, 
		Server_state#server_state.endpoint_states)}};

%% Periodically log the number of clients in the queue and reset
%% the counter.
handle_info(?LOG_QUEUE_COUNTER, State) ->
    erlang:send_after(?LOG_INTERVAL, self(), ?LOG_QUEUE_COUNTER),
    ?INFO(?F("~p clients queued and ~p clients rejected in the last ~p seconds",
	     [is_db:get_queue_counter(State#server_state.admit_db),
	      is_db:get_reject_counter(State#server_state.admit_db),
	      ?LOG_INTERVAL div 1000])),
    is_db:reset_queue_counter(State#server_state.admit_db),
    is_db:reset_reject_counter(State#server_state.admit_db),
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    case is_service_db:lookup(State#server_state.control_pid_db,
			      Pid) of
	[{Pid, Pid_endpoint_name}] ->
	    is_service_db:delete(State#server_state.control_pid_db, Pid),
	    Fend = get_the_frontend(State#server_state.frontend_name),
	    Admit_on = Fend#frontend.admit_ctrl,
	    Endpoint = lists:keysearch(Pid_endpoint_name, 1,
				       State#server_state.endpoint_states),
	    is_core:spawn_control(self(),
				  Fend#frontend.protocol_module,
				  Pid_endpoint_name,
				  State#server_state.frontend_name,
				  State#server_state.admit_db,
				  case Admit_on of
					   true ->
					  0;
				      _ ->
					  false
				  end,
				  State#server_state.queue_pid,
				  State#server_state.backend_db,
				  Endpoint#endpoint_state.ip_address,
				  Endpoint#endpoint_state.port,
				  Endpoint#endpoint_state.cmd,
				  State#server_state.cookie_name);
	[] ->
	    ok
    end,
    {noreply, State};
handle_info({mnesia_table_event, {Operation, Record, ActivityID}}, State) ->
    if
	State#server_state.activity_id == ActivityID ->
	    {noreply, State};
	true ->
	    %% We must make sure that the configurations for the endpoint
	    %% has not changed. If it has, restart the inet server.
	    case need_restart(State#server_state.db_values,
			      State#server_state.frontend_name) of
		true ->
		    {stop, shutdown, State};
		false ->
		    {noreply, State#server_state{activity_id = ActivityID}}
	    end
    end;
handle_info(_, State) ->
    {noreply, State}.



%% terminate
terminate(Reason, State) ->
    
    is_service_db:delete(State#server_state.pid_db),
    is_db:delete_adb(State#server_state.admit_db),
    Protocol_module = State#server_state.protocol_module,
    Protocol_module:db_delete(State#server_state.backend_db),

    %% Cancel subscription of tables.
    lists:foreach(fun(Table) ->
			  mnesia:unsubscribe({table, Table})
		  end, is_config:subscribed_tables()),

    ok.



%% Check consistancy with old database.
need_restart(Db_values_old, Frontend_name) ->
    case catch get_frontend(Frontend_name) of
	[Frontend] when record(Frontend, frontend) ->
	    not_same_config(Frontend#frontend.endpoints_config,
			    Db_values_old);
	Que ->
	    true
    end.

not_same_config([EP_new | Rest_new], 
		[EP_old | Rest_old]) when EP_new#endpoint_config.name ==
					  EP_old#endpoint_config.name,
					  EP_new#endpoint_config.port ==
					  EP_old#endpoint_config.port,
					  EP_new#endpoint_config.read_timeout ==
					  EP_old#endpoint_config.read_timeout,
					  EP_new#endpoint_config.keep_alive_timeout ==
					  EP_old#endpoint_config.keep_alive_timeout
					  ->
    not_same_config(Rest_new, Rest_old);
not_same_config([_|_], [_|_]) ->
    true;
not_same_config([], []) ->
    false.



%%
%% API for status reporting.
%%

status() ->
    status(text).

status(html) ->
    gather_status("\n", "  ", "%");
status(text) ->
    lists:flatten(gather_status("\n", "  ", [92,$0,$4,$5,92,$0,$4,$5])).

gather_status(NL, SP, Procent) ->
    Status_nodes = status_nodes(),
    %% First calc statistics for all nodes (site statistics)
    gather_data(Status_nodes, NL, SP, Procent).

gather_data(Status_nodes, NL, SP, Procent) ->
    Frontend_endpoint_pairs =
	find_frontend_endpoint_combos(Status_nodes),
    F = fun({Frontend, Endpoint}) ->
		format_status_string(
		  site_stats(Status_nodes, Frontend, Endpoint),
		  NL, SP, Procent)
	end,
    R = lists:map(F, Frontend_endpoint_pairs),
    "-- Statistics for the Frontends running on the web site:" ++ NL ++ R.

% returns a tuple containing data conerning entire site (ie. not just a node)
gather_site_status() ->
    Status_nodes = status_nodes(),
    Frontend_endpoint_pairs = find_frontend_endpoint_combos(Status_nodes),
    F = fun({Frontend, Endpoint}) ->
        {_, Site}=site_stats(Status_nodes, Frontend, Endpoint),
        Site
    end,
    lists:map(F, Frontend_endpoint_pairs).

format_status_string({List_of_this_frontend_statistics,
		      {Frontend_name,
		       Endpoint_name,
		       C_per_s,
		       A_per_s,
%%		       Threads,
%%		       Ratio_local,
%%		       Ratio_total,
		       Admission_ctrl,
		       Admit_reject,
		       Admit_total,
		       Admit_queued,
		       Rejection_rate}},
		     NL, SP, Procent) ->
    lists:concat([SP,"-- Summary for ",Frontend_name,"[",Endpoint_name,"]", NL,
		  SP,SP,SP,lists:flatten(io_lib:format("~.2f", [float(A_per_s)]))," admission/second.", NL,
		  SP,SP,SP,lists:flatten(io_lib:format("~.2f", [float(C_per_s)]))," connections/second.", NL,
%%		  SP,SP,SP,C_per_s," connections/second.", NL,
%%		  SP,SP,SP,Threads," living accept processes.", NL,
%%		  SP,SP,SP,Ratio_local," requests handled locally.", NL,
%%		  SP,SP,SP,Ratio_total - Ratio_local," requests handled",
%%		  " non-locally", NL,
%%		  SP,SP,SP, if Ratio_total == 0 -> "undefined";
%%			      true -> trunc(100 * Ratio_local/Ratio_total)
%%			   end,Procent," handled locally.", NL,
		  SP,SP,SP,
		  frmt_adm_ctrl(Admission_ctrl,NL,SP,Procent,Rejection_rate,
				Admit_reject,Admit_total,Admit_queued),
		  NL,
		  NL,
		 format_single_node_string(List_of_this_frontend_statistics, NL, SP, Procent)]).

frmt_adm_ctrl(false,NL,_,_,_,_,_,_) ->
    "Admission control is turned off." ++ NL;
frmt_adm_ctrl(true,NL,SP,P,Rej_rate,Adm_reject,Adm_tot,Adm_q) ->
    lists:concat(["Admission control is applied:", NL,
		  SP,SP,SP,SP,
		  Adm_tot," clients admitted.", NL,
		  SP,SP,SP,SP,
		  Adm_reject," clients rejected.", NL,
		  lists:append(
		    lists:map(fun ({Queue_name, Size}) ->
				      lists:concat([SP,SP,SP,SP,
						    Size,
						    " clients in ",
						    Queue_name,
						    " queue.",
						    NL])
			      end,
			      Adm_q))
		 ]).

format_single_node_string(List_of_this_frontend_statistics, NL, SP, Procent) ->
    [SP,SP,"-- Details:",
    lists:map(fun(#statistics{frontend_name = Frontend_name,
			      endpoint_name = Endpoint_name,
			      ip = {Ip3,Ip2,Ip1,Ip0},
			      node_name = Node_name,
			      c_per_s = C_per_s,
			      a_per_s = A_per_s,
%%			      threads = Threads,
%%			      ratio_local = Ratio_local,
%%			      ratio_total = Ratio_total,
			      admission_ctrl = Admission_ctrl,
			      admit_reject = Admit_reject,
			      admit_total = Admit_total,
			      admit_queued = Admit_queued,
			      rejection_rate = Rejection_rate,
                              preferred_admit_rate = Preferred_admit_rate}) ->
		      lists:concat([NL,
				    SP,SP,SP,"-- ",Ip3,".",Ip2,".",Ip1,".",
				    Ip0," (",Node_name,")", NL,
				    SP,SP,SP,SP,SP,lists:flatten(io_lib:format("~.2f", [float(Rejection_rate)])), " rejection indicator ratio", NL,
				    SP,SP,SP,SP,SP,lists:flatten(io_lib:format("~.2f", [float(Preferred_admit_rate)]))," preferred admission rate.", NL,
				    SP,SP,SP,SP,SP,lists:flatten(io_lib:format("~.2f", [float(A_per_s)]))," admissions/second.", NL,
				    SP,SP,SP,SP,SP,lists:flatten(io_lib:format("~.2f", [float(C_per_s)]))," connections/second.", NL,
%%				    SP,SP,SP,SP,SP,Threads," living accept processes.", NL,
%%				    SP,SP,SP,SP,SP,Ratio_local," requests handled locally.", NL,
%%				    SP,SP,SP,SP,SP,Ratio_total - Ratio_local,
%%				    " requests handled non-locally", NL,
%%				    SP,SP,SP,SP,SP, if Ratio_total == 0 -> "undefined";
%%						true -> trunc(100 * Ratio_local/Ratio_total)
%%					     end,Procent," handled locally.", NL,
				    if Admission_ctrl == false ->
					    "";
				       true ->
					    lists:concat(
					      [SP,SP,SP,SP,SP,SP,
					       Admit_total," clients admitted.", NL,
					       SP,SP,SP,SP,SP,SP,
					       Admit_reject," clients rejected.", NL,
					       lists:append(
						 lists:map(
						   fun ({Queue_name, Size}) ->
							   lists:concat(
							     [SP,SP,SP,SP,SP,SP,
							      Size,
							      " clients in ",
							      Queue_name,
							      " queue.",
							      NL])
						   end,
						   Admit_queued))
						])
				    end]);
		 ([]) ->
		      []
	      end, List_of_this_frontend_statistics)].	    

find_frontend_endpoint_combos(Status_nodes) ->
    %% Finds all unique {Frontend, Endpoint} combos.
    lists:flatten(
      case
	  lists:mapfoldl(fun(#statistics{frontend_name = FE,
					 endpoint_name = EP},
			     Acc) ->
				 case lists:keysearch({FE, EP}, 1, Acc) of
				     {_, {_, _}} ->
					 {[], Acc};
				     _ ->
					 {{FE, EP}, [{{FE, EP}, true} | Acc]}
				 end
			 end, [], Status_nodes) of
	  {List, _ } ->
	      List;
	  _ ->
	      []
      end).

site_stats(Status_nodes, Frontend_name, Endpoint_name) ->
    %% Returns statistics for Frontend, Endpoint combo.
    lists:mapfoldl(fun(N, {Frontend,
			   Endpoint,
			   C_per_sec,
			   A_per_sec,
%%			   Threads,
%%			   Ratio_local,
%%			   Ratio_total,
			   _,
			   Admit_reject,
			   Admit_total,
			   Admit_queued,
			   _}) when N#statistics.frontend_name == Frontend,
				    N#statistics.endpoint_name == Endpoint ->
			   {N, {Frontend,
				Endpoint,
				C_per_sec + N#statistics.c_per_s,
				A_per_sec + N#statistics.a_per_s,
%%				case N#statistics.threads of
%%				    NThreads when number(NThreads) ->
%%					Threads + NThreads;
%%				    _ ->
%%					Threads
%%				end,
%%				Ratio_local + N#statistics.ratio_local,
%%				Ratio_total + N#statistics.ratio_total,
				N#statistics.admission_ctrl,
				Admit_reject + N#statistics.admit_reject,
				Admit_total + N#statistics.admit_total,
				add_queue_sizes(Admit_queued,
						N#statistics.admit_queued),
				N#statistics.rejection_rate}};
		      (_, Sum) ->
			   {[], Sum}
			   %%{Sum, []}
			   %% Should be {[],Sum} ???
		   end, 
		   {Frontend_name,
		    Endpoint_name,
		    0,
%%		    0,
%%		    0,
		    0,
%%		    0,
		    undefined,
		    0,
		    0,
		    [],
		    0},
		   Status_nodes).

%% Given 2 lists of pairs representing the queue name/size pair,
%% return a new lists of pairs that is the sum of the sizes in
%% the 2 lists. This function is asymmetric in the sense that
%% the first lists could be a "sub-list" of the second. For example,
%% add_queue_sizes([{gold, 12}], [{gold, 3}, {silver, 4}, {bronze, 5}])
%% returns [{gold, 15}, {silver, 4}, {bronze, 5}].
%%
add_queue_sizes(Queue_sizes_1, Queue_sizes_2) ->
    lists:map(fun({Queue_name, Size}) ->
		      case lists:keysearch(Queue_name, 1, Queue_sizes_1) of
			  {value, {_, Add_size}} ->
			      {Queue_name, Size + Add_size};
			  _ ->
			      {Queue_name, Size}
		      end
	      end,
	      Queue_sizes_2).

status_node() ->
    case supervisor:which_children(servant_inet_server_sup) of
	List when list(List) ->
	    lists:map(fun({_, Pid, _, _}) ->
			      inet_server:get_status(Pid)
		      end, List);
	_ ->
	    []
    end.

%% The status report is a list of 'statistics' records
status_nodes() ->
    F = fun(Node) ->
		case rpc:call(Node, inet_server, status_node, []) of
		    {badrpc, _}   -> [];
		    Status_report -> Status_report
		end
	end,
    lists:flatten(lists:map(F, lists:sort([node() | nodes()]))).


%% Init functions.
%% Initializing of a server from configuration files.
%% Create mnesia databases for storing data.
init_database(Config_file, Nodes) ->
    is_config:create_mand_tables(Nodes).

load(Conf_file) ->
    is_config:run_load(Conf_file).

create_tables(Nodes) ->
    is_config:create_mand_tables(Nodes).

db_tables() ->
    is_config:db_tables().

%% Misc

get_frontend(FrontendName) ->
    is_db:read({frontend, FrontendName}).

get_admit_ctrl(FrontendName) ->
    case is_db:read({admit_ctrl, FrontendName}) of
	{error, Reason} ->
	    ?FATAL(?F("Failed reading admit_ctrl data from database, reason: ~p", [Reason])),
	    exit(dberror);
	[] ->
	    ?FATAL(?F("Did not find the admit_ctrl data in the database", [])),
	    exit(dberror);
	Else ->
	    hd(Else)
    end.

first(Pred, [H|T]) ->
    case Pred(H) of
	false ->
	    first(Pred, T);
	_ ->
	    H
    end;
first(_, []) ->
    false.

change_element(Pred, New, [H|T]) ->
    case Pred(H) of
	false ->
	    [H | change_element(Pred, New, T)];
	_ ->
	    [New | T]
    end;
change_element(_, _, []) ->
    ?ERROR("didn't find element"),
    [].

find(Name, [H|T]) ->
    case H#endpoint_state.endpoint_name of
	Name ->
	    H;
	_ ->
	    find(Name, T)
    end;
find(_, []) ->
    ?ERROR("didnt find element"),
    [].

find_record(E_name, [First | List]) when First#endpoint_config.name == E_name ->
    First;
find_record(E_name, [First | List]) ->
    find_record(E_name, List);
find_record(_, []) ->
    not_found.


%% Based on arrival and admission rate, determine if the client can
%% be admitted.
%%
admission_rate_check(Server_state) ->
    ?TEMP_INFO("admission rate check"),
    if
        Server_state#server_state.reject_rate == false ->
            ?TEMP_INFO("admission rate check: TRUE 1"),
           true;
        Server_state#server_state.reject_rate == 0.0 ->
            ?TEMP_INFO("admission rate check: TRUE 2"),
           true;
        true ->
           {Now, _} = statistics(wall_clock),
           Time_delta = Now - Server_state#server_state.start_time,
           Admit_rate =
	       if 
	           Time_delta == 0 ->
		       0.0;
		   true ->
                       (Server_state#server_state.admitted_count * 1000.0) /
                           Time_delta
	       end,
	    Admit_in_period =
		    Server_state#server_state.preferred_admit_rate *
		    Server_state#server_state.arrival_counting_period / 1000.0,
            ?TEMP_INFO(?F("Admit_rate=~p, Admit_count=~p, Admit_in_period=~p, preferred_admit_rate=~p",
            [ Admit_rate, Server_state#server_state.admitted_count, 
                Admit_in_period, Server_state#server_state.preferred_admit_rate ])),
                
	    if
		Server_state#server_state.admitted_count < Admit_in_period ->
		    if
			Admit_rate <
			3.0 * Server_state#server_state.preferred_admit_rate ->
                ?TEMP_INFO("admission rate check: TRUE 3"),
			    true;
			true ->
                ?TEMP_INFO("admission rate check: FALSE 2"),
			    false
		    end;
		true ->
            ?TEMP_INFO("admission rate check: FALSE 3"),
		    false
	    end
    end.


%% Given the queue spec in the QoS config, return a list of queue names.
get_queue_names(Queue_spec) ->
    lists:map(
      fun({Queue_name, _, _}) ->
	      Queue_name
      end,
      Queue_spec).

