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
%%% Sep 99 - Eric.Yeo@ericsson.com.au
%%%

-define(DEBUG(_Par1, _Par2), 
	io:format(lists:concat(["\n<", ?MODULE, ":", ?LINE, ">\n",
				_Par1, "\n</", ?MODULE, ">\n"]), _Par2)).

%%
%% NB! Be careful when changing in this file, quite a few modules 
%% includes it.
%%

-record(admit_ctrl,
	{frontend,           %%% Key field. 
	 time,               %%% Admit period. (the time period a client is
	                     %%% granted access, after first being admitted).
	 blocked_message = undefined,
                             %%% HTML message when client is blocked
                             %%% Overrides blocked_page
	 reject_message = undefined,
                             %%% HTML message when client is rejected
                             %%% Overrides reject_page
	 queue_message = undefined,
                             %%% HTML message when client is queued
                             %%% Overrides queue_page
	 blocked_page,
	 reject_page,
	 queue_page,
	 queue_places,
	 max_sessions,       %%% Max number of client sessions kept track of
                             %%% (at the same time).
	 qos_config
         }).


-record(admit_static,              %%% Static admit fields.              
	{ip,                       %%% Ip address.
	 status}).                 %%% Status.

-record(frontend,
	{name,                     %%% Name of the frontend, e.g. 
                                   %%% volvo.
	 protocol_module,          %%% Name of the module that implements the
                                   %%% protocol, e.g. 'http11'.
	 endpoints_config,         %%% A list of configurations that overrides
                                   %%% the Protocol:server() ones.
	 admit_ctrl,               %%% true | false.
         arrival_counting_period}).

-record(gen_storage,               %%% This storage is intended to be used by
	                           %%% protocol modules and erlets.
	{key,
	 key_value_pair}).

-record(backend,                   %%% Glue Marwin/Arthur.
	{endpoint_name,            %%% Name of endpoint (e.g. http_tcp).
	 spec,                     %%% e.g. {erlets, cookie_server} | 
                                   %%% {proxy, {"150.236.14.153", 8888}}.  
	 backend_node,             %%% e.g. eddie@arthur.
	 schedule_patterns}).      %%% e.g. / /france98 ...

-record(erlets,                    %%% Spec of erlets sequence contents.
	{name,                     %%% e.g. cookie_server.
	 list}).                   %%% e.g. [erlet_cookie, erlet_log].

-record(server_state,
	{frontend_name,            %%% Name of the frontend from which this
                               %%% server was initialized.
	 db_values,                %%% Values specified in the database.
	                           %%% Used to syncronize when reconfiguring.
	 endpoint_states,          %%% List of endpoint_state records.
	 pid_db,                   %%% Elements of type {Pid, Endpoint_name}.
         control_pid_db,           %%% Elements of type {Pid, Endpoint_name}.
	 admit_db,                 %%% Database for admit control.
	 queue_pid,                %%% Size of abovementioned queue.
	 reject_rate,              %%% Current rate for rejection of reqs.
	 activity_id,              %%% Current mnesia activity id.
	 backend_db,               %%% Database for needed backend info in
                                   %%% the protocol module, and its server
         arrival_rate,             %%% Last arrivate rate calculated. (connections/second)
         arrival_count,            %%% Use to caculate arrive rate for period.
         start_time,               %%% Time we started counting arrivals.
         preferred_admit_rate,     %%% Optimal admission rate.
         admit_rate_delta,         %%% Some small incremental ratio for
                                   %%% changing the preferred admission rate.
         admitted_rate,            %%% Last admitted rate calculated. (admissions/second)
         admitted_count,           %%% Use to caculate admitted rate for period.
         arrival_counting_period,  %%% The time between calculation of arrival
                                   %%% rate.
	 cookie_name,              %%% Name of the cookie use in QoS.
	 protocol_module}).        %%% Protocol module for the frontend

-record(endpoint_state,
	{endpoint_name,            %%% As defined in Protocol_module:server().
	 ip_address,               %%% Active IP address.
	 port,                     %%% Used port.
	 read_timeout,             %%% Timeout for read.
	 keep_alive_timeout,       %%% Timeout for keep-alive.
	 socket,                   %%% TCP: Listen_socket, UDP: Open_socket .  
	 socket_type,              %%% [tcp | udp].
	 active_pid,               %%% The process currently receiveing new
                               %%% client requests.
	 control_socket,           %%% Control listening socket
     control_pid,              %%% Control process pid
	 ratio_local = 0,          %%% Requests handled locally.
	 ratio_total = 0,          %%% Requests handled totally.
     admit_reject = 0,         %%% The number of client that have been 
	                           %%% rejected
	 admit_total = 0,          %%% Equal to the number of clients accepted
	 connections = 0,          %%% Number of accepted connections (since
                               %%% last reset of this counter).
	 c_per_s = 0,              %%% Connections per second latest measure
	                           %%% interval.
	 threads = 0,              %%% Number of spawned processes from this
                               %%% endpoint (that is still alive).
	 cmd}).                    %%% The command to start the relay

-record(report,                %%% Given defaults that are used in case
                               %%% of an illegal report
                               %%% being delivered to the inet_server.
	{ratio_local = 0,          %%%
	 ratio_total = 0,
	 admitted = 0}).           %%% 0 | 1.

-record(statistics,
	{frontend_name,            %%% Name of the frontend.
	 endpoint_name,            %%% Name of this endpoint.
	 node_name,                %%% Name of the node.
	 ip,                       %%% Ip address of endpoint.
	 c_per_s,                  %%% Connections / second.
	 a_per_s,                  %%% Admissions / second.
	 threads,                  %%% Number of living accept threads.
	 ratio_local,              %%% Number of request proxied locally.
	 ratio_total,              %%% Number of request proxied totally.
	 admission_ctrl,           %%% Admission control or not [true|false].
	 admit_reject,             %%% Number of clients admitted.
	 admit_total,              %%% Number of clients that has tried to connect.
	 admit_queued,             %%% Number of clients queued.
	 rejection_rate,           %%% Current rate of rejections (0-1).
         preferred_admit_rate}).   %%% Preferred admission rate.

-record(replicate,                 %%% Defines replication of data. 
	{source,                   %%% Source of data replication e.g.
                                   %%% http://www.ericsson.se/cgi-bin/
	 current_path,             %%% [path_A | path_B]
	 path_A,                   %%% Path to store source.
	 path_B,                   %%% Path to store source.
	 schedule_patterns}).      %%% Patterns that this replicate may serve.

-record(endpoint_config,           %%% Configuration of an endpoint.
	{name,                     %%% The name of the endpoint, must be the
                                   %%% same as in Protocol_module:server/0.
	 port,                     %%% What port the endpoint is to use.
	 read_timeout,             %%% Timeout when reading from the endpoint.  
	 keep_alive_timeout,       %%% Time to wait when pipelined requests
	                           %%% comes in.

         external_proxy}).         %% Specifications to run an extenal proxy.

-record(messages,
	{name,                     %% Atom for type of message.
         mesg}).                   %% String containing the message.
