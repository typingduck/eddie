-module(http_admit).
-author('patrik@erix.ericsson.se').
-modified_by('jon@eddiware.com'). % restructuring and updating to HTTP/1.1
-modified_by('eric.yeo@ericsson.com.au').
%%%----------------------------------------------------------------------
%%% File    : http_admit.erl
%%% Author  : Patrik Winroth <patrik@erix.ericsson.se>
%%%         : Jon Larsson <jon@eddieware.com> - restructuring and
%%% Created : 06 Oct 1998
%%% Purpose : Protocol Module for implementing an intelligent
%%%           Gateway for HTTP/1.0 and HTTP/1.1.
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
-id('$Id: http_admit.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:25 $ ').
-state('$State: Exp $ ').

%% External exports
%% used by module is_core (through module http)
-export([check_admit/7]).

-include("inet_server.hrl"). %% uses the admit_ctrl record

-define(INITIAL_RELOAD_TIME, 5).

%%----------------------------------------------------------------------
%% Function  : admit/5
%% Arguments : Server_ref,     - pid of the queue server, maintains queue
%%           : Admit_db,       - the admission database 
%%           : Frontend_name,  - name of the frontend, needed to access Mnesia
%%           : Ip,             - Ip address of client, used as key
%%           : AdmitP          - if true a client can be admitted
%% Returns   : true | {false, Redirection}
%% Purpose   : Determine if Eddie should admit this client or not.
%%           : If not admitted, return a redirect page. This is a valid
%%           : HTTP request and should be pattern matched to be handled
%%           : by the Eddie Erlets (erlet_admit_ctrl.erl). Possible
%%           : redirection pages are queued/rejection/blocked pages.
%%----------------------------------------------------------------------

%% WHEN TO ADMIT A NEW CLIENT?
%%
%% AdmitP is the resulting boolean from comparing a random number generated
%% when this client entered with the rejection rate of the frontend.
%% If the random number is greater than the rejection rate, a client can
%% be admitted.
%%
%% However there are a few exceptions.
%% * The clients that are always admitted are always admitted. No exceptions.
%% The always admitted clients are not counted in the maximum number of
%% sessions count.
%% * The frontend have a maximum number of allowed sessions. When this limit
%% is reached, no more clients are accepted until someone leaves.
%%
%% QUEUEING
%%
%% If a client is not admitted due to overload (represented by the random
%% number being less than the rejection rate) or that Eddie has reached the
%% maximum limit of clients - it is placed in the queue (if it is not banned
%% from the site with the AdmitNever option).
%%
%% Exceptions:
%% * Eddie has reached the maximum number of clients in the queue. If this
%% is the case, the client gets the rejection page and is not placed in any
%% queue.


%%
%% The client has arrived within the session time (AdmitTime) and is
%% automatically admitted again. The timer is cancelled and a new one is set.
%%

renew_time_interval(Admit_db,
		    Frontend_name,
		    Client_id) ->
    is_db:put_session(Admit_db,Client_id).


%% Estimates the queue time of this client (based on if the client has
%% advanced in the queue).
%% Returns: {New_place, New_reload_time, Estimate}
estimate_queue_time(Class, Old_place,Old_queue_relative,Sum_reload_time,Admit_db) ->
    New_queue_relative = is_db:get_dequeued_size(Admit_db, Class),
    Advance_in_queue = New_queue_relative - Old_queue_relative,
    New_place = Old_place - Advance_in_queue,
    if Advance_in_queue == 0 ->
	    {New_place, ?INITIAL_RELOAD_TIME, unknown};
       Sum_reload_time == 0 ->
	    {New_place, ?INITIAL_RELOAD_TIME, unknown};
       true ->
	    case trunc(New_place / (Advance_in_queue / Sum_reload_time)) of
		Time when Time < ?INITIAL_RELOAD_TIME ->
		    {New_place, Time, Time};
		Est ->
		    {New_place, ?INITIAL_RELOAD_TIME, Est}
	    end
    end.



%% Check if the client is in the static DB. If not, check the dynamic one.
status_ip(Admit_db, Client_id) ->
    {{Ip3, Ip2, Ip1, Ip0}, _} = Client_id,
    case is_db:read({admit_static, {Ip3, Ip2, Ip1, '*'}}) of
	[{_, _, Status}] ->
	    Status;
	_ ->
	    case is_db:read({admit_static, {Ip3, Ip2, '*', '*'}}) of
		[{_, _, Status}] ->
		    Status;
		_ ->
		    case is_db:read({admit_static, {Ip3, '*', '*', '*'}}) of
			[{_, _, Status}] ->
			    Status;
			_ ->
			    case is_db:read({admit_static, {Ip3, Ip2, Ip1, Ip0}}) of
				[{_, _, Status}] ->
				    Status;
				_ ->
				    %% Not any static status, return dynamic status.
				    is_db:get_session(Admit_db,Client_id)
			    end
		    end
	    end
    end.


%%
%% Name: check_admit/7
%% Purpose: 
%%      Check if we can let a session into the site.
%% Returns:
%%      true - if we're allowed in.
%%      false - if we're not allowed in; maybe queued.
%%

check_admit(Inet_server, Queue_pid, Admit_db, Frontend_name, Ip,
	    Http_header, Cookie_name) ->
    Client_id = get_client_id(Ip, Http_header, Cookie_name, Queue_pid),
    case status_ip(Admit_db, Client_id) of
        always ->              
            %% Always admit this client
            true;
        never ->               
            %% Banned from the site
            check_admit_never(Frontend_name);
        {Active_session} ->    
            %% Client has an active session
            renew_time_interval(Admit_db, Frontend_name, Client_id),
            true;
        {Old_place, Old_queue_relative, Sum_reload_time} -> 
            %% Client in queue
            check_admit_old_client(Inet_server,
				   gen_server:call(Inet_server,
						   check_admit_rate),
				   Old_place, Old_queue_relative,
                                   Sum_reload_time, Queue_pid, Admit_db,
                                   Frontend_name, Client_id);
        _ -> 
            %% New client
            check_admit_new_client(Inet_server,
                gen_server:call(Inet_server, check_admit_rate),
				   Queue_pid, Admit_db, Frontend_name, Client_id, Http_header)
    end.


%% Put up a page that says the client is banned from the site.
%%
check_admit_never(Frontend_name) ->
    [Admit_ctrl] = is_db:read({admit_ctrl, Frontend_name}),
    reply_blocked_page(Admit_ctrl).

%%
%% Since AdmitP is true Eddie admits the head of the queue (to keep a FIFO
%% order). But only if we have not reached the maximum number of sessions.
%%
check_admit_old_client(Inet_server,true,Old_place,Old_queue_relative,Sum_reload_time,
	               Server_ref,Admit_db,Frontend_name,Client_id) ->
    [Admit_ctrl] = is_db:read({admit_ctrl,Frontend_name}),
    Max_sessions = Admit_ctrl#admit_ctrl.max_sessions,
    case is_db:update_sessions_size(Admit_db,1) of
	Size when Size =< Max_sessions ->
	    check_admit_start_session_head_of_queue(
		 Inet_server,
                 Old_place,Old_queue_relative,
                 Sum_reload_time, Server_ref,
                 Admit_db,Client_id,Admit_ctrl);
	_ ->
	    is_db:update_sessions_size(Admit_db,-1),
	    check_admit_update_queued_session(
                 Old_place,Old_queue_relative,Sum_reload_time,
                 Admit_db,Client_id,Admit_ctrl)
    end;

%%
%% AdmitP is false and no new client can be admitted. Update queue stuff.
%%

check_admit_old_client(Inet_server,false,Old_place,Old_queue_relative,Sum_reload_time,
	   Server_ref,Admit_db,Frontend_name,Client_id) ->
    [Admit_ctrl] = is_db:read({admit_ctrl,Frontend_name}),
    check_admit_update_queued_session(
         Old_place,Old_queue_relative,Sum_reload_time,
         Admit_db,Client_id,Admit_ctrl).


%%
%% This client did not make it through to the backends,
%% update queue info and return a queue-redirect page
%%

check_admit_update_queued_session(
  Old_place,Old_queue_relative,Sum_reload_time,
  Admit_db,Client_id,Admit_ctrl) ->
    {_, Class} = Client_id,
    {New_place,New_reload_time,Estimate} = estimate_queue_time(Class,
							       Old_place,
							       Old_queue_relative,
							       Sum_reload_time,
							       Admit_db),
    is_db:put_queued_session(Admit_db,
			     Client_id,
			     Old_place,
			     Old_queue_relative,
			     Sum_reload_time + New_reload_time), % new sum_rel.
    reply_queue_page(Admit_ctrl, New_reload_time, New_place, Class,Estimate).
%    {redirect, lists:append([Admit_ctrl#admit_ctrl.queue_page,
%                             "?place=", integer_to_list(New_place),
%                             "&estimate=", 
%                                 case Estimate of
%                                      unknown ->
%                                          "unknown";
%                                      _ ->
%                                          integer_to_list(Estimate)
%                                 end,
%                              "&reload=", integer_to_list(New_reload_time)])}.


%%
%% Ok, finally Eddie can admit the head of the queue. It might be this
%% client or another client.
%%

check_admit_start_session_head_of_queue(
			    Inet_server,
                            Old_place,Old_queue_relative,Sum_reload_time,
			    Server_ref,Admit_db,Client_id,Admit_ctrl) ->
    %% Get client thats first in queue and start a session for it..
    Inet_server ! { increment_admitted_count, 1 },
    case queue_srv:out(Server_ref) of
	Client_id ->
	    is_db:put_session(Admit_db,Client_id),
	    true;
	Another_id ->
	    is_db:put_session(Admit_db,Another_id),
	    check_admit_update_queued_session(Old_place,Old_queue_relative,
					      Sum_reload_time,
					      Admit_db,Client_id,Admit_ctrl)
    end.


%%
%% AdmitP is true and Eddie can admit a client if we have not reached the
%% maximum number of clients. If there are clients in the queue, admit
%% the client in the head of the queue and queue this client. If not, admit
%% this client.
%%

check_admit_new_client(Inet_server,true,Server_ref,Admit_db,
		       Frontend_name,Client_id,Http_header) ->
    [Admit_ctrl] = is_db:read({admit_ctrl,Frontend_name}),
    Max_sessions = Admit_ctrl#admit_ctrl.max_sessions,
    case is_db:update_sessions_size(Admit_db,1) of
	Size when Size =< Max_sessions ->
        Inet_server ! { increment_admitted_count, 1 },
	    check_admit_start_session(Server_ref,Admit_db,Client_id,
	    			      Admit_ctrl,Http_header);
	_ ->
	    is_db:update_sessions_size(Admit_db,-1),
	    check_admit_queue_this_client(Server_ref,Admit_db,
					  Client_id,Admit_ctrl,Http_header)
    end;

%%
%% AdmitP is false and no clients can be admitted. Queue this client.
%%

check_admit_new_client(Inet_server,false,Server_ref,Admit_db,
		       Frontend_name,Client_id,Http_header) ->
    [Admit_ctrl] = is_db:read({admit_ctrl,Frontend_name}),
    check_admit_queue_this_client(Server_ref,Admit_db,Client_id,
				  Admit_ctrl,Http_header).


%%
%% If there are clients in the queue, admit the head of the queue. Otherwise
%% admit this client.
%%
check_admit_start_session(Server_ref,
			  Admit_db,
			  Client_id,
			  Admit_ctrl,
			  Http_header) ->
    %% Check if there are queued clients to start a session for.
    case queue_srv:out(Server_ref) of
	empty ->
	    is_db:put_session(Admit_db,Client_id),
	    true;
	Another_Id ->
	    %% this overwrites old queue-mark.
	    is_db:put_session(Admit_db,Another_Id),
	    check_admit_queue_this_client(
	      Server_ref,Admit_db,Client_id,Admit_ctrl,Http_header)
    end.

%%
%% Queue this client. If the queue is full, return the reject page.
%%
check_admit_queue_this_client(Server_ref,
			      Admit_db,
			      Client_id,
			      Admit_ctrl,
			      Http_header) ->
    case queue_srv:in(Server_ref,
		      Client_id,
		      Admit_ctrl#admit_ctrl.queue_places) of
	full ->
	    is_db:update_reject_counter(Admit_db, 1),
	    reply_reject_page(Admit_ctrl);
	Place ->
	    is_db:update_queue_counter(Admit_db, 1),
	    {_, Class} = Client_id,
	    Queue_relative = is_db:get_dequeued_size(Admit_db, Class),
	    is_db:put_queued_session(Admit_db,
				     Client_id,
				     Place,
				     Queue_relative,
				     ?INITIAL_RELOAD_TIME),
	    reply_queue_page(Admit_ctrl, ?INITIAL_RELOAD_TIME, Place, Class,unknown)
            %{redirect, lists:append([Admit_ctrl#admit_ctrl.queue_page,
            %                         "?place=", integer_to_list(Place),
            %                         "&estimate=unknown",
            %                         "&reload=",
            %                         integer_to_list(?INITIAL_RELOAD_TIME)])}
    end.


%% Given the IP address, HTTP header and the QoS cookie name,
%% we compose a client identifier which is made up of the
%% client IP and the cookie value. If the cookie does not
%% exists, we use the atom not_found.
get_client_id(Ip, Http_header, Cookie_name, Queue_pid) ->
    Id = {Ip, queue_srv:get_client_queue_name(
		Queue_pid,
		http_fields:get_cookie_value(
		  Cookie_name,
		  erlet_utils:keysearch_all_values('Cookie', Http_header)))},
    Id.


%% If the blocked message file was specified, use it.
%% Otherwise, default to URL redirection.
%%
reply_blocked_page(Admit_ctrl) ->
    case Admit_ctrl#admit_ctrl.blocked_message of
	undefined ->
	    {redirect, Admit_ctrl#admit_ctrl.blocked_page};
	Mesg ->
	    reply_message(Mesg)
    end.

%% If the queue message file was specified, use it.
%% Otherwise, default to URL redirection.
%%
reply_queue_page(Admit_ctrl, Reload_time, Place, Class, Estimate) ->
    String_estimate =
        if integer(Estimate) ->
               integer_to_list(Estimate);
           true ->
               "unknown"
        end,
    case Admit_ctrl#admit_ctrl.queue_message of
	undefined ->
	    {redirect, lists:append([Admit_ctrl#admit_ctrl.queue_page,
				     "?reload=",
				     integer_to_list(Reload_time),
				     "&place=", integer_to_list(Place),
				     "&class=", atom_to_list(Class),
                                     "&estimate=", String_estimate])};
	Mesg ->
	    Mesg1 = subst_string(Mesg,
				 "@meta_refresh",
				 lists:append(
				   ["<META HTTP-EQUIV=\"Refresh\" CONTENT=\"",
				    integer_to_list(Reload_time),
				    "\">"])),
	    Mesg2 = subst_string(Mesg1,
				 "@place",
				 integer_to_list(Place)),
	    Mesg3 = subst_string(Mesg2,
				 "@class",
				 atom_to_list(Class)),
            Mesg4 = subst_string(Mesg3,
                                 "@estimate",
                                 String_estimate),
	    reply_message(Mesg4)
    end.

%% If the reject message file was specified, use it.
%% Otherwise, default to the URL method.
%%
reply_reject_page(Admit_ctrl) ->
    case Admit_ctrl#admit_ctrl.reject_message of
	undefined ->
	    {redirect, Admit_ctrl#admit_ctrl.reject_page};
	Mesg ->
	    reply_message(Mesg)
    end.

%% Generate a HTTP header for error 503 and append the Mesg to it.
%%
reply_message(Mesg) ->
    Date = srv_parse:enc_current_date(),
    {message, lists:append([
			 "HTTP/1.0 503 Service Unavailable\r\n"
			 "Server: Eddie-gateway/1.4\r\n"
			 "Connection: close\r\n"
			 "Date: ",
			 Date,
			 "\r\n"
			 "Expires: ",
			 Date,
			 "\r\n"
			 "Content-Type: text/html\r\n"
			 "Content-Length: ",
			 integer_to_list(length(Mesg)),
			 "\r\n\r\n",
			 Mesg])}.

%% Finds the Pattern in Mesg and replace it with Subst.
%%
subst_string([], _, Subst) ->
    [];
subst_string(Mesg, Pattern, Subst) ->
    case lists:prefix(Pattern, Mesg) of
	true ->
	    lists:append(Subst,
			 subst_string(lists:nthtail(length(Pattern), Mesg),
				      Pattern, Subst));
	false ->
	    [hd(Mesg)|subst_string(tl(Mesg), Pattern, Subst)]
    end.
