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
%%%    21/07/00 clement.lyons@eddieware.org
%%%    Call function gen_tcp:recv/3 instead of gen_tcp:recv/2 with timeout
%%%    otherwise the process may hang waiting for a packet and will not exit.

-module(is_core).
-author('patrik@elrond').
-modified_by('jon@serc.rmit.edu.au').
-modified_by('eric.yeo@ericsson.com.au').
-modified_by('clement.lyons@eddieware.org').
-include("inet_server.hrl").
-include("erlet.hrl").
-include("logger.hrl").


-define(server_name,"Eddie - Intelligent Gateway/1.0").
-define(RESTART_COUNT, 15).
-define(PORT_RECEIVE_TIMEOUT, 2000).

-export([find_node/5,
         spawn_control/12,
         start_listening/11,
         handle_control/10,
	 execute_erlets/4,
	 start_proxy/2,
	 get_error_response/1]).


%% Find a Node among Nodes && Backend_nodes that can take care of the request. 
find_node(Protocol_module, Endpoint_name, Nodes, Backend_nodes, Pattern) ->
    until_true(fun(Node) ->
                       case lookup_patterns(Node, Backend_nodes) of
                           [] ->
                               false;
                           Pattern_list  ->
                               %% This node has some rsrss for this request.
                               %% Test if it has the exact resource required.
                               case run_protocol_module(Protocol_module,
                                                        Pattern,
                                                        Pattern_list) of
                                   false ->
                                       false;
                                   Backend ->
                                       {ok, Backend, Node}
                               end
                       end
               end, Nodes).


%% Looks up all 'node; among the possible ones and
%% returns their type(s) (proxy/erlet) & patterns.
lookup_patterns(Node,
		[#backend{backend_node = Node, 
			  spec = Backend_spec,
			  schedule_patterns = Schedule_patterns} | Rest]) ->
    [{Backend_spec, Schedule_patterns} | lookup_patterns(Node, Rest)];
lookup_patterns(Node, [First | Rest]) ->
    lookup_patterns(Node, Rest);
lookup_patterns(_, []) ->
    [].


run_protocol_module(Protocol_module, Pattern, [{Back_spec, Patterns} | Rest]) ->
    case Protocol_module:schedule_match(Pattern, Patterns) of
        true ->
            Back_spec;
        _ ->
            run_protocol_module(Protocol_module, Pattern, Rest)
    end;
run_protocol_module(_, _, []) ->
    false.



%% Misc

foreach(Fun, [H|T]) ->
    Fun(H),
    foreach(Fun, T);
foreach(_, _) ->
    ok.

until_true(Pred, [H|T]) ->
    case Pred(H) of
	false ->
	    until_true(Pred, T);
	Value ->
	    Value
    end;
until_true(_, []) ->
    false.


%% Spawn a process to listen on the control port.
%% Randomly chooses a port between 1024 and 5024.
%% Return the port and pid.
%%
spawn_control(Server_pid,              % Pid of process we talk to
              Protocol_module,         % Http protocol I presume
              Endpoint_name,           % What endpoint are we
              Frontend_name,           % What frontend are we
              Admit_db,                % Admission control db
	      Reject_rate,             % To indicate if admit control is on
              Queue_pid,               % Pid of the queue
              Backend_db,              % Backend db
	      Ip,                      % IP address the proxy listens to
	      Port,                    % Port the proxy listens to
	      Cmd,                     % Command to start extenal proxy
	      Cookie_name) ->          % Name of the cookie used in QoS
    { S1, S2, S3 } = erlang:now(),
    random:seed(S3,S2,S1),
    LPort = 1024 + random:uniform(4000),
    case gen_tcp:listen(LPort, [{backlog, 1024},
				{active, false},
				{packet, 0},
				{reuseaddr, true},
				{keepalive, true}]) of
        {ok, LSock} ->
	    Str_cmd = list_to_cmd_str(
			cmd_substitution(Cmd,
					 LPort,
					 Ip,
					 Port)),
	    Pid = spawn_link(?MODULE, start_listening,
			     [Server_pid,
			      Protocol_module,
			      Endpoint_name,
			      Frontend_name,
			      Admit_db,
			      Reject_rate,
			      Queue_pid,
			      Backend_db,
			      LSock,
			      Str_cmd,
			      Cookie_name]),
	    {Port, Pid, LSock};
	_ ->
	    spawn_control(Server_pid,
			  Protocol_module,
			  Endpoint_name,
			  Frontend_name,
			  Admit_db,
			  Reject_rate,
			  Queue_pid,
			  Backend_db,
			  Ip,
			  Port,
			  Cmd,
			  Cookie_name)
    end.



%% Spawn the external proxy and starts listening for control
%% connections.
%%
start_listening(Server_pid,
		Protocol_module,
		Endpoint_name,
		Frontend_name,
		Admit_db,
		Reject_rate,
		Queue_pid,
		Backend_db,
		LSock,
		Cmd,              % Command to start proxy
		Cookie_name) ->
    %% Spawn the external proxy here.
    spawn_link(?MODULE, start_proxy, [Cmd, ?RESTART_COUNT]),
    control_listening(Server_pid,
		      Protocol_module,
		      Endpoint_name,
		      Frontend_name,
		      Admit_db,
		      Reject_rate,
		      Queue_pid,
		      Backend_db,
		      LSock,
		      Cookie_name).

%% We spawn the proxy through a port and wait for any messages.
%% If we get told that the proxy died, we restart the proxy.
%%
start_proxy(Cmd, 0) ->
    ?FATAL(?F("Unable to restart relay for ~p seconds", [?RESTART_COUNT]));
start_proxy(Cmd, Restart_count) ->
    process_flag(trap_exit, true),
    case catch open_port({spawn, Cmd}, []) of
	Port when port(Port) ->
	    link(Port),
	    monitor_port(Port, Cmd);
	_ ->
	    receive %% wait for 1 second before retrying
		after (?RESTART_COUNT - Restart_count) * 5000 ->
			timeout
		end,
	    start_proxy(Cmd, Restart_count-1)
    end.

%% Wait for message to arrive. If a EXIT message arrive, restart
%% the proxy.
%%	    
monitor_port(Port, Cmd) ->
    receive
	{Port, closed} ->
	    start_proxy(Cmd, ?RESTART_COUNT);
	{'EXIT', Port, Reason}  ->
	    start_proxy(Cmd, ?RESTART_COUNT);
	{'EXIT', Pid, Reason} ->
	    Port ! {self(), close};
	_ ->
	    monitor_port(Port, Cmd)
    end.
	    

%% Listen on the control socket for connections and spawn
%% a process each time to handle a new connection.
%%
control_listening(Server_pid,
		  Protocol_module,
		  Endpoint_name,
		  Frontend_name,
		  Admit_db,
		  Reject_rate,
		  Queue_pid,
		  Backend_db,
		  LSock,
		  Cookie_name) ->
    %random:seed(),
    case handle_connection(Server_pid,
			   Protocol_module,
			   Endpoint_name,
			   Frontend_name,
			   Admit_db,
			   Reject_rate,
			   Queue_pid,
			   Backend_db,
			   gen_tcp:accept(LSock),
			   Cookie_name) of
	false ->
	    false;
	_ ->
	    control_listening(Server_pid,
			      Protocol_module,
			      Endpoint_name,
			      Frontend_name,
			      Admit_db,
			      Reject_rate,
			      Queue_pid,
			      Backend_db,
			      LSock,
			      Cookie_name)
    end.


%% If the socket is accepted properly, we spawn a process
%% to handle it.
%%
handle_connection(Server_pid,
		  Protocol_module,
		  Endpoint_name,
		  Frontend_name,
		  Admit_db,
		  Reject_rate,
		  Queue_pid,
		  Backend_db,
		  {ok, Sock},
		  Cookie_name) ->
    case inet:peername(Sock) of
	{ok, {{127, 0, 0, 1}, _}} ->
	    spawn_link(?MODULE,
		  handle_control,
		  [Server_pid,
		   Protocol_module,
		   Endpoint_name,
		   Frontend_name,
		   Admit_db,
		   Reject_rate,
		   Queue_pid,
		   Backend_db,
		   Sock,
		   Cookie_name]);
	_ ->
	    ?INFO(?F("Access denied~n", [])),
	    gen_tcp:close(Sock),
	    false
    end.


%% Continuously read and packet, process it and handle
%% the requests implicit in the packet.
%%
handle_control(Server_pid,
	       Protocol_module,
	       Endpoint_name,
	       Frontend_name,
	       Admit_db,
	       Reject_rate,
	       Queue_pid,
	       Backend_db,
	       Sock,
	       Cookie_name) ->
    %random:seed(),
    case get_packet(Sock) of
        {ok, Packet} ->
            case process_packet(Packet) of
		{1, Uid, CIp, LIp, LPort, Http_header} -> % A request packet!
			    case admission(Server_pid,
					   Protocol_module,
					   Endpoint_name,
					   Frontend_name,
					   Admit_db,
					   Reject_rate,
					   Queue_pid,
					   Backend_db,
					   Uid, CIp, LIp, LPort,
					   Http_header, Cookie_name) of
				error ->
				    ?DEBUG("Admission internal error", []),
				    gen_tcp:close(Sock),
				    false;
				Reply_packet ->
				    gen_tcp:send(Sock, Reply_packet),
				    handle_control(Server_pid,
						   Protocol_module,
						   Endpoint_name,
						   Frontend_name,
						   Admit_db,
						   Reject_rate,
						   Queue_pid,
						   Backend_db,
						   Sock,
						   Cookie_name)
			    end;
		{4, Uid, LIp, LPort, {BIp1, BIp2, BIp3, BIp4}, BPort, CIp} ->
                    Backend_nodes = is_db:read({backend, Endpoint_name}),
                    BIp = integer_to_list(BIp1) ++ "." ++
                          integer_to_list(BIp2) ++ "." ++
                          integer_to_list(BIp3) ++ "." ++
                          integer_to_list(BIp4),
		    case get_node_from_ip_port(Backend_nodes, BIp, BPort) of
			false ->
			    ok;
			Node ->
			    Protocol_module:db_remove_BE_node(Backend_db,
							      Frontend_name,
							      CIp,
							      LPort,
							      Node)
		    end,
		    handle_control(Server_pid,
				   Protocol_module,
				   Endpoint_name,
				   Frontend_name,
				   Admit_db,
				   Reject_rate,
				   Queue_pid,
				   Backend_db,
				   Sock,
				   Cookie_name);
                _ ->
		    ?DEBUG("Unknown packet type", []),
                    gen_tcp:close(Sock),
		    false
            end;
    {error, timeout} ->
            handle_control(Server_pid,
              Protocol_module,
              Endpoint_name,
              Frontend_name,
              Admit_db,
              Reject_rate,
              Queue_pid,
              Backend_db,
              Sock,
              Cookie_name);
    _ ->
            gen_tcp:close(Sock),
	    false
    end.


%% Get the full packet.
%%
get_packet(Sock) ->
    case get_packet_length(Sock) of
        {ok, Length} ->
            ?DEBUG("Packet length is ~p~n", [Length]),
	    if
		Length > 4, Length < 4096 ->
		    case catch gen_tcp:recv(Sock, Length-4, ?PORT_RECEIVE_TIMEOUT) of
			{'EXIT', Reason} ->
			    ?DEBUG("Socket error", []),
			    error;
			Normal ->
			    Normal
		    end;
		true ->
		    ?DEBUG("Length error ~p", [Length]),
		    gen_tcp:close(Sock),
		    error
	    end;
        Error ->
            Error
    end.

%% Get the next 4 bytes as an integer for the length of the
%% rest of the packet.
%%
get_packet_length(Sock) ->
    case catch gen_tcp:recv(Sock, 4, ?PORT_RECEIVE_TIMEOUT) of
	{'EXIT', Reason} ->
	    ?DEBUG("Socket error (get_packet_length 1)", []),
	    error;
        {ok, [L1, L2, L3, L4]} ->
            {ok, 256 * (256 * ((256 * L1) + L2) + L3) + L4};
        Error ->
%%	    ?DEBUG("Socket error (get_packet_length 2)", []),
            Error
    end.


%% Determine the type of packet we have.
%%
process_packet([Cmd, Id1, Id2, Id3, Id4 | Rest]) ->
    case Cmd of
        1 ->
            parse_request_body([Id1, Id2, Id3, Id4], Rest);
	4 ->
	    parse_be_error_body([Id1, Id2, Id3, Id4], Rest);
        _ ->
            %% Unrecognised packet
	    ?DEBUG("Unknown packet type", []),
            error
    end;
process_packet(_) ->
    ?DEBUG("Unknown packet type", []),
    error.


%% Determine the contents of the body of the packet
%% and act on it.
%%
parse_request_body(Uid, Body) ->
    case parse_request_body2(Body) of
        {CIp, LIp, LPort, Http_header} ->
	    {1, Uid, CIp, LIp, LPort, Http_header};
        _ ->
	   ?DEBUG("Unable to recognise request body", []),
           error
    end.

%% The body for the request packet is compose of a 4 byte IP address
%% of the client and a even number of strings representing the
%% HTTP header name/value pairs.
%%
parse_request_body2([CIp1, CIp2, CIp3, CIp4,
                     LIp1, LIp2, LIp3, LIp4,
                     LPort1, LPort2 | Rest]) ->
    parse_request_body3(Rest,
                        {CIp1, CIp2, CIp3, CIp4},
                        {LIp1, LIp2, LIp3, LIp4},
                        LPort1 * 256 + LPort2,
                        []);
parse_request_body2(_) ->
    ?DEBUG("Unable to recognise request body", []),
    error.

%% Read the name/value pairs of the HTTP header.
%%
parse_request_body3([], CIp, LIp, LPort, Pairs) ->
    {CIp, LIp, LPort, lists:reverse(Pairs)};
parse_request_body3(Body, CIp, LIp, LPort, Pairs) ->
    case parse_string(Body, []) of
        error ->
            ?DEBUG("Unable to recognise request body (name)", []),
            error;
        {Rest, Name} ->
            case parse_string(Rest, []) of
                error ->
                    ?DEBUG("Unable to recognise request body (value)", []),
                    error;
                {Rest1, Value} ->
                    parse_request_body3(Rest1, CIp, LIp, LPort,
                                        [{list_to_atom(Name), Value} | Pairs])
            end
    end.


%% Construct the BE error packet
%%
parse_be_error_body(Uid, [LIp1, LIp2, LIp3, LIp4,
			  LPort1, LPort2,
			  BIp1, BIp2, BIp3, BIp4,
			  BPort1, BPort2,
                          CIp1, CIp2, CIp3, CIp4]) ->
    {4, Uid,
     {LIp1, LIp2, LIp3, LIp4}, LPort1 * 256 + LPort2,
     {BIp1, BIp2, BIp3, BIp4}, BPort1 * 256 + BPort2,
     {CIp1, CIp2, CIp3, CIp4}};
parse_be_error_body(_, _) ->
    ?DEBUG("Unable to recognise BE error packet body", []),
    error.

%% Read a null-terminated string.
%%
parse_string([], _) ->
    error;
parse_string([0 | Rest], So_far) ->
    {Rest, lists:reverse(So_far)};
parse_string([C | Rest], So_far) ->
    parse_string(Rest, [C|So_far]).


%% This is where we check for admission of clients
%%
admission(Server_pid,
	  Protocol_module,
	  Endpoint_name,
	  Frontend_name,
	  Admit_db,
	  Reject_rate,
	  Queue_pid,
	  Backend_db,
	  Uid, CIp, LIp, LPort,
	  Http_headers, Cookie_name) ->
    Server_pid ! {increment_arrival_count, 1},
    Me = self(),
    Admit_client =
	case Reject_rate of
	    false ->  %% No admission control, admit all.
		true;
	    _ ->
		Protocol_module:check_admit(Server_pid,
					    Queue_pid,
					    Admit_db,
					    Frontend_name,
					    CIp,
					    Http_headers,
					    Cookie_name)
	end,
    load_server:init_seed(),
    case Admit_client of
	true ->
	    lookup_backend(Server_pid,
			   Protocol_module,
			   Endpoint_name,
			   Frontend_name,
			   Admit_db,
			   Reject_rate,
			   Queue_pid,
			   Backend_db,
			   Uid, CIp, LIp, LPort, Http_headers);
	{redirect, Uri} ->
	    lookup_backend(Server_pid,
			   Protocol_module,
			   Endpoint_name,
			   Frontend_name,
			   Admit_db,
			   Reject_rate,
			   Queue_pid,
			   Backend_db,
			   Uid, CIp, LIp, LPort,
			   [{method, "get"},
			    {uri, Uri},
			    {version, "HTTP/1.0"}]);
	{message, Mesg} ->
	    make_message_packet(Uid, Mesg)
    end.



%% We need to find a suitable backend to send our request to.
%% Then we need to work out if the request is an erlet or http.
%% If erlet, we have to send to the request to the erlet and
%% send back the reply. If http, we simply inform the client.
%%
lookup_backend(Server_pid,
	       Protocol_module,
	       Endpoint_name,
	       Frontend_name,
	       Admit_db,
	       Reject_rate,
	       Queue_pid,
	       Backend_db,
	       Uid, CIp, LIp, LPort, Http_header) ->
    Backend_nodes = is_db:read({backend, Endpoint_name}),
    case Protocol_module:db_get_BE_nodes(Backend_db,
					 Frontend_name,
					 CIp, LPort) of
	error ->
	    make_message_packet(Uid, get_error_response("404"));
	Node_list ->
	    find_best_backend(Server_pid,
			      Protocol_module,
			      Endpoint_name,
			      Frontend_name,
			      Admit_db,
			      Reject_rate,
			      Queue_pid,
			      Backend_db,
			      Uid, CIp, LIp, LPort, Http_header,
			      Backend_nodes,
			      Node_list)
    end.



%% Determine which is the best backend to use for the request.
%%
find_best_backend(Server_pid,
		  Protocol_module,
		  Endpoint_name,
		  Frontend_name,
		  Admit_db,
		  Reject_rate,
		  Queue_pid,
		  Backend_db,
		  Uid, CIp, LIp, LPort, Http_header,
		  Backend_nodes,
		  Node_list) ->
    case find_node(Protocol_module,
		   Endpoint_name,
		   Node_list,
		   Backend_nodes,
		   Http_header) of
        false ->
	    % Since we don't have a list of nodes we previously used,
	    % we simply choose a node that has the least load.
            case load_server:best_nodes() of
		[] -> make_message_packet(Uid, get_error_response("500"));
		Nodes -> Result = find_node(Protocol_module,
			       Endpoint_name,
			       Nodes,
			       Backend_nodes,
			       Http_header),
            use_backend(Result,
			Server_pid,
			Protocol_module,
			Endpoint_name,
			Frontend_name,
			Admit_db,
			Reject_rate,
			Queue_pid,
			Backend_db,
				     Uid, CIp, LIp, LPort, Http_header, Backend_nodes)
	    end;
        Result ->
            use_backend(Result, Server_pid, Protocol_module,
			Endpoint_name, Frontend_name, Admit_db,
			Reject_rate, Queue_pid, Backend_db,
			Uid, CIp, LIp, LPort, Http_header, Backend_nodes)
    end.


%% Need to run the intended erlet to get the result and
%% send it to the client.
%%
use_backend({ok, {erlets, Erlets}, Node}, Server_pid, Protocol_module,
	    Endpoint_name, Frontend_name, Admit_db,
	    Reject_rate, Queue_pid, Backend_db,
	    Uid, CIp, LIp, LPort, Http_header, Backend_nodes) ->
    case http_to_erlet_req(Http_header) of
        error ->
            make_message_packet(Uid, get_error_response("400"));
        Erlet_req ->
            % ttey - HACK HACK HACK
            % We ignore the input Node because for agentless, this
            % Erlang node does not exist. We simply run it on the
            % existing node.
	    use_erlets(node(), Erlets, Server_pid, Protocol_module,
		       Endpoint_name, Frontend_name, Admit_db,
		       Reject_rate, Queue_pid, Backend_db,
		       Uid, CIp, LIp, LPort, Http_header,
		       Backend_nodes, Erlet_req)
    end;

%%
%% A backend was found to proxy. Inform the client of the fact.
%%

use_backend({ok, {proxy, Server_ip, Server_port}, Node}, Server_pid,
	    Protocol_module, Endpoint_name, Frontend_name, _, _, _, Backend_db,
	    Uid, CIp, LIp, LPort, Http_header, _) ->
    case inet:getaddr(Server_ip, inet) of
        {ok, Ip} ->
	    Protocol_module:db_add_BE_node(Backend_db,Frontend_name,
					   CIp,LPort,Node),
	    %Server_pid ! {report_admitted, Endpoint_name, Node},
            make_reply_packet(Uid, Ip, Server_port);
        _ ->
	    error
    end;

%%
%% Unable to match a backend. We tell the client that it is not found.
%%

use_backend(Error, Server_pid, _, Endpoint_name, _, _, _, _, _, 
	    Uid, _, _, _, _, _) ->
    % Update statistics here
    %Server_pid ! {report_reject, Endpoint_name},
    make_message_packet(Uid, get_error_response("404")).


%% We have a run the request erlets on the specified node and construct
%% a packet.
%%
use_erlets(Node, Erlets, Server_pid, Protocol_module,
	   Endpoint_name, Frontend_name, Admit_db,
	   Reject_rate, Queue_pid, Backend_db,
	   Uid, CIp, LIp, LPort, Http_header, Backend_nodes,
	   Erlet_req) ->
    case spawn_erlet_request(Node, Erlets, Http_header, Erlet_req) of
	not_alive ->
	    case Protocol_module:db_get_BE_nodes(Backend_db,
						 Frontend_name,
						 CIp, LPort) of
		error ->
		    error;
		Node_list ->
		    New_node_list =
			Protocol_module:db_remove_BE_node(Backend_db,
							  Frontend_name,
							  CIp, LPort,
							  Node),
		    find_best_backend(Server_pid,
				      Protocol_module, Endpoint_name,
				      Frontend_name, Admit_db, Reject_rate,
				      Queue_pid, Backend_db, Uid, CIp,
				      LIp, LPort, Http_header, Backend_nodes,
				      New_node_list)
	    end;
	{ok, Pid} ->
	    Message = synchronise_with_erlet(Protocol_module, Pid, []),
	    Protocol_module:db_add_BE_node(Backend_db,Frontend_name,
					   CIp,LPort,Node),
	    % Report statistics
	    %Server_pid ! {report_admitted, Endpoint_name, Node},
	    make_message_packet(Uid, Message);
	Else ->
	    ?DEBUG("Erlet appears to have crashed", []),
	    make_message_packet(Uid, get_error_response("404"))
    end.


%% Wait for erlet to send us some information.
%%
synchronise_with_erlet(Protocol_module, Pid, Message_out) ->
    receive
	{Pid, {out, Encode_data}} ->
	    case catch Protocol_module:out(Encode_data) of
		{ok, Data_to_send} ->
		    synchronise_with_erlet(Protocol_module, Pid,
					   [Data_to_send|Message_out]);
		Else ->
		    ?ERROR(?F("Error in Protocol_out(~p) -> ~p~n",
			      [Encode_data, Else]))
	    end;
	{Pid, {send, Data}} ->
	    synchronise_with_erlet(Protocol_module, Pid, [Data|Message_out]);
	{Pid, {error_out, Code}} ->
	    synchronise_with_erlet(Protocol_module, Pid,
				   [get_error_response(Code)|Message_out]);
	done ->
	    Message_out;
	Else ->
	    ?ERROR(?F("Unidentified message receieved from "
		      "erlet sequence ~p~n", [Else]))
    end.


%% Spawn a process on the remote node to run the erlet.
%%
spawn_erlet_request(Node, Erlets, Http_header, Erlet_req) ->
    [#erlets{list = Erlet_seq}] = is_db:read({erlets, Erlets}),
    case net_adm:ping(Node) of
	pang ->
	    not_alive;
	pong ->
	    {ok, proc_lib:spawn_link(Node,
				     ?MODULE,
				     execute_erlets,
				     [self(),
				      Http_header,
				      Erlet_seq,
				      Erlet_req])}
    end.


%% This will run in an erlet process to carry out work for the
%% request.
%%
execute_erlets(Pid, State, [], _) ->
    Pid ! done;
execute_erlets(Pid, State, [Erlet|Rest], Erlet_req) ->
    case call_erlet(Pid, Erlet, State, Erlet_req) of
	{New_state, next} ->
	    execute_erlets(Pid, New_state, Rest, Erlet_req);
	{New_state, same} ->
	    execute_erlets(Pid, New_state, [Erlet|Rest], Erlet_req);
	{New_state, stop} ->
	    Pid ! done
    end.

%% Call the specified erlet to carry out the request. The return
%% result of the erlet will determine if the next action to take.
%%
call_erlet(Pid, Erlet, State, Erlet_req) ->
    case Erlet:do(State, Erlet_req) of
	{New_state, [], Next_action} ->
	    {State, Next_action};
	{New_state, Data, Next_action} ->
	    Pid ! {self(), Data},
	    {State, Next_action}
    end.

%% Given the HTTP header, construct an erlet request.
%%
http_to_erlet_req(Http_header) ->
    case get_value_from_pairs(method, Http_header) of
        {ok, Method} ->
            case get_value_from_pairs(uri, Http_header) of
                {ok, Uri} ->
                    case get_value_from_pairs(version, Http_header) of
                        {ok, Version} ->
                            lists:append([string_upper(Method), " ",
                                          Uri, " ", Version, "\r\n\r\n"]);
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end. 


%% Convert characters of string to upper case.
%%
string_upper([]) -> [];
string_upper([C|Rest]) when C >= $a, C =< $z ->
    [C-$a+$A|string_upper(Rest)];
string_upper([C|Rest]) ->
    [C|string_upper(Rest)].


%% Given the name and a list of pairs. Return the
%% value that matches the name.
%%
get_value_from_pairs(_, []) ->
    error;
get_value_from_pairs(Name, [{Name, Value} | _]) ->
    {ok, Value};
get_value_from_pairs(Name, [_ | Rest]) ->
    get_value_from_pairs(Name, Rest).


%% Construct a reply packet based on the supplied information.
%%
make_reply_packet(Uid, {Ip1, Ip2, Ip3, Ip4}, Port) ->
    Subpacket = [2, Uid, [Ip1, Ip2, Ip3, Ip4], short_to_list(Port)],
    [long_to_list(lists:flatlength(Subpacket)+4), Subpacket].


%% Construct a message packet to send a reply to the client in HTML.
%%
make_message_packet(Uid, Message) ->
    Subpacket = [3, Uid, Message, 0],
    [long_to_list(lists:flatlength(Subpacket)+4), Subpacket].


%% If input is a integer, convert it to a byte list.
%%
long_to_list(L) when list(L) ->
    L;
long_to_list(N) ->
    D1 = N rem 256,
    R1 = N div 256,
    D2 = R1 rem 256,
    R2 = R1 div 256,
    D3 = R2 rem 256,
    D4 = (R2 div 256) rem 256,
    [D4, D3, D2, D1].


%% If input is an integer, convert it into a byte list.
%%
short_to_list(L) when list(L) ->
    L;
short_to_list(N) ->
    [(N div 256) rem 256, D1 = N rem 256].
    


%% Convert the command list we get from the config into
%% a command string.
%%
list_to_cmd_str([]) ->
    [];
list_to_cmd_str([Item | Rest]) ->
    Item ++ " " ++ list_to_cmd_str(Rest).


%% Given a list of backend nodes specifications, find and return
%% the node that has the specified Ip and Port. Return false if
%% not found.
%%
get_node_from_ip_port([Node_spec | Rest], BIp, BPort) ->
    if
	Node_spec#backend.spec == {proxy, BIp, BPort} ->
	    Node_spec#backend.backend_node;
	true ->
	    get_node_from_ip_port(Rest, BIp, BPort)
    end;
get_node_from_ip_port([], _, _) ->
    false.



%% Take a list of strings as a command specification,
%% substitute @control_port, @ip and @port for the
%% actual parameters.
%%
cmd_substitution(["@control_port" | Rest], Control_port, Ip, Port) ->
    [integer_to_list(Control_port) |
     cmd_substitution(Rest, Control_port, Ip, Port)];
cmd_substitution(["@ip" | Rest], Control_port, Ip, Port) ->
    {Ip1, Ip2, Ip3, Ip4} = Ip,
    [lists:append([integer_to_list(Ip1), ".",
		   integer_to_list(Ip2), ".",
		   integer_to_list(Ip3), ".",
		   integer_to_list(Ip4)]) |
     cmd_substitution(Rest, Control_port, Ip, Port)];
cmd_substitution(["@port" | Rest], Control_port, Ip, Port) ->
    [integer_to_list(Port) |
     cmd_substitution(Rest, Control_port, Ip, Port)];
cmd_substitution([Param | Rest], Control_port, Ip, Endpoint) ->
    [Param | cmd_substitution(Rest, Control_port, Ip, Endpoint)];
cmd_substitution([], _, _, _) ->
    [].


%% Make an error response message, to send back to the client
%%
get_error_response(Code) ->
    case srv_table:lookup({http,status,Code}) of
	{_,_,Phrase} ->
	    ["HTTP/1.0 ", Code, " ", Phrase,
	     "\r\nDate: ",
	     srv_parse:enc_current_date(),
	     "\r\n",
	     "Server: ",
	     ?server_name,
	     "\r\nContent-type: text/plain\r\n",
	     "\r\n",
	     Code, " ", Phrase, "\r\n"];
	E ->
	    ["500 Internal Server Error",
	     "\r\nDate: ",
	     srv_parse:enc_current_date(),
	     "\r\n",
	     "Server: ",
	     ?server_name,
	     "\r\nContent-type: text/plain\r\n",
	     "\r\n",
	     "500 Internal Server Error\r\n"]
    end.
