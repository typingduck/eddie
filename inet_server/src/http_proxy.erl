-module(http_proxy).
-author('patrik@erix.ericsson.se').
-modified_by('jon@eddiware.com'). % restructuring and updating to HTTP/1.1
%%%----------------------------------------------------------------------
%%% File    : http_proxy.erl
%%% Author  : Patrik Winroth <patrik@erix.ericsson.se>
%%% Created : 06 Oct 1998
%%% Purpose : Protocol Module for implementing an intelligent
%%%           Gateway for HTTP/1.0 and HTTP/1.1.
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
-id('$Id: http_proxy.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/inet_server/src/http_proxy.erl,v $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:25 $ ').
-state('$State: Exp $ ').

%% External exports
%% used by module is_core (through module http)
-export([proxy/7]).

-include("logger.hrl").

%% Time to wait before time-out when proxying.
-define(PROXY_CONNECT_TIMEOUT, 60000).
%% TAG_DATA used to check if all answers has been received from server backend,
%% used when syncronizing HTTP/1.1 requests.
-define(TAG_DATA, "TRACE\r\n\r\n").
-define(TAG_DATA_SIZE, 9).

-define(server_name,"Eddie - Intelligent Gateway/1.1").

-define(stream_size,8192).

%%----------------------------------------------------------------------
%% Function  : proxy/7
%% Arguments : 
%%             
%% Returns   : State | {State,Action}
%%
%%             State: [] | {Ip, Port, Socket}
%%             Action: Action is used when the decision if the connection
%%                     to the client is to be kept, has to be taken in
%%                     this module. This is the case when a request has
%%                     been split over several packages and this module
%%                     reads the rest of the request.
%% Purpose   : Communicate with a HTTP 1.1 server (acting back-proxy).
%%             Assumes that TRACE without arguments is echo'ed (as is
%%             the case with Apache 1.3.4).
%%             If no socket is opened, open a socket. Pipeline requests
%%             that can be scheduled to the same backend. If a request
%%             that can not be pipelined is detected, wait for the
%%             answers from the requests that has been done. Then close
%%             the backend socket. If anything at all goes wrong,
%%             abandon & clean up.
%%----------------------------------------------------------------------
%% Send error message (request error) back to client
proxy(_,_,_,{tcp, Client_socket},_,error, Code) ->
    gen_tcp:send(Client_socket,
		 get_error_response(Code));
%% Use this one to syncronize (wait for) answers
proxy(_, _, State, {tcp, Client_socket}, Read_timeout, _, []) ->
    case get_open_proxy(State) of
	{_,{Ip_server, Port_server, Server_socket}} ->
	    %% Wait for all data to return from Old_socket
	    %% before continuing.
	    syncronize_http(State, Client_socket, Server_socket);
	_ ->
	    ok
    end;
proxy(Server_ip, Server_port, State,
      {tcp, Client_socket}, Read_timeout, Parsed_headers, Data) ->

%%    ?DEBUG("<proxy> Parsed headers: ~p", [Parsed_headers]),

    case get_open_proxy(State) of
	{_, {Server_ip, Server_port, Server_socket}} ->
	    %% Pipeline.
	    case send_all_data(Data,Parsed_headers,Server_socket,Client_socket,
			       Read_timeout) of
		{ok,Action} -> {State,Action};
		ok -> State;
		{error,Reason} -> % Backend server not ok
		    gen_tcp:close(Server_socket),
		    {error,Reason};
		error -> % Unrecoverable error
		    gen_tcp:close(Server_socket),
		    gen_tcp:close(Client_socket),
		    error
	    end;
	{_, {_, {Server_ip_old, Server_port_old, Server_socket_old}}} ->
	    %% Send tag data to backend, and wait for all data to come back,
	    %% then close the connection.
	    %% Wait for all data to return from Old_socket,
	    %% before continuing.
	    syncronize_http(State, Client_socket, Server_socket_old),
	    case connect_server(Server_ip, Server_port) of
		{ok, Server_socket_new} ->
		    case send_all_data(Data,Parsed_headers,Server_socket_new,
				       Client_socket,Read_timeout) of
			{ok,Action} ->
			    New_state = add_open_proxy(new(),
						       Server_ip,
						       Server_port,
						       Server_socket_new),
			    New_state2 = add_version(New_state,Parsed_headers),
			    {New_state2,Action};
			ok ->
			    New_state = add_open_proxy(new(),
						       Server_ip,
						       Server_port,
						       Server_socket_new),
			    add_version(New_state,Parsed_headers);
			{error,Reason} -> % Backend server is not ok, close and return error
			    gen_tcp:close(Server_socket_new),
			    {error,Reason};
			error -> % Unrecoverable error, close both connections
			    gen_tcp:close(Server_socket_new),
			    gen_tcp:close(Client_socket),
			    error
		    end;
		{error,Reason} -> % Could not open connection to server
		    {error,Reason};
		false -> % Out of file descriptors
		    []
	    end;
	_ ->
	    %% No socket to pipeline, or await answers on; open a new.
	    case connect_server(Server_ip, Server_port) of
		{ok, Server_socket_new} ->
		    case send_all_data(Data,Parsed_headers,Server_socket_new,
				       Client_socket,Read_timeout) of
			{ok,Action} ->
			    New_state = add_open_proxy(new(),
						       Server_ip,
						       Server_port,
						       Server_socket_new),
			    New_state2 = add_version(New_state,Parsed_headers),
			    {New_state2,Action};			
			ok ->
			    New_state = add_open_proxy(new(),
						       Server_ip,
						       Server_port,
						       Server_socket_new),
			    add_version(New_state,Parsed_headers);
			{error,Reason} -> % Backend server is not ok, close and return error
			    gen_tcp:close(Server_socket_new),
			    {error,Reason};
			error -> % Unrecoverable error, close both connections
			    gen_tcp:close(Server_socket_new),
			    gen_tcp:close(Client_socket),
			    error
		    end;
		{error,Reason} -> % Could not open connection to server
		    {error,Reason};
		false -> % Out of file descriptors
		    []
	    end
    end.

send_all_data({partial_request,Req_so_far,Partialinfo},Parsed_headers,Server_socket,
	      Client_socket,Read_timeout) ->
    case send_data(Server_socket,Req_so_far) of
	ok ->
	    keep_reading_body(Partialinfo,Parsed_headers,Server_socket,Client_socket,
			      Read_timeout);
	{error,Reason} -> % This backend server not ok
	    {error,Reason}
    end;
send_all_data(Data,Parsed_headers,Server_socket,Client_socket,Read_timeout) ->
    case send_data(Server_socket,Data) of
	ok             -> ok;
	{error,Reason} -> {error,Reason} % This backend server not ok
    end.

send_data(Server_socket,Data) ->
    gen_tcp:send(Server_socket,Data).

keep_reading_body({read_body,Length},Parsed_headers,Server_socket,
		  Client_socket,Read_timeout) when Length>?stream_size ->
    case gen_tcp:recv(Client_socket,?stream_size,Read_timeout) of
	{ok,Packet} ->
	    case send_data(Server_socket,Packet) of
		ok -> 
		    keep_reading_body({read_body,Length-?stream_size},
				      Parsed_headers,
				      Server_socket,
				      Client_socket,
				      Read_timeout);
		{error,Reason} -> error % Backend server not ok, close client too
	    end;
	Reason ->
	    ?INFO(?F("Did not receive the whole request from client, reason: ~p~n",[Reason])),
	    error
    end;
keep_reading_body({read_body,Length},Parsed_headers,Server_socket,
		  Client_socket,Read_timeout) ->
    case gen_tcp:recv(Client_socket,Length,Read_timeout) of
	{ok,Packet} ->
	    case send_data(Server_socket,Packet) of
		ok    -> return(Parsed_headers,[]);
		{error,Reason} -> error % Backend server not ok, close client too
	    end;
	Reason ->
	    ?INFO(?F("Did not receive the whole request from client, reason: ~p~n",[Reason])),
	    error
    end;
keep_reading_body({read_chunked_body,Last_chunk},Parsed_headers,Server_socket,
		  Client_socket,Read_timeout) ->
    read_chunked_body({read_chunked_body,Last_chunk},Parsed_headers,
		      Server_socket,Client_socket,Read_timeout);
keep_reading_body({read_chunked_footer},Parsed_headers,Server_socket,
		  Client_socket,Read_timeout) ->
    read_chunked_body({read_chunked_footer},Parsed_headers,Server_socket,
		      Client_socket,Read_timeout);
keep_reading_body({read_chunked_footer,one_newline},Parsed_headers,
		  Server_socket,Client_socket,Read_timeout) ->
    read_chunked_body({read_chunked_footer,one_newline},Parsed_headers,
		      Server_socket,Client_socket,Read_timeout).

read_chunked_body(Partial_info,Parsed_headers,Server_socket,Client_socket,
		  Read_timeout) ->
    case gen_tcp:recv(Client_socket,0,Read_timeout) of
	{ok,Packet} ->
	    Result = http_parse:read_chunked_body(Partial_info,
						  binary_to_list(Packet)),
	    analyze(Result,Server_socket,Client_socket,
		    Read_timeout,Parsed_headers);
	Reason ->
	    ?INFO(?F("Did not receive the whole request from client, reason: ~p~n",[Reason])),
	    error
    end.

%% Check the result returned from parsing the new packet
analyze({ok,Data,Rest},Server_socket,_,_,Parsed_headers) ->
    case send_data(Server_socket,Data) of
	ok             -> return(Parsed_headers,Rest);
	{error,Reason} -> error % Backend server not ok, close client too
    end;
analyze({partial,Data,Info},Server_socket,Client_socket,
	Read_timeout,Parsed_headers) ->
    case send_data(Server_socket,Data) of
	ok             -> keep_reading_body(Info,Parsed_headers,Server_socket,
					    Client_socket,Read_timeout);
	{error,Reason} -> error % Backend server not ok, close client too
    end;
analyze(error,_,_,_,_) ->
    error.

%% check the headers of last received request to see if the connection is to be
%% kept alive or not.
%% http_parse:keep_alive_p returns:  close | keep_alive
return(Parsed_headers,Rest) ->
    case http_parse:keep_alive_p(Parsed_headers) of
	close      -> {ok,close};
	keep_alive -> return2(keep_alive,http_parse:remove_newlines(Rest))
    end.

return2(keep_alive,[])   -> {ok,{read_bytes,0,keep_alive}};
return2(keep_alive,Rest) -> {ok,{process,Rest}}.
    

syncronize_http(State, Client_socket, Server_socket) ->

%%    ?DEBUG("State: ~p", [State]),

    case get_version(State) of
	{_,"HTTP/1.1"} ->
	    case gen_tcp:send(Server_socket, ?TAG_DATA) of
		ok ->
		    %% Wait for all data to return from Old_socket
		    %% before continuing.
		    syncronize_http2(Client_socket, Server_socket);
		{error, Reason} ->
		    ?INFO(?F("Couldnt send tag data to server, "
			     "reason: ~p", [Reason]))
	    end,
	    gen_tcp:close(Server_socket);
	Else ->
	    syncronize_pre_http11(Client_socket, Server_socket)
    end.

syncronize_pre_http11(Client_socket, Server_socket) ->
%%    ?DEBUG("sync pre http11", []),
    case gen_tcp:recv(Server_socket, 0, 10000) of
	{ok, Packet} ->
	    case gen_tcp:send(Client_socket, Packet) of
		ok ->
		    syncronize_pre_http11(Client_socket, Server_socket);
		{error, Reason} ->
		    ?INFO(?F("Couldnt send data back to client, "
			     "reason: ~p", [Reason]))
	    end;
	{error, Reason} ->
	    gen_tcp:close(Client_socket),
	    gen_tcp:close(Server_socket)
    end.

syncronize_http2(Client_socket, Server_socket) ->
    case gen_tcp:recv(Server_socket, 0) of
	{ok, Packet} ->
%%	    ?DEBUG("Got data: ~s", [binary_to_list(Packet)]),
	    Size = size(Packet),
	    case Size >= ?TAG_DATA_SIZE of
		true ->
		    {P_first, P_tag} =
			split_binary(Packet, Size - (?TAG_DATA_SIZE)),
		    case binary_to_list(P_tag) == ?TAG_DATA of
			true ->
			    %% Ok, found tag - send data to client,
			    %% and close socket to server backend.
			    case gen_tcp:send(Client_socket, P_first) of
				ok ->
				    ok;
				{error, Reason} ->
				    ?INFO(?F("Couldnt send data back to client, "
						  "reason ~p", [Reason])) 
			    end,
			    gen_tcp:close(Server_socket);
			_ ->
			    case gen_tcp:send(Client_socket, Packet) of
				ok ->
				    syncronize_http2(Client_socket, Server_socket);
				{error, Reason} ->
				    ?INFO(?F("Couldnt send data back to client, "
						  "reason: ~p", [Reason]))
			    end
		    end;
		_ ->
		    case gen_tcp:send(Client_socket, Packet) of
			ok ->
			    syncronize_http2(Client_socket, Server_socket);
			{error, Reason} ->
			    ?INFO(?F("Couldnt send data back to client, "
					  "reason: ~p", [Reason]))
		    end
	    end;
	{error, Reason} ->
	    ok
    end.

connect_server(Proxy_ip, Proxy_port) ->
    case gen_tcp:connect(Proxy_ip,
			 Proxy_port, 
			 [binary, 
			  {packet, raw},
			  {reuseaddr, true},
			  {nodelay, false},
			  {active, false}], 
			 ?PROXY_CONNECT_TIMEOUT) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, {enfile, _}} ->
	    ?INFO("Run out of file descriptors when trying to connect to server backend"),
	    false;
	{error, Reason} ->
	    ?ERROR(?F("This proxy backend not ok <ip:~p port:~p>, "
			  "reason: ~p", [Proxy_ip, Proxy_port, Reason])),
	    {error,Reason}
    end.

%% Make an error response message, to send back to the client
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
	_ ->
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


new() ->
    [].

add_open_proxy(State,Server_ip,Server_port,Server_socket) ->
    add_to_state(open_proxy,{Server_ip,Server_port,Server_socket},State).

add_version(State,Parsed_headers) ->
    Value = http_fields:get_header_value(version,Parsed_headers),
    add_to_state(version,Value,State).

get_open_proxy(State) -> get_from_state(open_proxy,State).

get_version(State) -> get_from_state(version,State).

add_to_state(Type,Value,State) ->
    [{Type,Value}|State].

get_from_state(Type,State) ->
    case lists:keysearch(Type,1,State) of
	{value,Info} -> Info;
	_            -> []
    end.

