-module(http).
-author('patrik@erix.ericsson.se').
-modified_by('jon@eddiware.com'). % restructuring and updating to HTTP/1.1
-modified_by('eric.yeo@ericsson.com.au').
%%%----------------------------------------------------------------------
%%% File    : http.erl
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
-id('$Id: http.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:25 $ ').
-state('$State: Exp $ ').

%% External exports
%% used by module is_core
-export([server/0,schedule_match/2,db_new/0,db_delete/1,
       db_get_BE_nodes/4,check_admit/7,db_remove_BE_node/5,db_add_BE_node/5]).

%%%----------------------------------------------------------------------
%%%  Main interface
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Function  : server/0
%% Arguments : 
%% Returns   : {State, JobList}
%% Purpose   : Configuration of the protocol module.
%%----------------------------------------------------------------------
server() -> [{protocol_name, "HTTP 1.0 & 1.1 - Intelligent Gateway"},
	     {endpoints, [{http_tcp, tcp, 80, 480, 20, [{packet, 0},
							binary,
							{active, false}, 
							{tcp_nodelay, true},
                                                        {backlog, 128},
							{reuseaddr, true}]},
			  {http_ssl, tcp, 443, 480, 20, [{packet, 0},
							 binary,
							 {active, false}, 
							 {tcp_nodelay, true},
							 {backlog, 128},
							 {reuseaddr, true}]}]}].

check_admit(Inet_server,Server_ref,Admit_db,
	    Frontend_name,Ip,Http_header,Cookie_name) ->
    http_admit:check_admit(Inet_server,Server_ref,Admit_db,Frontend_name,
			   Ip,Http_header,Cookie_name).

schedule_match(Parsed_request,List_of_patterns) ->
    http_parse:schedule_match(Parsed_request,List_of_patterns).

db_new() ->
    http_db:new().

db_delete(Db) ->
    http_db:delete(Db).

db_get_BE_nodes(Db,Frontend_name,Ip, Port) ->
    http_db:get_BE_nodes(Db,Frontend_name,Ip,Port).

db_add_BE_node(Db,Frontend_name,Ip,Port,BE_node) ->
    http_db:add_BE_node(Db,Frontend_name,Ip,Port,BE_node).

db_remove_BE_node(Db,Frontend_name,Ip,Port,BE_node) ->
    http_db:remove_BE_node(Db,Frontend_name,Ip,Port,BE_node).
