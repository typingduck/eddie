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
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : dns_sup.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Supervisor for the DNS server.
%%% Created :  4 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Apr 99 - pekka@eddieware.org - fixed worker startups
%%%----------------------------------------------------------------------

-module(dns_sup).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('magnus@erix.ericsson.se').

-behaviour(supervisor).
-include_lib("../../misc/include/logger.hrl").

%% External exports
-export([start_link/0, start/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, dns_sup}, dns_sup, []).

%% Experimental !!
start() ->
    {ok, Pid} = supervisor:start_link({local, dns_sup}, dns_sup, []),
    unlink(Pid),
    {ok, Pid}.

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init([]) ->
    %% Fix up logging.
    LogFile = dns_catalog:logfile(),
    Verbose = dns_catalog:verbose(),
    error_logger:add_report_handler(disk_log_handler, [LogFile, Verbose]),
    ?INFO("LBDNS supervisor started"),
    DNS_sig = {dns_sig, {dns_sig, start_link, []},
	       permanent, 2000, worker, [dns_sig]},
    DNS_domain = {dns_domain_sup, {dns_domain_sup, start_link, []},
		  permanent, 2000, supervisor, [dns_domain_sup]},
    DNS_UDP_tracker = {dns_recurse_udp_tracker,
		       {dns_recurse_udp_tracker, start_link, []},
		       permanent, 2000, worker, [dns_recurse_udp_tracker]},
    DNS_server = {dns_server,{dns_server,start_link,[]},
		  permanent,2000,worker,[dns_server]},
    {ok,{{one_for_all,0,1}, [DNS_sig,
			     DNS_domain,
			     DNS_UDP_tracker,
			     DNS_server]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
