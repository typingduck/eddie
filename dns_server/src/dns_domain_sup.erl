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
%%% File    : dns_domain_sup.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Supervisor for the DNS domain data.
%%% Created : 11 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_domain_sup).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-vsn('$Revision: /main/eddie/eddie-1.0/4').
-author('magnus@erix.ericsson.se').

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, dns_domain_sup}, dns_domain_sup, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    DNS_catalog = {dns_catalog,{dns_catalog,start_link,[]},
		   permanent,2000,worker,[dns_catalog]},
    DNS_cache = {dns_cache,{dns_cache,start_link,[]},
		 permanent,2000,worker,[dns_cache]},
    DNS_load = {dns_load,{dns_load,start_link,[]},
		  permanent,2000,worker,[dns_load]},
    DNS_zone = {dns_zone, {dns_zone, start_link, []},
		permanent,2000,worker,[dns_zone]},
    {ok,{{one_for_all,0,1}, [DNS_catalog, DNS_cache, DNS_load, DNS_zone]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------




