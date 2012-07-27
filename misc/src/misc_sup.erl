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
%%% File    : misc_sup.erl
%%% Author  : Magnus Fröberg <magnus@erix.ericsson.se>
%%% Created : 22 Oct 1998 by Magnus Fröberg <magnus@erix.ericsson.se>
%%% Apr 99 - geoff@eddieware.org - startup log signal handler
%%%----------------------------------------------------------------------

-module(misc_sup).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-vc('$Id: misc_sup.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    Log_sig = {log_sig, {log_sig, start_link, []},
               permanent, 2000, worker, [dns_sig]},
    case os:type() of
    {win32, nt} ->
        {ok, {{one_for_all,0,1}, [Log_sig]}};
    _ ->
        Fdsrv = {fdsrv, {fdsrv, start_link, []},
                 permanent, 2000, worker, [fdsrv]},
        {ok,{{one_for_all,0,1}, [Fdsrv, Log_sig]}}
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
