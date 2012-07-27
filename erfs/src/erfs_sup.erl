%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.0, (the "License"); you may not use this file except in
%%% compliance with the License. You may obtain a copy of the License at
%%% http://www.eddieware.org/EPL
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% The Original Code is Erfs-0.2b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (c), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : erfs_sup.erl
%%% Author  : Anders Dahlin <anders@eddieware.org>
%%% Purpose : Eddie Replicated File System supervisor
%%% Created : 14 Oct 1998 by Anders Dahlin <anders@eddieware.org>
%%%----------------------------------------------------------------------

-module(erfs_sup).

-author('anders@eddieware.org').
-copyright('Copyright (c) 1998 Ericsson Telecom AB').

-vsn("0.2b1").


-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(NAME, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(StartArgs) ->
    supervisor:start_link({local, ?NAME}, ?MODULE, StartArgs).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init(_StartArgs) ->
    Erfs = {erfs, {erfs, start_link, []}, permanent, 10000, worker, 
	    [erfs]},
    {ok, {{one_for_all, 0, 1}, [Erfs]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
