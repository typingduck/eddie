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

-module(is_sup).
-vsn('$Revision: /main/eddie/eddie-1.0/13').
-author('patrik@erix.ericsson.se').
-export([start_link/0, init/1]).
%% start
start_link() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 3600},
	  [{inet_server,
	    {srv_table, start_link, []}, permanent, 2000, worker, [srv_table]}]}}.

