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
%%% File    : misc_app.erl
%%% Author  : Magnus Fröberg <magnus@erix.ericsson.se>
%%% Created : 22 Oct 1998 by Magnus Fröberg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(misc_app).
-author('magnus@erix.ericsson.se').
-behaviour(application).
-export([start/2,stop/1]).

%% start

start(Type,StartArgs) ->
  misc_sup:start_link().

%% stop

stop(State) ->
  ok.
