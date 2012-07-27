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
%%% The Original Code is Eddie-0.83b1.
%%%
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : master_app.erl
%%% Author  : Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Created : 24 Jun 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

-module(master_app).
-author('jocke@erix.ericsson.se').
-vc('$Id: master_app.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-behaviour(application).
-export([start/2,stop/1]).

%% start

start(Type,StartArgs) ->
  master_sup:start_link().

%% stop

stop(State) ->
  ok.
