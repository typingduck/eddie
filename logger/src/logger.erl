-module(syslog).
-author('geoff@eddieware.org').
%%%----------------------------------------------------------------------
%%% File    : syslog.erl
%%% Author  : geoff@eddiware.org
%%% Created : Feb 2000
%%% Purpose : Register gen event handler with system error logger
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
%%% The Initial Developer of the Original Code is Ericsson Telecom
%%% AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
%%% Telecom AB. All Rights Reserved.
%%%----------------------------------------------------------------------
-id('$Id: logger.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/logger/src/logger.erl,v $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:26 $ ').
-state('$State: Exp $ ').


%% Start

start_link() ->
    sys_logger:add_handler().


%% Stop

stop(State) ->
    % perhaps should remove the report handler?
    ok.

