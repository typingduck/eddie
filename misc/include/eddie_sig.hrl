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
%%% File    : eddie_sig.hrl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : UNIX signal handler.
%%%           Handles SIGINT, SIGHUP, SIGABRT, SIGUSR1, SIGUSR2 signals.
%%% Created :  8 May 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-define(ERL_SIGHUP,  1).
-define(ERL_SIGINT,  2).
-define(ERL_SIGABRT, 3).
-define(ERL_SIGUSR1, 4).
-define(ERL_SIGUSR2, 5).

