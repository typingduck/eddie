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

-module(test).
-author('magnus@erix.ericsson.se').

-export([load/0, up/1, down/1]).

load() ->
    {ok, Udp} = gen_udp:open(0),
    Domain = "test.du.etx.ericsson.se",
    gen_udp:send(Udp, {150,236,14,68}, 4567,
		 [0,19 + length(Domain) + 2,1,0,length(Domain)] ++ Domain ++
		 [150,236,14,67,0,3,
		  150,236,14,68,0,2,
		  150,236,14,70,0,1]),
    gen_udp:close(Udp).

up(1) -> do_up(150,236,14,67);
up(2) -> do_up(150,236,14,68);
up(3) -> do_up(150,236,14,70).

do_up(IP1,IP2,IP3,IP4) ->
    {ok, Udp} = gen_udp:open(0),
    Domain = "test.du.etx.ericsson.se",    
    gen_udp:send(Udp, {150,236,14,68}, 4567, [0,5 + length(Domain) + 2,3,0,
					      length(Domain)] ++
		                             Domain ++ [IP1,IP2,IP3,IP4]),
    gen_udp:close(Udp).

down(1) -> do_down(150,236,14,67);
down(2) -> do_down(150,236,14,68);
down(3) -> do_down(150,236,14,70).

do_down(IP1,IP2,IP3,IP4) ->
    {ok, Udp} = gen_udp:open(0),
    Domain = "test.du.etx.ericsson.se",    
    gen_udp:send(Udp, {150,236,14,68}, 4567, [0,5 + length(Domain) + 2,2,0,
					      length(Domain)] ++
					     Domain ++ [IP1,IP2,IP3,IP4]),
    gen_udp:close(Udp).


