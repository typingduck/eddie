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
%%% File    : make.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : 
%%% Created : 21 Aug 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(make).
-author('magnus@erix.ericsson.se').

-export([script/1, tar/1]).

script([RelFile, Emulator, Prefix]) ->
    F = atom_to_list(RelFile),
    Pre = atom_to_list(Prefix),
    case catch systools:make_script(F, [{machine, Emulator},
				        {variables, [{"DNS_ROOT",Pre}]}]) of
	ok ->
	    halt();
	{'EXIT', Error} ->
	    io:format("Error = ~p~n", [Error]),
	    halt(1);
	_ ->
	    halt(1)
    end.

%% Module tests are supposed to have been made in make_script !
tar([RelFile, Emulator | ErtsDir]) ->
    F = atom_to_list(RelFile),
    Erts = include_erts(ErtsDir),
    case catch systools:make_tar(F, [{machine, Emulator},
				     no_module_tests |
				     Erts]) of
	ok ->
	    halt();
	{'EXIT', Error} ->
	    io:format("Error = ~p~n", [Error]),
	    halt(1);
	_ ->
	    halt(1)
    end.

include_erts([])      -> [];
include_erts(ErtsDir) -> [{erts, atom_to_list(ErtsDir)}].

