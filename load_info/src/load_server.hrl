-ifndef(_LOAD_SERVER_HRL).
-define(_LOAD_SERVER_HRL, true).
%%%----------------------------------------------------------------------
%%% File    : load_server.hrl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : Contains the definition used be load_server.erl
%%% Created : 15 Sep 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%
%%% Apr 99 - geoff@eddieware.org - Removed unnecessaries macroes (do more!)
%%% Modified: 28 Apr 1999 by tobbe@eddieware.org
%%%
%%%----------------------------------------------------------------------
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

%% an id string to distinguish us from other
%% incompatible versions and othter dns packets etc..
-define(HSTRING, "LB 1.0"). 

-define(OUR_DNS_HEADER,
	[
	 0,0,         %% ID
	 13 bsl 3, 0, %% QR =0, our opcode 13 in pos 1-4, AA,TC,RD,RA=0, Z=0, RCODE=0
	 0,0,         %% QDCount, 
	 0,0,         %% ANCount
	 0,0,         %% NSCount
	 0,0         %% ARCount
	]).

-define(OUR_DNS_HEADER_LEN, length(lists:flatten(?OUR_DNS_HEADER))).
%-define(HLENGTH, length(?OUR_DNS_HEADER)).

%%-define(FE, frontend).
%%-define(BE, backend).
-define(FE, "fe").
-define(BE, "be").
-define(FEBE_LEN, 2).

-define(DNS_TIMEOUT, 5000).
-define(LOAD_TIMEOUT, 5000).
-define(RETRY_GLOBAL_TIMEOUT, 500).

-define(DNS_LOAD_PORT, 4567). % default

-define(LOADINFO, 1).
-define(IP_DOWN, 2).
-define(IP_UP, 3).
-define(OP_CODE_LEN, 1).

%% One load record per IP address.
-record(load, {node = node(),
	       class,         % ?FE | ?BE
	       ip,
	       status,
	       type,
	       load,
	       host = hd(tl(string:tokens(atom_to_list(node()), [$@]))),
	       nodes = [],
	       'R',           % The threshold valuea
	       f = 1}).       % MIN_FRACTION =< f <= 1

%% #load.type
-define(STATIC, 0).
-define(FAILOVER, 1).

%% Use record to match #load records from ets table.
-define(MATCH(Node, Class, IP), #load{node = Node,
				      class = Class,
				      ip = IP,
				      status = '_',
				      type = '_',
				      load = '_',
				      host = '_',
				      nodes = '_',
				      'R' = '_',
				      f = '_'}).

-define(MIN_FRACTION_L, 0.01).
-define(PHI, 0.03).
-define(PHI1, 0.03).
-define(FE_FRACTION, 1).
-define(ALPHA, 0.5).

%% Used to store the current node fraction list in ets.
-record(nodes, {key = nodes,
		fractions = []}).

%% Backend admission control values.
-record(be_ac, {'G' = 0,    % Geometrically decaying average load.
		'R' = 0,    % Average of max desired utilization.
		'SR' = 0,   % The sum of desired utilization.
		'B' = 0}).  % The fraction of rejected new session attempts.

%% Don't let G be too small.
-define(MIN_G(G),
	if (G) >= 0.001 -> (G);
	    true       -> 0
	end).
-endif.

