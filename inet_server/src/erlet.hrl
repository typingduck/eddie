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

-define(next_erlet, 'next_erlet').
-define(same_erlet, 'same_erlet').
-define(do_pre, 'do_pre').
-define(do_data, 'do_data').
-define(do_post, 'do_post').
-define(do_end, 'end').

-record(thread_data,               %%% This record is sent as an argument to the in function and the erlets.
	{peer_ip,                  %%% Ip address of the peer.
	 peer_port,                %%% Port of the peer.
	 endpoint_name,            %%% The name of the endpoint.
	 threads}).                %%% Number of threads spawn from this endpoint.

