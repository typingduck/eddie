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
%%% File    : dummy.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 24 Jun 1998 by Joakim G. <jocke@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dummy).
-author('jocke@erix.ericsson.se').
-export([start/3,stop/3]).

-include_lib("misc/include/logger.hrl").

%% start

start(IPAddress,Port,FrontEndCluster) ->
  case pget({node(),IPAddress,Port,FrontEndCluster}) of
    defined ->
      {error,already_started};
    undefined ->
      pput({node(),IPAddress,Port,FrontEndCluster},defined),
      ok
  end.

%% stop

stop(IPAddress,Port,FrontEndCluster) ->
  case pget({node(),IPAddress,Port,FrontEndCluster}) of
    defined ->
      pput({node(),IPAddress,Port,FrontEndCluster},undefined),
      ok;
    undefined ->
      {error,already_stopped}
  end.

pput(Key,Value) ->
  Filename="/tmp/dummy_dets."++atom_to_list(node()),
  {ok,Name}=dets:open_file(dummy_dets,[{file,Filename}]),
  dets:insert(Name,{Key,Value}).

pget(Key) ->
  Filename="/tmp/dummy_dets."++atom_to_list(node()),
  {ok,Name}=dets:open_file(dummy_dets,[{file,Filename}]),
  case dets:lookup(Name,Key) of
    [] ->
      dets:close(Name),
      undefined;
    [{Key,Value}] ->
      dets:close(Name),
      Value
  end.
