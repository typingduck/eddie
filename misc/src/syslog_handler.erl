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
%%% File    : syslog_handler.erl
%%% Author  : Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Created : 23 Jun 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

-module(syslog_handler).
-author('jocke@erix.ericsson.se').
-behaviour(gen_event).
-export([init/1,handle_event/2,handle_call/2,handle_info/2,terminate/2]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%% init

init([]) ->
  {ok,[]}.

%% handle_event

handle_event({error_report,Gleader,{EPid,fatal,{Module,Line,Reason}}},State) ->
  os:cmd(io_lib:format("logger -i -t eddie \"fatal: ~s\"",[one_line(Reason)])),
  {ok,State};
handle_event({error_report,Gleader,{EPid,error,{Module,Line,Reason}}},State) ->
  os:cmd(io_lib:format("logger -i -t eddie \"error: ~s\"",[one_line(Reason)])),
  {ok,State};
handle_event({info_report,Gleader,{EPid,info,{Module,Line,Reason}}},State) ->
  os:cmd(io_lib:format("logger -i -t eddie \"info: ~s\"",[one_line(Reason)])),
  {ok,State};
handle_event(Event,State) ->
  {ok,State}.

one_line([]) ->
  [];
one_line([$ ,$ |Rest]) ->
  one_line(Rest);
one_line([$\n|Rest]) ->
  one_line([$ |Rest]);
one_line([$\t|Rest]) ->
  one_line([$ |Rest]);
one_line([Char|Rest]) ->
  [Char|one_line(Rest)].

%% handle_call

handle_call(Request,State) ->
  {ok,ok,State}.

%% handle_info

handle_info(Info,State) ->
  {ok,State}.

%% terminate

terminate(Reason,State) ->
  ok.
