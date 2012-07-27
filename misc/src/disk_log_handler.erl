-module(disk_log_handler).
-author('jocke@erix.ericsson.se').

%%%----------------------------------------------------------------------
%%% File    : disk_log_handler.erl
%%% Author  : Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Created : 23 Jun 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Modified: 27 Apr 1999 by tobbe@eddieware.org
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
%%% Contributor(s): Feb 2000 - geoff@eddieware.org
%%%
%%%----------------------------------------------------------------------
-vc('$Id: disk_log_handler.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-export([init/1,handle_event/2,handle_call/2,handle_info/2,
	 terminate/2,reopen/0]).

-behaviour(gen_event).

-include("logger.hrl").

-define(MAX_BYTES,512000).
-define(MAX_FILES,5).

-record(state, {
        verbose = false,
        port,
		name 
        }).

reopen() ->
    gen_event:notify(error_logger, reopen_disk_log).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

log_name() -> ?MODULE.

%% init

init([FileName,Verbose]) ->
    Name = log_name(),
%    case disk_log:open([{distributed,[node()]},
%			{file,FileName},
%			{format,external},
%			{name,Name},
%			{type,halt}]) of
%	{error,Reason} ->
%	    {error,Reason};
%	_ ->
%	    {Date, Time} = erlang:localtime(),
%	    InitS = io_lib:format("~n--------------------------------------------~n"
%				  "Logging started: ~p - ~p~n"
%				  "--------------------------------------------~n~n",
%				  [Date,Time]),
%	    disk_log:balog(Name,InitS),
%	    {ok,#state{verbose=Verbose, name=FileName}}
%    end.
    PrivDir = code:priv_dir(misc),
    case start_syslog(PrivDir) of
      {ok, DPort} ->
	    {Date, Time} = erlang:localtime(),
	    InitS = io_lib:format("Eddie Started: ~p - ~p~n", [Date,Time]),
        %?INFO(InitS),
        error_logger:info_report(InitS),
        {ok,#state{verbose=Verbose, port=DPort, name=FileName}};
      Error ->
        {stop, Error}
    end
    .

%%
%% Name: start_syslog
%% Purpose: link is syslog C driver
%%

start_syslog(PrivDir) ->
    case erl_ddll:load_driver(PrivDir, "syslog_drv") of
    ok ->
        DPort = open_port({spawn, "syslog_drv"}, []),
        {ok, DPort};
    Error ->
        ?FATAL(?F("Couldn't link in driver ~s/syslog_drv - ~p",
              [PrivDir, Error])),
        Error
    end.

%% handle_event

handle_event({error_report,_,{_,fatal,{_M,_L,Dist,Reason}}},State) ->
    write_report(fatal,Dist,Reason,State),
    distribute(fatal,Reason,Dist),
    {ok,State};
handle_event({error_report,_,{_,error,{_M,_L,Dist,Reason}}},State) ->
    write_report(error,Dist,Reason,State),
    distribute(error,Reason,Dist),
    {ok,State};

% new
handle_event({error_report,_,{_,Type,{_M,_L,Dist,Reason}}},State) ->
    write_report(error,Dist,Reason,State),
    distribute(error,Reason,Dist),
    {ok,State};

handle_event({info_report,_,{_,info,{_M,_L,Dist,Reason}}},State) ->
    write_report(info,Dist,Reason,State),
    distribute(info,Reason,Dist),
    {ok,State};

% new
handle_event({info_report,_,{_,Type,{_M,_L,Dist,Reason}}},State) ->
    write_report(info,Dist,Reason,State),
    distribute(info,Reason,Dist),
    {ok,State};

% new
handle_event({error,_,{_,Type,{_M,_L,Dist,Reason}}},State) ->
    write_report(error,Dist,Reason,State),
    distribute(error,Reason,Dist),
    {ok,State};

% new
handle_event({info,_,{_,Type,{_M,_L,Dist,Reason}}},State) ->
    write_report(info,Dist,Reason,State),
    distribute(info,Reason,Dist),
    {ok,State};

handle_event(reopen_disk_log,State) ->
    %disk_log:reopen(log_name(),lists:concat([State#state.name,".old"])),
    {ok,State};
handle_event(Event,State) ->
    {ok,State}.

%% handle_call

handle_call({dist,Type,Reason,Node},State) ->
    write_report(Type,Node,Reason,State),
    {ok,ok,State};
handle_call(_,State) ->
    {ok,{error,bad_request},State}.

%% handle_info

handle_info(Info,State) ->
    {ok,State}.

%% terminate

terminate(Reason,State) ->
    %disk_log:close(log_name()),
    ok.

%% Distribute a report to all other known nodes.
distribute(_,_,false) ->
    ok;
distribute(Type,Reason,_) ->
    Q = {dist,Type,Reason,node()},
    F = fun(Node) ->
		rpc:cast(Node,gen_event,call,[error_logger,log_name(),Q])
	end,
    lists:foreach(F, nodes()).

%% write a report with a time stamp and type indication.
write_report(Type,Node,Reason,State) ->
    TT = type(Type, Node),
    T = wtime(erlang:localtime(),TT),
    String = ?F("~s~n",[Reason]),
    %disk_log:balog(log_name(),String),
    %?ERROR(String),
    %error_logger:error_report(String),
    [ H | _ ] = TT,
    port_command(State#state.port, [ H, String, 0 ]),
    verbose(State,String),
    ok.

type(Type,false) -> type(Type);
type(Type,true)  -> type(Type, node());
type(Type,Node)  -> "GLOBAL " ++ type(Type) ++ " (" ++
			atom_to_list(Node) ++ ")".

type(fatal) -> "FATAL ERROR";
type(error) -> "ERROR";
type(info)  -> "INFO".

wtime({{Y,Mo,D},{H,Mi,S}},Type) ->
    io_lib:format("=~s==== ~p-~s-~p::~s:~s:~s ===",
		  [Type,D,m(Mo),Y,t(H),t(Mi),t(S)]).

t(X) when integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".
t1([X]) -> [$0,X];
t1(X)   -> X.

m(1) -> "Jan";
m(2) -> "Feb";
m(3) -> "Mar";
m(4) -> "Apr";
m(5) -> "May";
m(6) -> "Jun";
m(7) -> "Jul";
m(8) -> "Aug";
m(9) -> "Sep";
m(10) -> "Oct";
m(11) -> "Nov";
m(12) -> "Dec".

verbose(S,String) when S#state.verbose == true -> io:format(String);
verbose(_,_)                                   -> ok.

