-module(eddie_logger).
-author('tobbe@eddieware.com').
%%%----------------------------------------------------------------------
%%% File    : eddie_logger.erl
%%% Author  : tobbe@eddiware.com
%%% Created : 16 Tue 1999
%%% Purpose : General log framework.
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
%%% AB. Portions created by Ericsson are Copyright (C), 1998,1999 Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): Feb-2000 geoff@eddieware.org
%%%----------------------------------------------------------------------
-id('$Id: sys_logger.erl,v 1.1 2000/10/27 22:20:26 dredd Exp $ ').
-source('$Source: /cvsroot/eddie/eddie/logger/src/sys_logger.erl,v $ ').
-revision('$Revision: 1.1 $ ').
-date('$Date: 2000/10/27 22:20:26 $ ').
-state('$State: Exp $ ').

-behaviour(gen_event).

%% External exports
-export([add_handler/0,add_handler/1,add_handler/2,error/1,error/2,
	 info/1,info/2,log/3,log/5]).
-export([emergency/0,alert/0,critical/0,error/0,warning/0,
	 notice/0,info/0,debug/0]).
-export([kern/0,user/0,mail/0,daemon/0,auth/0,syslog/0,lpr/0,
	 news/0,uucp/0,cron/0,authpriv/0,ftp/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

-define(WHO, eddie).       % Under what name syslog should log the info.

-record(state,
	{sock,             % UDP socket 
	 host,             % Host running syslogd
	 port=514          % The syslog UDP port number
	}).

%% --------------------
%% Exported interface. 

%% Add the report handler. As default, the syslog
%% facility will be used on the local host.

add_handler()          -> add_handler(syslog).
add_handler(Type)      -> error_logger:add_report_handler(?MODULE,[Type]).
add_handler(Type,Host) -> error_logger:add_report_handler(?MODULE,[Type,Host]).

%% A couple of examples:
%%  error("no response from front-end~n").
%%  info("~w: dns server ~w started~n", [time(), DnsServer]).
%%  log(emergency(), "the network card is on fire~n", []).
%%  log(eddie_ftp, ftp(), notice(), "replacing std-ftp~n", []).

error(Msg)      -> log(error(),Msg,[]).
error(Msg,Args) -> log(error(),Msg,Args).

info(Msg)      -> log(info(),Msg,[]).
info(Msg,Args) -> log(info(),Msg,Args).

log(Level,Msg,Args) when integer(Level),list(Msg),list(Args) ->
    error_logger:error_report({{?WHO,Level},{Msg,Args}}).

log(Who,Facility,Level,Msg,Args) when atom(Who),integer(Facility),
				      integer(Level),list(Msg),list(Args) ->
    error_logger:error_report({{Who,Facility,Level},{Msg,Args}}).

%% Convenient routines for specifying levels.

emergency() -> 0. % system is unusable 
alert()     -> 1. % action must be taken immediately 
critical()  -> 2. % critical conditions 
error()     -> 3. % error conditions 
warning()   -> 4. % warning conditions 
notice()    -> 5. % normal but significant condition 
info()      -> 6. % informational
debug()     -> 7. % debug-level messages 

%% Convenient routines for specifying facility codes

kern()     -> (0 bsl 3) . % kernel messages 
user()     -> (1 bsl 3) . % random user-level messages 
mail()     -> (2 bsl 3) . % mail system 
daemon()   -> (3 bsl 3) . % system daemons 
auth()     -> (4 bsl 3) . % security/authorization messages 
syslog()   -> (5 bsl 3) . % messages generated internally by syslogd 
lpr()      -> (6 bsl 3) . % line printer subsystem 
news()     -> (7 bsl 3) . % network news subsystem 
uucp()     -> (8 bsl 3) . % UUCP subsystem 
cron()     -> (9 bsl 3) . % clock daemon 
authpriv() -> (10 bsl 3). % security/authorization messages (private) 
ftp()      -> (11 bsl 3). % ftp daemon 


%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%%----------------------------------------------------------------------
init([syslog]) ->
    S = init_syslog(local_host()),
    {ok,S};
init([syslog,Host]) ->
    S = init_syslog(Host),
    {ok,S};
init(Other) ->
    io:fwrite("~w: <ERROR> No support for ~w , no handler is added !~n",
	      [?MODULE,Other]),
    {error,not_supported}.

init_syslog(Host) ->
    {ok,Sock} = gen_udp:open(0),
    #state{sock=Sock,
	   host=Host}.


%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_event({error_report,_,{_,{Who,Level},{Msg,Args}}},S) ->
    Emsg = lists:flatten(io_lib:format(Msg,Args)),
    do_send(S#state.sock,S#state.host,S#state.port,{Who,Level,Emsg}),
    {ok,S};
handle_event({error_report,_,{_,{Who,Facility,Level},{Msg,Args}}},S) ->
    do_send(S#state.sock,S#state.host,S#state.port,{Facility,Who,Level,Msg}),
    {ok,S};
handle_event(_,State) ->
    {ok,State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}                            
%%----------------------------------------------------------------------
handle_call(Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler                              
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    gen_udp:close(State#state.sock),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% priorities/facilities are encoded into a single 32-bit 
%% quantity, where the bottom 3 bits are the priority (0-7) 
%% and the top 28 bits are the facility (0-big number).    

do_send(S,Host,Port,{Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg,
    gen_udp:send(S#state.sock,Host,Port,Packet);
do_send(S,Host,Port,{Facil,Who,Level,Msg}) ->
    Packet = "<" ++ i2l(Facil bor Level) ++ "> " ++ a2l(Who) ++ ": " ++ Msg,
    gen_udp:send(S#state.sock,S#state.host,Port,Packet).

local_host() ->
    {ok,Hname} = inet:gethostname(),
    Hname.

i2l(Int) -> integer_to_list(Int).

a2l(Atom) -> atom_to_list(Atom).
