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
%%% File    : dns_server.erl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : A load balanced DNS name server.
%%% Created :  4 Mar 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(dns_server).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-vsn('$Revision: /main/eddie/eddie-1.0/5').
-author('magnus@erix.ericsson.se').

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {udp = [],
	        tcp,
		port,
	        tcp_accept = false}).

-record(udp, {pid,
	      ip}).

-include("dns.hrl").
-include_lib("../../misc/include/logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, dns_server}, dns_server, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),

    %% Fix up logging.
    LogFile = dns_catalog:logfile(),
    Verbose = dns_catalog:verbose(),
    %% Initiate the server DNS authority ~
    Port = get_port(),
    ?INFO(?F("dns listening port: ~p",[Port])),
    ?INFO(?F("logfile: ~p",           [LogFile])),
    ?INFO(?F("forwarders: ~p",        [dns_catalog:forwarders()])),
    ?INFO(?F("lb netmasks: ~p",       [dns_catalog:netmasks()])),
    ?INFO(?F("lb ttl: ~p",            [dns_catalog:lb_ttl()])),
    ?INFO(?F("root hints: ~p",        [dns_catalog:all_root_hints()])),
    ?INFO(?F("preferential lb: ~p",   [dns_catalog:prefs()])),
    %% TCP handles several interfaces listening on "any" IP !
    case gen_tcp:listen(Port, [{reuseaddr, true},
			       {header, 4},
			       binary,
			       {packet, 2},
			       {ip, any}]) of
	{ok, Tcp} ->
	    %% Start an accept process after 0 ms in handle_info/2 !
	    %% This makes it possible to put the accept process underneath
	    %% the DNS supervisor.
	    case inet:getif(Tcp) of
		{ok, []} ->
		    ?FATAL(?F("Can't get any interfaces", [])),
		    {stop, {get_if_error, []}};
		{ok, IF} ->
		    Udp = initiate_udp(IF, Port),
		    initiate_own_ip(IF),
		    {ok, #state{udp = Udp,
				tcp = Tcp,
				port = Port}, 0};
		Error ->
		    ?FATAL(?F("Can't get interfaces - ~p", [Error])),
		    {stop, {get_if_error, Error}}
	    end;
	Error ->
	    ?FATAL(?F("Can't listen on port ~p - ~p", [Port, Error])),
	    {stop, {listen, Error}}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
handle_info(timeout, State) when State#state.tcp_accept == false ->
    start_accept(State);

handle_info({Pid, accepted}, State) when State#state.tcp_accept == Pid ->
    start_accept(State);

handle_info({Pid, terminated, Id}, State) when State#state.tcp_accept == Pid ->
    clean_supervisor(Id),
    start_accept(State);

handle_info({Pid, terminated, Id}, State) ->
    %% This was an already accepted connection.
    clean_supervisor(Id),
    {noreply, State};

handle_info({'EXIT',Pid,shutdown}, State) when State#state.tcp_accept == Pid ->
    %% The supervisor is going to shutdown us soon !
    {noreply, State#state{tcp_accept = undefined}};

handle_info({'EXIT',Pid,_}, State) when State#state.tcp_accept == Pid ->
    clean_supervisor(),
    start_accept(State);

handle_info({'EXIT', Pid, Reason}, State) ->
    handle_exit(Pid, Reason, State);

handle_info({tcp_closed, Tcp}, State) when State#state.tcp == Tcp ->
    exit(State#state.tcp_accept, kill),
    Port = State#state.port,
    ?INFO(?F("Listen socket (~p) has been closed - restarting", [Port])),
    case gen_tcp:listen(Port, [{reuseaddr, true},
			       {header, 4},
			       binary,
			       {packet, 2},
			       {ip, any}]) of
	{ok, NewTcp} ->
	    {noreply, State#state{tcp = NewTcp}};
	Error ->
	    ?FATAL(?F("Can't listen on port ~p - ~p", [Port, Error])),
	    {stop, Error, State#state{tcp = undefined}}
    end;

handle_info(Info, State) ->
    %% io:format("~p got: ~w~n",[?MODULE,Info]),
    {noreply, State}.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    F = fun(Udp) -> exit(Udp#udp.pid, kill) end,
    lists:foreach(F, State#state.udp),
    if
	State#state.tcp == undefined ->
	    ok;
	true ->
	    gen_tcp:close(State#state.tcp)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Start a new TCP accept process.
%%
start_accept(State) ->
%    case dns_tcp_accept:start_link(State#state.tcp,self()) of
    Id = now(),
    case catch supervisor:start_child(dns_sup,
				      {{dns_tcp_accept, Id} ,
				       {dns_tcp_accept,
					start_link,
					[State#state.tcp, self(), Id]},
				       temporary,
				       2000,
				       worker,
				       [dns_tcp_accept]}) of
	{ok, Pid} ->
	    link(Pid),
	    {noreply, State#state{tcp_accept = Pid}};
	{'EXIT', _} ->
	    ?FATAL(?F("Can't start without dns_sup supervisor", [])),
	    {stop, no_supervisor, State};
	Error ->
	    ?FATAL(?F("Couldn't start tcp accept process - ~p", [Error])),
	    {stop, Error, State}
    end.

clean_supervisor(Id) ->
    catch supervisor:delete_child(dns_sup, {dns_tcp_accept, Id}).

%%
%% This is ugly, but we have to clean the supervisor in case
%% the dns_tcp_accept process received a kill exit signal which
%% it cant trap.
%%
clean_supervisor() ->
    case catch supervisor:which_children(dns_sup) of
	Childs when list(Childs) ->
	    clean_supervisor1(Childs);
	_ ->
	    ok
    end.

clean_supervisor1([{{dns_tcp_accept,Id}, undefined, _, _}| T]) ->
    clean_supervisor(Id),
    clean_supervisor1(T);
clean_supervisor1([_|T]) ->
    clean_supervisor1(T);
clean_supervisor1(_) ->
    ok.

%%
%% Handle different types of terminating Pid's.
%%
handle_exit(Pid, Reason, State) ->
    catch do_handle_exit(Pid, Reason, State).

do_handle_exit(Pid, Reason, State) ->
    Udps = State#state.udp,
    case lists:keymember(Pid, #udp.pid, Udps) of
	true ->
	    Udps1 = lists:keydelete(Pid, #udp.pid, Udps),
	    {stop, Reason, State#state{udp = Udps1}};
	_ ->
	    {noreply, State}
    end.

%%% ----------------------------------------------------------------
%%% Misc. functions.
%%% ----------------------------------------------------------------

get_port() ->
    case application:get_env(port) of
	{ok, Port} when integer(Port) -> Port;
	_                             -> ?NAMESERVER_PORT
    end.

%%
%% Open an UDP socket per interface (except loopback) !
%%
initiate_udp(IFs, Port) ->
    IPs = ips(IFs),
    lists:foreach(fun(X) -> ?INFO(?F("listening on ~p",[X])) end, IPs),
    F = fun(IP) -> start_udp(Port, IP) end,
    lists:map(F, IPs).

start_udp(Port, IP) ->
    Pid = dns_udp:start_link(Port, IP),
    #udp{pid = Pid,
	 ip = IP}.

ips(IPs) -> [element(1,X) || X <- IPs].  
%ips([{{127,0,0,1}, _, _}|IF])       -> ips(IF);
%ips([{{0,0,0,0,0,0,0,1}, _, _}|IF]) -> ips(IF);
%ips([{IP, _, _}|IF])                -> [IP|ips(IF)];
%ips([])                             -> [].

%%
%% Store our IP(s) in dns_catalog.
%%
initiate_own_ip(IF) ->
    dns_catalog:init_ip(ips(IF)),
    ok.

