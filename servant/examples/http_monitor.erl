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
%%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%%% Telecom AB. All Rights Reserved.
%%%
%%% Contributor(s): ______________________________________.
%%%

%%%----------------------------------------------------------------------
%%% File    : http_monitor.erl
%%% Author  : Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Created : 29 Sep 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

-module(http_monitor).
-author('jocke@erix.ericsson.se').
-export([start/4]).

start(Parent,IPAddress,Port,Time) ->
  process_flag(trap_exit,true),
  case timer:send_interval(list_to_integer(Time),poll) of
    {ok,Ref} ->
      case inet:getaddr(IPAddress,inet) of
	{ok,IP} ->
	  loop(Parent,IP,Port,Ref,trunc(list_to_integer(Time)/2));
	{error,Reason} ->
	  exit(Reason)
      end;
    {error,Reason} ->
      exit(Reason)
  end.

loop(Parent,IP,Port,Ref,Timeout) ->
  receive
    poll ->
      case gen_tcp:connect(IP,Port,[{active,false},{packet,0}],Timeout) of
	{ok,Socket} ->
	  gen_tcp:send(Socket,"HEAD / HTTP/1.0\r\n\r\n"),
	  case do_recv(Socket,[],Timeout) of
	    [$H,$T,$T,$P,$/,$1,$.,N,$ ,$2,$0,$0,$ ,$O,$K|_] ->
	      Parent ! {report,"ok"},
	      gen_tcp:close(Socket),
	      loop(Parent,IP,Port,Ref,Timeout);
	    %% Not an error, wait for new poll.
	    [$H,$T,$T,$P,$/,$1,$.,N,$ ,$5,$0,$3|_] ->
	      loop(Parent,IP,Port,Ref,Timeout);
	    WrongResponse ->
	      Parent ! {report,WrongResponse},
	      gen_tcp:close(Socket),
	      loop(Parent,IP,Port,Ref,Timeout)
	  end;
	{error,enfile} ->
	  %% Not an error, wait for new poll.
	  loop(Parent,IP,Port,Ref,Timeout);
	{error,Reason} ->
	  Parent ! {report,Reason},
	  loop(Parent,IP,Port,Ref,Timeout)
      end;
    close ->
      timer:cancel(Ref);
    {'EXIT',From,Reason} ->
      timer:cancel(Ref)
  end.

do_recv(Socket,Data,Timeout) ->
  case gen_tcp:recv(Socket,0,Timeout) of
    {ok,NewData} ->
      do_recv(Socket,NewData++Data,Timeout);
    {error,closed} ->
      Data
  end.
