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
%%% File    : servant_util.erl
%%% Author  : Joakim G. <jocke@force.du.etx.ericsson.se>
%%% Created : 17 Sep 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------

-module(servant_util).
-author('jocke@erix.ericsson.se').
-vc('$Id: servant_util.erl,v 1.1 2000/10/27 22:20:27 dredd Exp $ ').
-export([run/2,replacements/1,replacements/2,extract_command/2,format/1]).

-include("db.hrl").
-include("ip.hrl").

%% run

run(Commands,Replacements) ->
  case extract_command(Commands,Replacements) of
    {module,Module,Function,Args} ->
      case catch apply(Module,Function,Args) of
	ok ->
	  ok;
	{error,Reason} ->
	  {error,Reason};
	{'EXIT',Reason} ->
	  {error,Reason}
      end;
    {exec,Exec,Args} ->
      case os:cmd(Exec++Args) of
	    "" ->
	        ok;
	    Reason ->
	        {error,Reason}
      end;
    skip ->
      ok;
    {error,Reason} ->
      {error,Reason}
  end.

%% replacements

replacements(S) ->
  replacements(S,[]).

replacements(S,CustomReplacements) ->
  T=S#server.template,
  [{"@Node",S#server.node},
   {"@Interface",S#server.interface},
   {"@Alias",S#server.alias},
   {"@IPAddress",?IP2STR(S#server.ip_address)},
   {"@FrontEndCluster",T#template.cluster},
   {"@OriginNode",T#template.node},
   {"@OriginInterface",T#template.interface},
   {"@OriginIPAddress",?IP2STR(T#template.ip_address)},
   {"@Port",integer_to_list(T#template.port)}|CustomReplacements].

%%
%% Name: extract_command
%% Purpose: extract command and arglist from a given text line
%%

extract_command([],Replacements) ->
  skip;
extract_command(Commands,Replacements) ->
  case misc:multikeysearch([node(),generic,module,exec],1,Commands) of
    {value,{_,module,Module,Function,Args}} ->
      ExpandedArgs=
	lists:map(fun(Arg) ->
		      case lists:keysearch(Arg,1,Replacements) of
			false ->
			  Arg;
			{value,{Arg,ExpandedArg}} ->
			  ExpandedArg
		      end
		  end,Args),
      {module,Module,Function,ExpandedArgs};
    {value,{_,exec,Exec,Args}} ->
      ExpandedArgs=
	lists:foldl(fun(Arg,Acc) ->
			case lists:keysearch(Arg,1,Replacements) of
			  false ->
			    lists:concat([Acc," ",Arg]);
			  {value,{Arg,ExpandedArg}} ->
			    lists:concat([Acc," ",ExpandedArg])
			end
		    end,[],Args),
      {exec,Exec,ExpandedArgs};
    {value,{module,Module,Function,Args}} ->
      ExpandedArgs=
	lists:map(fun(Arg) ->
		      case lists:keysearch(Arg,1,Replacements) of
			false ->
			  Arg;
			{value,{Arg,ExpandedArg}} ->
			  ExpandedArg
		      end
		  end,Args),
      {module,Module,Function,ExpandedArgs};
    {value,{exec,Exec,Args}} ->
      ExpandedArgs=
	lists:foldl(fun(Arg,Acc) ->
			case lists:keysearch(Arg,1,Replacements) of
			  false ->
			    lists:concat([Acc," ",Arg]);
			  {value,{Arg,ExpandedArg}} ->
			    lists:concat([Acc," ",ExpandedArg])
			end
		    end,[],Args),
      {exec,Exec,ExpandedArgs};
    false ->
      {error,command_not_found}
  end.

%% format

format(S) when record(S,server) ->
  "Server: "++format_address(S)++" in cluster "++
    (S#server.cluster)#cluster.name++" on "++format_node(S);
format(T) when record(T,template) ->
  "Server: "++format_address(T)++" in cluster "++T#template.cluster++
    " on "++format_node(T);
format([]) ->
  "";
format([ST|Rest]) ->
  format(ST)++"\n"++format(Rest).

format_address(S) when record(S,server) ->
  ?IP2STR(S#server.ip_address)++":"++
    integer_to_list((S#server.template)#template.port);
format_address(T) ->
  ?IP2STR(T#template.ip_address)++":"++integer_to_list(T#template.port).

format_node(S) when record(S,server) ->
  atom_to_list(S#server.node)++" (origin: "++
    atom_to_list((S#server.template)#template.node)++")";
format_node(T) ->
  atom_to_list(T#template.node)++" (origin: "++
    atom_to_list(T#template.node)++")".
