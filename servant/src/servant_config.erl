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
%%% File    : servant_config.erl
%%% Author  : Joakim G. <jocke@erix.ericsson.se>
%%% Created : 13 Jul 1998 by Joakim G. <jocke@force.du.etx.ericsson.se>
%%%----------------------------------------------------------------------
%% fiddled with by pekka

-module(servant_config).
-author('jocke@erix.ericsson.se').
-vc('$Id: servant_config.erl,v 1.1 2000/10/27 22:20:27 dredd Exp $ ').
-export([create_tables/1,load/1, print_table/1]).
-include("logger.hrl").
-include("db.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").

%% create_tables

create_tables(Nodes) ->
    create_tables(Nodes,[{root,record_info(fields,root),set},
			 {node,record_info(fields,node),set},
			 {cluster,record_info(fields,cluster),set},
			 {template,record_info(fields,template),bag}]).

create_tables(Nodes,[]) -> ok;
create_tables(Nodes,[{Name,RecordInfo,Type}|Rest]) ->
    case mnesia:create_table(Name,[{attributes,RecordInfo},
				   {disc_copies,Nodes},
				   {type,Type}]) of
	{atomic,ok} ->
	    create_tables(Nodes,Rest);
	{aborted,Reason} ->
	    mnesia:error_description(Reason)
    end.

%% set config mnesia tables provided a file
load(ConfigFile) ->
    db:wait_for_tables(?TABLES,5000),
    case config_file:parse(ConfigFile) of
	{ok,Body} ->
	    put(start_order,0),
	    case mnesia:transaction(fun purge/0, []) of
		{aborted,R1} ->
		    ?F("~s: ~p",[ConfigFile,mnesia:error_description(R1)]);
		{atomic,R2} ->
		    R2
	    end,
        db:wait_for_tables(?TABLES,5000),
	    case mnesia:transaction(fun root/1,[Body]) of
		{aborted,Reason} ->
		    ?F("~s: ~p",[ConfigFile,mnesia:error_description(Reason)]);
		{atomic,Result} ->
		    Result
	    end;
	{error,Reason} ->
	    ?F("~s: ~p",[ConfigFile,Reason])
    end.

%% root

purge() ->
     %purge_table(root),
     %purge_table(node),
     %purge_table(cluster),
     purge_table(template),
    ok.

root(Body) ->
    case (catch get_mig_conf:root(#root{},Body)) of
	{error, Error} -> 
        io:format("get_mig_conf error: ~w~n", Error),
        mnesia:abort(Error);
	R              -> FR = lists:flatten(R),
			  lists:foreach(fun(Confs) ->
                        % io:format("mnesia:write(~w)~n", [ Confs ]),
						mnesia:write(Confs)
					end,
					FR)
    end,
    % io:format("root: added new config - validating~n"),
    validate(frontend_cluster,
	     mnesia:match_object(mnesia:table_info(cluster, wild_pattern))),
    validate(backend_cluster,
	     mnesia:match_object(mnesia:table_info(cluster, wild_pattern))),
    print_table(template),
    ok.

%% purge_table
purge_table(Table) -> 
    % io:format("purged table(~w)~n", [ Table ]),
    lists:foreach(fun(Key) -> mnesia:delete({Table,Key}) end,
				    mnesia:all_keys(Table)).

%% debugging: print_table
print_table(Tab) ->
    mnesia:transaction(fun out_table/1, [ Tab ]).

out_table(Table) -> lists:foreach(fun(Key) -> 
                io:format("MT(~w:~w):~w~n", [ Table, Key, mnesia:read({Table,Key})] ) end,
				mnesia:all_keys(Table)).

validate(frontend_cluster, CL) ->
    BackendNames = [C#cluster.name || C <- CL,
				      C#cluster.cluster_type == backend],
    FrontendCs = [C || C <- CL,
		       C#cluster.cluster_type == frontend],
    valid_backend_clusters(BackendNames,FrontendCs);

validate(backend_cluster, CL) ->
    CNameL = [C#cluster.name || C <- CL,
				C#cluster.cluster_type == backend,
				C#cluster.failover_nodes /= []] ,
    case CNameL of 
	[]    ->   ok;
	Names ->
	    throw({error, (?F("Fail-over nodes not allowed in back-end cluster(s): ~p",
			      [Names]))})
    end.

valid_backend_clusters(BackendNames,[]) -> ok;
valid_backend_clusters(BackendNames,[FrontendC|Rest]) ->
    F =	fun(Name) ->
		case lists:member(Name,BackendNames) of
		    true ->
			ok;
		    false ->
			throw({error, (?F("Unknown back-end cluster ~s in "
					  "front-end ~s.",
					  [Name,FrontendC#cluster.name]))})
		end
	end,
    lists:foreach(F, FrontendC#cluster.backend_clusters),
    valid_backend_clusters(BackendNames, Rest).



