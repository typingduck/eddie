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
%%% Mar, Apr 99 - jon@eddieware.org
%%%

-module(is_config).
-copyright('Copyright (c) 1991-98 Ericsson Telecom AB').
-author('patrik@erix.ericsson.se').
-modified_by('eric.yeo@ericsson.com.au').
-vc('$Id: is_config.erl,v 1.1 2000/10/27 22:20:25 dredd Exp $ ').
-export([run_load/1, run_store/1, remove_server/1,
	 create_mand_tables/1, wait_for_tables/1,
	 root/2, db_tables/0, subscribed_tables/0]).
%% Erlet API
-export([load/2, store/2]).

-include("inet_server.hrl").
-include("logger.hrl").

%% These are the db tables that the inet server utilises:
-define(DB_TABLES, [{frontend, set, record_info(fields, frontend)},
		    {backend, bag, record_info(fields, backend)},
		    {erlets, set, record_info(fields, erlets)},
		    {gen_storage, set, record_info(fields, gen_storage)},
		    {admit_ctrl, set, record_info(fields, admit_ctrl)},
		    {admit_static, set, record_info(fields, admit_static)}
		    ]).

-define(SUBSCRIBED_TABLES, [frontend]).

run_load(Config_file) ->
    wait_for_tables(5000),
    case config_file:parse(Config_file) of
	{ok, Config_data} ->

	    %?DEBUG("from config_file: ~p", [Config_data]),

	    case mnesia:transaction(fun purge/0, []) of
		{aborted, R1} ->
		    ?F("~s: ~p", [Config_file,
				  mnesia:error_description(R1)]);
		{atomic, R2} ->
		    R2
	    end,

	    case mnesia:transaction(fun root/2, [Config_file, Config_data]) of
		{aborted, Reason} ->
		    ?F("~s: ~p", [Config_file,
				  mnesia:error_description(Reason)]);
		{atomic, Result} ->
		    Result
	    end;
	{error, Reason} ->
	    ?F("~s: ~p", [Config_file,
			  Reason])
    end.

purge() ->
    %lists:foreach(fun(Table) ->
	%		  purge_table(Table)
	%	  end, db_tables()),
    ok.

root(Config_file, Config_data) ->
    Loaded = load_it(Config_file, Config_data),
    
%%   ?DEBUG("Loaded: ~p", [Loaded]),

    case run_store(Loaded) of
	ok ->
	    ok;
	_ ->
	    mnesia:abort("Store failed")
    end.

load_it(Config_file, Config_list)->
    lists:map(fun({_, "frontend", [Frontend_name], Frontend_cfg}) ->
		      case lists:keysearch("protocolmodule",
					   2,
					   Frontend_cfg) of
			  {value,
			   {_, "protocolmodule", [Protocol_module]}} ->
			      [{frontend, list_to_atom(Frontend_name), 
				load_values(list_to_atom(Protocol_module),
					    Frontend_cfg)}];
			  _ ->
			      mnesia:abort(
				lists:concat(["ProtocolModule definition missing,"
					      " in configure file: ", Config_file,
					      " for frontend ", Frontend_name]))
		      end
	      end,
	      Config_list).


%% load_values is used to interpret the strings from config_file:parse/1.
%% The interpretation is done by the module the configuration is intended
%% for - may be the general one (this module),the protocol module or an
%% Erlet module.
load_values(Protocol_module, [{Row, Key, Value} | Rest]) ->
    [exec_load(Row, {Key, Value}, [?MODULE, Protocol_module]) |
     load_values(Protocol_module, Rest)];
%% Context inroducers:
load_values(Protocol_module, [{_Row, "backend", [Endpoint_name, Backend_node],
			       Backend_context} | Rest]) ->
    [{backend, list_to_atom(Endpoint_name), list_to_atom(Backend_node),
      load_schedule(Backend_context)} | load_values(Protocol_module, Rest)];
load_values(Protocol_module, [{_Row, "erlets", [Erlets_name], Erlets_context} |
			     Rest]) ->
    [{erlets, list_to_atom(Erlets_name), load_erlets(Erlets_context)} |
    load_values(Protocol_module, Rest)];
load_values(Protocol_module, [{_Row, "endpointconfig", [Endpoint_name], Endpoint_context} |
			     Rest]) ->
    [{endpoint, list_to_atom(Endpoint_name), load_endpoint(Endpoint_context)} |
    load_values(Protocol_module, Rest)];

load_values(_, []) ->
    [];
load_values(_, [{Row, Key, Value} | _]) ->
    mnesia:abort(lists:concat(["Error on line ", Row, " in configuration file"]));
load_values(_, [{Row, _, _, _} | _]) ->
    mnesia:abort(lists:concat(["Error on line ", Row, " in configuration file"]));
load_values(_, What) ->
    mnesia:abort(lists:concat(["Wrong format on configuration file: ", What])).

load_endpoint([{Row, "port", [Port]} | Rest]) -> 
    case erlet_utils:is_int(Port) of
	true ->
	    [{port, list_to_integer(Port)} | load_endpoint(Rest)];
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'Port N', "
				       "type(N) = integer"]))
    end;
load_endpoint([{Row, "readtimeout", [Time]} | Rest]) -> 
    case erlet_utils:is_int(Time) of
	true ->
	    [{read_timeout, 1000 * list_to_integer(Time)} | load_endpoint(Rest)];
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'ReadTimeout Time (seconds)', "
				       "type(Time) = integer"]))
    end;
load_endpoint([{Row, "keepalivetimeout", [Time]} | Rest]) -> 
    case erlet_utils:is_int(Time) of
	true ->
	    [{keep_alive_timeout, 1000 * list_to_integer(Time)} | load_endpoint(Rest)];
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'KeepAliveTimeout Time (seconds)', "
				       "type(Time) = integer"]))
    end;
load_endpoint([{Row, "externalproxy", [Prog|Args]} | Rest]) ->
    [{external_proxy, [Prog|Args]} | load_endpoint(Rest)];
load_endpoint([{Row, What, [_]} | Rest]) -> 
	    mnesia:abort(lists:concat(["Error on line ", Row, ", Unsupported variable:"
				       ,What]));
load_endpoint(_) ->
    [].

load_schedule([{Row, "schedule", [], Schedule_context} | Rest]) ->
    [load_backend(Schedule_context) | load_schedule(Rest)];
load_schedule([]) ->
    [].

load_backend([{Row, "patterns", [Key | List_of_patterns]} | Rest]) ->
    [{schedule_patterns, list_to_atom(Key), List_of_patterns} | load_backend(Rest)];
load_backend([{_, "proxy", [IP, Port]} | Rest]) ->
    [{proxy, [IP, list_to_integer(Port)]} | load_backend(Rest)];
load_backend([{_, "erlets", [Erlets_name]} | Rest]) ->
    [{erlets, list_to_atom(Erlets_name)} | load_backend(Rest)];
load_backend([]) ->
    [];
load_backend([{Row, _, _}]) ->
    mnesia:abort(lists:concat(["Syntax error on line ", Row]));
load_backend(What) ->
    mnesia:abort(lists:concat(["backend configured erroneously: ", What])).

load_erlets([{Row, "erlet", [Erlet_name], Erlet_config} | Rest]) ->
    Erlet = list_to_atom(Erlet_name),
    case catch Erlet:load(Row, Erlet_config) of
	ok ->
	    [{erlet, Erlet, []} | load_erlets(Rest)];
	{ok, Config} ->
	    [{erlet, Erlet, Config} | load_erlets(Rest)];
	What ->
	    mnesia:abort(
	      lists:concat(["Couldn't load erlet ", Erlet," with data ", 
			    Erlet_config, " from line ", Row,
			    " in the configuration file, reason: ", What]))
    end;
load_erlets([]) ->
    [].

exec_load(Row, Value, [Module | Rest]) ->
    case catch Module:load(Row, Value) of
	{ok, Converted_value} ->
	    Converted_value;
	_ ->
	    exec_load(Row, Value, Rest)
    end;
exec_load(Row, Value, []) ->
    mnesia:abort(lists:concat(["Syntax error ", Value, " on line ", Row,
			       " in the configuration file"])).

%%
%% Phase 2: Run_Store
%%
%% Run_Store; this puts config data into the database table that corresponds
%%        to the protocol_module frontend_name & respective Erlets.
%%
run_store(Frontend_cfgs) ->
    lists:foreach(fun(Config_list) ->
			  run_store2(Config_list)
		  end, Frontend_cfgs).

run_store2([{frontend, Frontend, Frontend_context}])->
    case lists:keysearch(protocol_module, 1, Frontend_context) of
	{value, {protocol_module, Protocol_module}} ->
	    run_store2(Frontend,
		       Frontend_context,
		       [?MODULE, Protocol_module]);
	false ->
	    mnesia:abort(lists:concat(["Couldn't find a protocol module"
				       " in configuration for frontend ", Frontend]))
    end.

run_store2(Frontend, Config_list, [First | Rest]) ->
    case catch First:store(Frontend, Config_list) of
	{'EXIT', {undef, {First, store, _}}} ->
	    run_store2(Frontend, Config_list, Rest);
	{'EXIT', {function_clause, {First, store, _}}} ->
	    run_store2(Frontend, Config_list, Rest);
	{'EXIT', Reason} ->
	    run_store2(Frontend, Config_list, Rest);
	{ok, Config_data} ->

%%	    ?DEBUG("Store data: ~p", [Config_data]),

	    lists:foreach(fun(Config_entry) ->
				  is_db:write(Config_entry)
			  end, Config_data),
	    run_store2(Frontend, Config_list, Rest);
	{error, Reason} ->
	    run_store2(Frontend, Config_list, Rest)
    end;
run_store2(_, _, []) ->
    ok.

%%
%% Create mandatory database tables, Mnesia is used as database back-end.
%%
create_mand_tables(Nodes) ->

%%    ?DEBUG("Nodes = ~p, record_info(fields, backend) = ~p", 
%%	   [Nodes, record_info(fields, backend)]),

    lists:foreach(fun({Name, Type, Rec_info}) ->
			  case is_db:create_table(Name,
						  [{attributes, Rec_info},
						   {type, Type},
						   {disc_copies, Nodes}]) of
			      {atomic, ok} ->
				  ok;
			      Else ->
				  ?FATAL(?F("Failed creating ~p, reason:~p",
					    [Name, Else])),
				  exit(dberror)
			  end
		  end, ?DB_TABLES).
%%
%% Phase 3: Remove
%%
remove_server(Frontend_name) ->
    Protocol_module = is_db:read(Frontend_name, protocol_module),
    Protocol_module:remove().

remove_erlets_config(Frontend_name) ->
    Erlets = is_db:read(Frontend_name, erlets),
    remove_traverse(Erlets).

remove_traverse([]) ->
    ok;
remove_traverse([Erlet | Rest]) ->
    case catch Erlet:remove() of
	{'EXIT', {undef, {Erlet, remove, _}}} ->
	    remove_traverse(Rest);
	{'EXIT',{function_clause,{Erlet, remove, _}}} ->
	    remove_traverse(Rest);
	{'EXIT', Reason} ->
	    ?ERROR(?F("EXIT: ~p", [Reason])),
	    remove_traverse(Rest);
	ok ->
	    remove_traverse(Rest);
	{error, Reason} ->
	    ?ERROR(?F("Error: ~p", [Reason])),
	    remove_traverse(Rest)
    end.
%%
%% General configuration directives (ALL protocol modules, i.e. EndpointConfig)
%% Phase 1: Load
%%
load(Row, {"protocolmodule", [Protocol_module]}) ->
    {ok, {protocol_module, list_to_atom(Protocol_module)}};
load(Row, {"admitcontrol", [Bool]}) ->
	case erlet_utils:is_bool(Bool) of
	    true ->
		{ok, {admit_control, list_to_atom(Bool)}};
	    _ -> 
		mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
					   " 'AdmitControl true | false'"]))
	end;
load(Row, {"admittime", [Admit_time]}) ->
    case erlet_utils:is_int(Admit_time) of
	true ->
	    {ok, {admit_time, 1000 * list_to_integer(Admit_time)}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitTime Time', "
				       "type(Time) = integer"]))
    end;
load(Row, {"admitalways", Ip_addresses}) ->
    case erlet_utils:make_ip_list(Ip_addresses) of
	{ok, Ip_list} ->
	    {ok, {admit_always, Ip_list}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitAlways (Ip_address)'"]))
    end;
load(Row, {"admitnever", Ip_addresses}) ->
    case erlet_utils:make_ip_list(Ip_addresses) of
	{ok, Ip_list} ->
	    {ok, {admit_never, Ip_list}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitNever (Ip_address)'"]))
    end;
load(Row, {"admitblockedpage", [URI]}) ->
    %% Add real check.
    case {ok, URI} of
	{ok, URI} ->
	    {ok, {admit_blocked_page, URI}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitBlockedPage URI'"]))
    end;
load(Row, {"admitblockedmesgfile", [Filename]}) ->
    {ok, {admit_blocked_message, do_read_file(Filename)}};
load(Row, {"admitrejectpage", [URI]}) ->
    %% Add real check.
    case {ok, URI} of
	{ok, URI} ->
	    {ok, {admit_reject_page, URI}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitRejectPage URI'"]))
    end;
load(Row, {"admitrejectmesgfile", [Filename]}) ->
    {ok, {admit_reject_message, do_read_file(Filename)}};
load(Row, {"admitqueueplaces", [N]}) ->
    case erlet_utils:is_int(N) of
	true ->
	    {ok, {admit_queue_places, list_to_integer(N)}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitQueuePlaces N', "
				       "type(N) = integer"]))
    end;
load(Row, {"admitqueuemesgfile", [Filename]}) ->
    {ok, {admit_queue_message, do_read_file(Filename)}};
load(Row, {"admitmaxsessions", [N]}) ->
    case erlet_utils:is_int(N) of
	true ->
	    {ok, {admit_max_sessions, list_to_integer(N)}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitMaxSessions N', "
				       "type(N) = integer"]))
    end;
load(Row, {"admitqueuepage", [URI]}) ->
    %% Add real check.
    case {ok, URI} of
	{ok, URI} ->
	    {ok, {admit_queue_page, URI}};
	_ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'AdmitQueuePage URI'"]))
    end;
load(Row, {"arrivalcountingperiod", [N]}) ->
    case erlet_util:is_int(N) of
        true ->
	    {ok, {arrival_counting_period, list_to_integer(N)}};
        _ ->
	    mnesia:abort(lists:concat(["Error on line ", Row, ", should be"
				       " 'ArrivalCountingPeriod N', "
				       "type(N) = integer"]))
    end.


%%
%% store gives the records as stored in Mnesia.
%%
store(Frontend, Config_list) ->
    {ok, [#frontend{name =
		    Frontend,
		    protocol_module = 
		    erlet_utils:keysearch_value(protocol_module, Config_list),
		    endpoints_config =
		    build_endpoint(Frontend, Config_list),
		    admit_ctrl =
		      case erlet_utils:keysearch_value(admit_control,
						     Config_list) of
			true -> true;
			_ -> false
		      end,
                    arrival_counting_period =
                        erlet_utils:keysearch_default_find(
                                           arrival_counting_period,
                                           Config_list,
                                           5000)
		   }] ++
     build_admit(Frontend, Config_list) ++
     build_backends(Frontend, Config_list) ++
     build_erlets(Frontend, Config_list)}.

build_endpoint(_, Config_list) ->
%% No direct connection to frontend (yet ...).
%% The connection is through the endpoint. 
    case catch
	lists:flatten(
	  lists:map(
	    fun({endpoint, EP_name, E_config}) ->
		    #endpoint_config{name = EP_name,
				     port =
				     erlet_utils:keysearch_must_find(port, E_config),
				     read_timeout = 
				     erlet_utils:keysearch_must_find(read_timeout, E_config),
				     keep_alive_timeout = 
				     erlet_utils:keysearch_must_find(keep_alive_timeout, E_config),
				     external_proxy =
				       erlet_utils:keysearch_default_find(
					 external_proxy, E_config, "")};
	       (_) ->
		    []
	    end, Config_list)) of
	List when list(List) ->
	    List;
	_ ->
	    mnesia:abort("EndpointConfig configuration error")
    end.

build_admit(Frontend, Config_list) ->
    [#admit_ctrl{frontend = Frontend,
		 time = erlet_utils:keysearch_default_find(admit_time,
							   Config_list,
							   10),
		 qos_config = {"ServiceLevel", [{default, 1, ""}]},  
		 max_sessions = erlet_utils:keysearch_default_find(admit_max_sessions,
								   Config_list,
								   256),
		 blocked_message = erlet_utils:keysearch_default_find(
				     admit_blocked_message,
				     Config_list,
				     undefined),
		 blocked_page = erlet_utils:keysearch_default_find(
				  admit_blocked_page,
				  Config_list,
				  "/eddie_admit_blocked"),
		 reject_message = erlet_utils:keysearch_default_find(
				     admit_reject_message,
				     Config_list,
				     undefined),
		 reject_page = erlet_utils:keysearch_default_find(
				 admit_reject_page,
				 Config_list,
				 "/eddie_admit_reject"),
		 queue_message = erlet_utils:keysearch_default_find(
				     admit_queue_message,
				     Config_list,
				     undefined),
		 queue_page = erlet_utils:keysearch_default_find(
				admit_queue_page,
				Config_list,
				"/eddie_admit_queue"),
		 queue_places = erlet_utils:keysearch_default_find(
				  admit_queue_places,
				  Config_list,
				  1024)}] ++
	build_admit_static(erlet_utils:keysearch_default_find(
			     admit_always,
			     Config_list,
			     []),
			   always) ++
	build_admit_static(erlet_utils:keysearch_default_find(
			     admit_never,
			     Config_list,
			     []),
			   never).

build_admit_static(Ip_list, Status) ->
    lists:map(fun(Ip) ->
		      #admit_static{ip = Ip,
				    status = Status}
	      end, Ip_list).

build_backends(_, Config_list) ->
%% No direct connection to frontend (yet ...).
%% The connection is through the endpoint. 
    lists:flatten(
      lists:map(
	fun({backend, EP_name, Backend_node, Schedules}) ->
		lists:map(fun(Schedule_list) ->
				  #backend{endpoint_name = EP_name,
					   spec = find_type(Schedule_list), 
					   backend_node = Backend_node,
					   schedule_patterns =
					   build_key_patterns(Schedule_list)}
			  end, Schedules);
	   (_) ->
		[]
	end, Config_list)).

build_erlets(Frontend_name, Config_list) ->
    %% Supports saving in Mnesia.
    lists:flatten(
      lists:map(fun({erlets, Erlets_name, Erlets_config}) ->
			{Erlet_data, Erlets} =
			    lists:mapfoldl(fun({erlet, 
						Erlet,
						Erlet_config}, Acc) ->
						   case catch Erlet:store(Erlets_name,
									  Erlet_config) of
						       {ok, {Key, Value}} ->
							   {#gen_storage{key = 
									 {Frontend_name, Erlet},
									 key_value_pair =
									 {Key, Value}},
							    [Erlet | Acc]};
						       _ ->
							   {[], [Erlet | Acc]}
						   end
					   end, [], Erlets_config),
			[#erlets{name = Erlets_name,
				 list = Erlets} |
			 lists:flatten(Erlet_data)];
		   (_) ->
			[]
		end, Config_list)).

%% Misc Utils
%%

%% Types and things,
find_type(Schedule_list) ->
    case lists:keysearch(proxy, 1, Schedule_list) of
	{value, {_, [Ip, Port]}} ->
	    {proxy, Ip, Port};
	_ ->
	    case lists:keysearch(erlets, 1, Schedule_list) of
		{value, {_, Erlets}} ->
		    {erlets, Erlets}
	    end
    end.

%% Build patterns for schedule matching.
build_key_patterns(Schedule_patterns) ->
    lists:flatten(
      lists:map(fun({schedule_patterns, Key, List_of_patterns}) ->
			{Key, build_patterns(List_of_patterns)};
		   (_) ->
			[]
		end, Schedule_patterns)).

build_patterns(Patterns) ->
    build_patterns(Patterns, {[], []}).

build_patterns([[$^ | Anti_pattern] | Rest], {List1, List2}) ->
    build_patterns(Rest, {List1, [Anti_pattern | List2]});
build_patterns([Pattern | Rest], {List1, List2}) ->
    build_patterns(Rest, {[Pattern | List1], List2});
build_patterns([], {List1, List2}) ->
    {lists:reverse(List1), lists:reverse(List2)}.


%% Wait for tables.
wait_for_tables(Time) ->
    db:wait_for_tables(db_tables(), Time).

%% The inet server's tables.
db_tables() ->
    lists:map(fun({Table, Type, Record_info}) ->
		      Table
	      end, ?DB_TABLES).

%% These tables should be subscribed.
subscribed_tables() ->
    ?SUBSCRIBED_TABLES.
%% Remove data from tables before repopulating them.
purge_table(Table) ->
    lists:foreach(fun(Key) ->
			  mnesia:delete({Table,Key})
		  end,mnesia:all_keys(Table)).


%% Read and return the contents of the file.
%% This function must be called in an mnesia transaction.
%%
do_read_file(undefined) ->
    undefined;
do_read_file(Filename) ->
    case file:read_file(Filename) of
	{ok, Bin} ->
	    binary_to_list(Bin);
	{error, Reason} ->
	    mnesia:abort(lists:flatten(io_lib:format("Can't read file ~p: ~p", [Filename, Reason])))
    end.
