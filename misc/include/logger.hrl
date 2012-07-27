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

%% Write the report at the local node only.
-define(FATAL(String_),
	error_logger:error_report(fatal,{?MODULE,?LINE,false,String_})).
-define(ERROR(String_),
	error_logger:error_report(error,{?MODULE,?LINE,false,String_})).
-define(INFO(String_),
	error_logger:info_report(info,{?MODULE,?LINE,false,String_})).
%-define(TEMP_INFO(String_),
%    error_logger:info_report(info,{?MODULE,?LINE,false,String_})).
-define(TEMP_INFO(String_), 0).
%-define(DEBUG(String_),
%	error_logger:info_report(info,{?MODULE,?LINE,false,String_})).

%% Write the report at all known nodes.
-define(DIST_FATAL(String_),
	error_logger:error_report(fatal,{?MODULE,?LINE,true,String_})).
-define(DIST_ERROR(String_),
	error_logger:error_report(error,{?MODULE,?LINE,true,String_})).
-define(DIST_INFO(String_),
	error_logger:info_report(info,{?MODULE,?LINE,true,String_})).

%% Format a string to be used by the macros above.
-define(F(Format_,Arguments_),
	lists:flatten(io_lib:format(Format_,Arguments_))).
