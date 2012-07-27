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

%%% File    : http_fields.erl
%%% Author  : Tony Rogvall <tony@erix.ericsson.se>
%%% Purpose : HTTP/1.1 fields (RFC 2068)
%%% Created :  7 Aug 1998 by Tony Rogvall <tony@erix.ericsson.se>

-module(http_fields).
-author('tony@erix.ericsson.se').
-modified_by('jon@serc.rmit.edu.au').

-export([load/1, unload/1, get_header_value/2,get_cookie_value/2]).
-import(lists, [map/2]).
-import(ets, [insert/2, delete/2]).

load(Tab) ->
    map(fun({Name, Fun}) -> insert(Tab, {{http,field,Name}, [request,response], Fun}) end, general_header()),
    map(fun({Name,Fun}) -> insert(Tab, {{http,field,Name}, [request,response], Fun})	end, entity_header()),
    map(fun({Name,Fun}) -> insert(Tab, {{http,field,Name}, [request], Fun}) end, request_header()),
    map(fun({Name,Fun}) -> insert(Tab, {{http,field,Name}, [response], Fun}) end, response_header()),
    map(fun({Name,Fun}) -> insert(Tab, {{http,method,Name}, [method], Fun}) end, methods()),
    map(fun({Code, String}) -> insert(Tab, {{http,status,Code}, [status], String}) end, status_code()),
    ok.

unload(Tab) ->
    map(fun({Name,_}) -> delete(Tab,{http,field,Name}) end, general_header()),
    map(fun({Name,_}) -> delete(Tab,{http,field,Name}) end, entity_header()),
    map(fun({Name,_}) -> delete(Tab,{http,field,Name}) end, request_header()),
    map(fun({Name,_}) -> delete(Tab,{http,field,Name}) end, response_header()),
    map(fun({Name,_}) -> delete(Tab,{http,method,Name}) end, methods()),
    map(fun({Code,_}) -> delete(Tab, {http,status,Code}) end, status_code()),
    ok.


%% methods functions have are named m_<methd>
methods() ->
    [
     { "options", fun m_options/1 },
     { "get", fun m_get/1 },
     { "head", fun m_head/1 },
     { "post", fun m_post/1 },
     { "put", fun m_put/1 },
     { "delete", fun m_delete/1 },
     { "trace", fun m_trace/1 } ].  %% extensio-methods ???

status_code() ->
    [ 
      { "100",   "Continue"},
      { "101",   "Switching Protocols" },
      { "200",   "OK" },
      { "201",   "Created" },
      { "202",   "Accepted" },
      { "203",   "Non-Authoritative Information" },
      { "204",   "No Content" },
      { "205",   "Reset Content" },
      { "206",   "Partial Content" },
      { "300",   "Multiple Choices" },
      { "301",   "Moved Permanently" },
      { "302",   "Moved Temporarily" },
      { "303",   "See Other" },
      { "304",   "Not Modified" },
      { "305",   "Use Proxy" },
      { "400",   "Bad Request" },
      { "401",   "Unauthorized" },
      { "402",   "Payment Required" },
      { "403",   "Forbidden" },
      { "404",   "Not Found" },
      { "405",   "Method Not Allowed" },
      { "406",   "Not Acceptable" },
      { "407",   "Proxy Authentication Required" },
      { "408",   "Request Time-out" },
      { "409",   "Conflict" },
      { "410",   "Gone" },
      { "411",   "Length Required" },
      { "412",   "Precondition Failed" },
      { "413",   "Request Entity Too Large" },
      { "414",   "Request-URI Too Large" },
      { "415",   "Unsupported Media Type" },
      { "500",   "Internal Server Error" },
      { "501",   "Not Implemented" },
      { "502",   "Bad Gateway" },
      { "503",   "Service Unavailable" },
      { "504",   "Gateway Time-out" },
      { "505",   "HTTP Version not supported" } ].

general_header() ->
    [ 
      { "cache-control", fun f_cache_control/2 },
      { "connection", fun f_connection/2 },      
      { "date", fun f_date/2 },                  
      { "pragma", fun f_pragma/2 },                
      { "transfer-encoding", fun f_transfer_encoding/2 },
      { "upgrade", fun f_upgrade/2 },               
      { "via", fun f_via/2 } ].

request_header() ->
    [
     { "accept", fun f_accept/2 },
     { "accept-charset", fun f_accept_charset/2 },
     { "accept-encoding", fun f_accept_encoding/2 },
     { "accept-language", fun f_accept_language/2 },
     { "authorization",  fun f_authorization/2 },
     { "from", fun f_from/2 },
     { "host", fun f_host/2 },
     { "if-modified-since", fun f_if_modified_since/2 },
     { "if-match", fun f_if_match/2 },
     { "if-none-match", fun f_if_none_match/2 },
     { "if-range", fun f_if_range/2 },
     { "if-unmodified-since", fun f_if_unmodified_since/2 },
     { "max-forwards", fun f_max_forwards/2 },
     { "proxy-authorization", fun f_proxy_authorization/2 },
     { "range", fun f_range/2 },
     { "referer", fun f_referer/2 },
     { "user-agent", fun f_user_agent/2 },
     { "x-forwarded-for", fun f_x_forwarded_for/2 } ].

response_header() ->
    [
     { "age", fun f_age/2 },
     { "location", fun f_location/2 },
     { "proxy-authenticate", fun f_proxy_authenticate/2 },
     { "public", fun f_public/2 },
     { "retry-after", fun f_retry_after/2 },
     { "server", fun f_server/2 },
     { "vary", fun f_vary/2 },
     { "warning", fun f_warning/2 },
     { "www-authenticate", fun f_www_authenicate/2 } ].


entity_header() ->
    [ 
      { "allow", fun f_allow/2 },
      { "content-base", fun f_content_base/2 },
      { "content-encoding", fun f_content_encoding/2 },
      { "content-language", fun f_content_language/2 },
      { "content-length", fun f_content_length/2 },
      { "content-location", fun f_content_location/2 },
      { "content-md5", fun f_content_md5/2 },
      { "content-range", fun f_content_range/2 },
      { "content-type", fun f_content_type/2 },
      { "etag", fun f_etag/2 },
      { "expires", fun f_expires/2 },
      { "last-modified", fun f_last_modified/2 } ].

    
%%----------------------------------------------------------------------------
%%
%% method parser/format 
%% general functions
%%   m_<method>(decode) -> method;
%%   m_<method>(encode) -> "Method".
%%
%%----------------------------------------------------------------------------


m_options(decode) -> options;
m_options(encode) -> "OPTIONS".

m_get(decode) -> get;
m_get(encode) -> "GET".

m_head(decode) -> head;
m_head(encode) -> "HEAD".

m_post(decode) -> post;
m_post(encode) -> "POST".

m_put(decode) -> put;
m_put(encode) -> "PUT".

m_delete(decode) -> delete;
m_delete(encode) -> "DELETE".

m_trace(decode) -> trace;
m_trace(encode) -> "TRACE".

     
%%----------------------------------------------------------------------------
%%
%% field parser/format 
%% general functions
%%   f_<field>(decode, Value) ->
%%         {<field>, parse_type(Value) };
%%   f_<field>(encode, Value) ->
%%         "Field: " ++ format_type(Value).
%%
%%----------------------------------------------------------------------------
	       
f_cache_control(decode, Value) -> { 'cache-control', Value};
f_cache_control(encode, Value) -> "Cache-Control: " ++ Value.

f_connection(decode, Value) -> { connection, Value};
f_connection(encode, Value) -> "Connection: " ++ Value.

f_date(decode, Value) -> 
    case srv_parse:dec_date(Value) of
	{error,_} -> {error,{ date, Value }};
	{Date,_} -> {date, Date}
    end;
f_date(encode, Value) -> "Date: " ++ srv_parse:enc_date(Value).

f_pragma(decode, Value) -> { pragma, Value};
f_pragma(encode, Value) -> "Pragma: " ++ Value.

f_transfer_encoding(decode, Value) -> { 'transfer-encoding', Value};
f_transfer_encoding(encode, Value) -> "Transfer-Encoding: " ++ Value.

f_upgrade(decode, Value) -> { upgrade, Value};
f_upgrade(encode, Value) -> "Upgrade: " ++ Value.

f_via(decode, Value) -> { via, Value};
f_via(encode, Value) -> "Via: " ++ Value.

f_accept(decode, Value) -> { accept, Value};
f_accept(encode, Value) -> "Accept: " ++ Value.

f_accept_charset(decode, Value) -> { 'accept-charset', Value};
f_accept_charset(encode, Value) -> "Accept-Charset: " ++ Value.

f_accept_encoding(decode, Value) -> { 'accept-encoding', Value};
f_accept_encoding(encode, Value) -> "Accept-Encoding: " ++ Value.

f_accept_language(decode, Value) -> { 'accept-language', Value};
f_accept_language(encode, Value) -> "Accept-Language: " ++ Value.

f_authorization(decode, Value) -> { authorization, Value};
f_authorization(encode, Value) -> "Authorization: " ++ Value.

f_from(decode, Value) -> { from, Value};
f_from(encode, Value) -> "From: " ++ Value.

f_host(decode, Value) -> { host, Value};
f_host(encode, Value) -> "Host: " ++ Value.

f_if_modified_since(decode, Value) -> { 'if-modified-since', Value};
f_if_modified_since(encode, Value) -> "If-Modified-Since: " ++ Value.

f_if_match(decode, Value) -> { 'if-match', Value};
f_if_match(encode, Value) -> "If-Match: " ++ Value.

f_if_none_match(decode, Value) -> { 'if-none-match', Value};
f_if_none_match(encode, Value) -> "If-None-Match: " ++ Value.

f_if_range(decode, Value) -> { 'if-range', Value};
f_if_range(encode, Value) -> "If-Range: " ++ Value.

f_if_unmodified_since(decode, Value) -> { 'if-unmodified-since', Value};
f_if_unmodified_since(encode, Value) -> "If-Unmodified-Since: " ++ Value.

f_max_forwards(decode, Value) -> { 'max-forwards', Value};
f_max_forwards(encode, Value) -> "Max-Forwards: " ++ Value.

f_proxy_authorization(decode, Value) -> { 'proxy-authorization', Value};
f_proxy_authorization(encode, Value) -> "Proxy-Authorization: " ++ Value.

f_range(decode, Value) -> { range, Value};
f_range(encode, Value) -> "Range: " ++ Value.

f_referer(decode, Value) -> { referer, Value};
f_referer(encode, Value) -> "Referer: " ++ Value.

f_user_agent(decode, Value) -> { 'user-agent', Value};
f_user_agent(encode, Value) -> "User-Agent: " ++ Value.

f_x_forwarded_for(decode, Value) -> { 'x-forwarded-for', Value};
f_x_forwarded_for(encode, Value) -> "X-Forwarded-For: " ++ Value.

f_age(decode, Value) -> { age, Value};
f_age(encode, Value) -> "Age: " ++ Value.

f_location(decode, Value) -> { location, Value};
f_location(encode, Value) -> "Location: " ++ Value.

f_proxy_authenticate(decode, Value) -> { 'proxy-authenticate', Value};
f_proxy_authenticate(encode, Value) -> "Proxy-Authenticate: " ++ Value.

f_public(decode, Value) -> { public, Value};
f_public(encode, Value) -> "Public: " ++ Value.

f_retry_after(decode, Value) -> { 'retry-after', Value};
f_retry_after(encode, Value) -> "Retry-After: " ++ Value.

f_server(decode, Value) -> { server, Value};
f_server(encode, Value) -> "Server: " ++ Value.

f_vary(decode, Value) -> { vary, Value};
f_vary(encode, Value) -> "Vary: " ++ Value.

f_warning(decode, Value) -> { warning, Value};
f_warning(encode, Value) -> "Warning: " ++ Value.

f_www_authenicate(decode, Value) -> { 'www-authenicate', Value};
f_www_authenicate(encode, Value) -> "WWW-Authenicate: " ++ Value.

f_allow(decode, Value) -> { allow, Value};
f_allow(encode, Value) -> "Allow: " ++ Value.

f_content_base(decode, Value) -> { 'content-base', Value};
f_content_base(encode, Value) -> "Content-Base: " ++ Value.

f_content_encoding(decode, Value) -> { 'content-encoding', Value};
f_content_encoding(encode, Value) -> "Content-Encoding: " ++ Value.

f_content_language(decode, Value) -> { 'content-language', Value};
f_content_language(encode, Value) -> "Content-Language: " ++ Value.

f_content_length(decode, Value) ->
    { N, _} = srv_parse:integer_field(Value),
    { 'content-length', N };
f_content_length(encode, Value) -> "Content-Length: " ++ integer_to_list(Value).

f_content_location(decode, Value) -> { 'content-location', Value};
f_content_location(encode, Value) -> "Content-Location: " ++ Value.

f_content_md5(decode, Value) -> { 'content-md5', Value};
f_content_md5(encode, Value) -> "Content-MD5: " ++ Value.

f_content_range(decode, Value) -> { 'content-range', Value};
f_content_range(encode, Value) -> "Content-Range: " ++ Value.

f_content_type(decode, Value) -> { 'content-type', Value};
f_content_type(encode, Value) -> "Content-Type: " ++ Value.

f_etag(decode, Value) -> { etag, Value};
f_etag(encode, Value) -> "ETag: " ++ Value.

f_expires(decode, Value) -> 
    case catch srv_parse:integer_field(Value) of
	{'EXIT', _} ->
	    case srv_parse:dec_date(Value) of
		{Date,_} ->
		    { expires, Date};
		_ ->
		    { error, {expires,Value}}
	    end;
	{Sec,_} ->
	    {expires, Sec}
    end;
f_expires(encode, Value) -> 
    if integer(Value) ->
	    "Expires: " ++ integer_to_list(Value);
       true ->
	    "Expires: " ++ srv_parse:enc_date(Value)
    end.

f_last_modified(decode, Value) -> { 'last-modified', Value};
f_last_modified(encode, Value) -> "Last-Modified: " ++ Value.


%% Get the value of a header, if it exists among the parsed headers of
%% a request
get_header_value(Field,Parsed_headers) ->
    case lists:keysearch(Field,1,Parsed_headers) of
	{value,{_,Value}} ->
	    Value;
	_->
	    not_found
    end.


%% Given a list representing the cookie field in the HTTP header,
%% return the value of the cookie that matches Cookie_name.
%%
get_cookie_value(Cookie_name, [Cookie | Rest]) ->
    Cookies_nv = string:tokens(Cookie, " 	;"),
    case get_cookie_value_nv(Cookie_name, Cookies_nv) of
	not_found ->
	    get_cookie_value(Cookie_name, Rest);
	Value ->
	    Value
    end;
get_cookie_value(_, []) ->
    not_found.
    
%% Given a list of strings of the form "Name=Value",
%% return Value if the Name matches Cookie_name.
%%
get_cookie_value_nv(Cookie_name, [Nv | Rest]) ->
    case string:tokens(Nv, " 	=\"") of
	[Cookie_name, Value] ->
	    Value;
	_ ->
	    get_cookie_value_nv(Cookie_name, Rest)
    end;
get_cookie_value_nv(_, []) ->
    not_found.
