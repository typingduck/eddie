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

%%% File    : dns.hrl
%%% Author  : Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Purpose : 
%%% Created :  1 Oct 1998 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%% Modified: 22 Feb 2000 by Magnus Fr|berg <magnus@erix.ericsson.se>
%%%
%%% Added ?IS_IP, #pend

%%%             ***Warning***
%%% Some of the records here are cut and pasted directly from
%%% the inet_dns.hrl file, do not change thoose, bad things will
%%% happen... If problems arise with new version of Erlang, check 
%%% here first. Fix in future! Yuck! /pekka
%%%             ***Warning***

%% ---------------------------------------------------
%% options directive: constructor and access macros.
%% ---------------------------------------------------

%% used both for current pending requests and prev_queries 
%% dns_query needs it here for dns_recurse start
-record(pend, {
	  pid,
	  domain,           %% 
	  answ = [],        %% #dns_req to fill in if cname, 
	  hints = false }). %% otherwise empty


-record(options, {
	  norec = false,      % no-recursion
	  forward = false,    % forward-only
	  noglue = false,     % no-fetch-glue,
	  qlog = false,       % query-log
	  fiq = false         % fake-iquery
	 }).

-define(CREATE_OPTS(Options),
	F =
	fun(norec, OAcc)   -> OAcc#options{norec = true};
	   (forward, OAcc) -> OAcc#options{forward = true};
	   (noglue, OAcc)  -> OAcc#options{noglue = true};
	   (qlog, OAcc)    -> OAcc#options{qlog = true};
	   (fiq, OAcc)     -> OAcc#options{fiq = true}
	end,
	lists:foldl(F, #options{}, Options)).

%% Use as a "guards".
-define(IS_IP(MYIPZZ),
	case MYIPZZ of
	    {AZZ,BZZ,CZZ,DZZ} when integer(AZZ+BZZ+CZZ+DZZ) -> true;
	    {AZZ,BZZ,CZZ,DZZ,EZZ,FZZ,GZZ,HZZ} when
		  integer(AZZ+BZZ+CZZ+DZZ+EZZ+FZZ+GZZ+HZZ) -> true;
	    _ -> false
	end).

-define(RECURSION_AVAIL(Opts), Opts#options.norec == false).

-define(REC_AVAIL(Opts),
	if
	    Opts#options.norec == false -> 1;
	    true                        -> 0
	end).

%% Use in guards.
-define(FORWARD_ONLY(Opts), Opts#options.forward == true).

%% ---------------------------------------------------
%% Used DNS definitions.
%% ---------------------------------------------------

-define(NAMESERVER_PORT, 53).

%% opcodes
%%
-define(QUERY, 16#0).

%% Response codes
%%
-define(NOERROR,  0).
-define(FORMERR,  1).
-define(SERVFAIL, 2).
-define(NXDOMAIN, 3).
-define(NOTIMP,	  4).
-define(REFUSED,  5).

%% Symbolic Type values for resources and queries
%%
-define(S_A,	 a).         %% host address
-define(S_NS,	 ns).        %% authoritative server
-define(S_MD,	 md).        %% mail destination
-define(S_MF,	 mf).        %% mail forwarder
-define(S_CNAME, cname).     %% connonical name
-define(S_SOA,	 soa).       %% start of authority zone
-define(S_MB,	 mb).        %% mailbox domain name
-define(S_MG,	 mg).        %% mail group member
-define(S_MR,	 mr).        %% mail rename name
-define(S_NULL,	 null).      %% null resource record
-define(S_WKS,	 wks).       %% well known service
-define(S_PTR,	 ptr).       %% domain name pointer
-define(S_HINFO, hinfo).     %% host information
-define(S_MINFO, minfo).     %% mailbox information
-define(S_MX,	 mx).        %% mail routing information
-define(S_TXT,	 txt).       %% text strings
-define(S_AAAA,  aaaa).      %% ipv6 address
-define(S_SRV,   srv).       %% services (RFC 2052)
-define(S_AXFR,	 axfr).      %% transfer zone of authority
-define(S_ANY,	 any).       %% wildcard match

-record(dns_header, 
	{
	 id = 0,       %% ushort query identification number 
	 %% byte F0
	 qr = 0,       %% :1   response flag
	 opcode = 0,   %% :4   purpose of message
	 aa = 0,       %% :1   authoritive answer
	 tc = 0,       %% :1   truncated message
	 rd = 0,       %% :1   recursion desired 
	 %% byte F1
	 ra = 0,       %% :1   recursion available
	 pr = 0,       %% :1   primary server required (non standard)
	               %% :2   unused bits
	 rcode = 0     %% :4   response code
	}).

-record(dns_rec,
	{
	 header,       %% dns_header record
	 qdlist = [],  %% list of question entries
	 anlist = [],  %% list of answer entries
	 nslist = [],  %% list of authority entries
	 arlist = []   %% list of resource entries
	}).

%% DNS resource record
-record(dns_rr,
	{
	 domain,        %% resource domain
	 class,         %% reource class (atom i.e c_in)
	 type,          %% resource type (atom i.e t_any)
	 cnt = 0,       %% access count
	 tm,            %% creation time
	 ttl,           %% time to live
         bm = [],       %% Bitmap storing domain character case information.
	 data = [],     %% raw data
         func = false   %% Optional function calculating the data field.
	}).

-record(dns_query,
	{
	  domain,     %% query domain
	  type ,    %% query type
	  class       %% query class
	 }).

