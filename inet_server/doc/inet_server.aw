Inet server, APIs and considerations. Draft. Patrik Winroth, 980329. 
--------------------------------------------------------------------
(this text is to be made a tiny bit more readable using Applixware/Linux
ASA I get to install it).

Background
----------
The inet server is part of a project called Eddie. The name 'inet'
server refers to the inet family of protocols, see [1]. Eddie's
primary goal is to implement a highly robust, fault tolerant,
scalable, generally nice WEB-server architecture. To make parts
reusable, we first create a general inet server platform. This inet
server is then used when implementing the WEB server. The inet
server is used to implement servers using the inet TCP and/or UDP
mechanisms over ip for communication with clients. Example of
servers that can be implemented, besides HTTP 1.1,  are FTP and
DNS servers.

Some benefits with using the inet server and its associated API's
for implementation of a server is as follows:

- The inet server concept is a part of a bigger picture - the Eddie
  framework. This framework gives properities such as fault tolerance,
  scalability and load balancing.

- The implementor can concentrate his efforts on the server specific
  functionality, the rest is already in place.

- Using the same core code for many different inet servers gives
  a similar behavior for a whole portfolio of servers.

System flow
-----------
[nice illustration of the following here ...]

Client [X] --Request--> 
  [X] [Endpoint_N Ip_adress:Port] -> 
    Protocol_module:in ... [possible outcomes] ... Protocol_module:in -> 
       schedule [X] --Parsed request--> 
         [X] erlets [possible outcomes]                     

(Stress the lose coupling inbetween endpoint/scheduling and the
actual execution in erlets.)

Note specially that after parsing of requests, scheduling takes
place. This scheduling determines what host is going to handle the
parsed request. The scheduling is based on the different hosts
actual set of resources. This principle with endpoint and data
parsing separated from the actual execution of the request makes
it possible to:

1) Introduce redundancy by having a specific resource replicated,
and thus servicable, to many hosts.

2) Introduce scalability by having a specific resource further
replicated.

3) Introduce fault tolerance by having the inet server restart an
endpoint at an other host if the one currently publishing the
endpoint crashes. This is made possible with to the scheduling
that, in any case, looks to that the request for a resource is
handled by a host owning that resource. The degree of fault tolerance
can be determined by how many replicas of a resource is made on a
specific eddie system.

4) Introduce load balancing by executing the request on the host
with the lowest load, given that many hosts has the actual resource.

Abstractions and explanations
-----------------------------

The following abstractions are based on the Erlang programming
language, e.g. module refers to the an Erlang module. For a reference
on Erlang, consult [2].

'Inet server'
The inet server is in itself not a server, it is as a base application
for implementing servers. To implement a server using the inet
server you need to write a protocol module and, possibly, some
erlets.

'Protocol module'
A protocol module is written to implement a specific protocol, such
as HTTP 1.1. A protocol module exports the following functions:
server/0, in/2 and schedule_match/2

'Profile'
A profile is a specialisation of a protocol module. In other words,
it is a configuration of the protocol module for a specific use of
a protocol. This includes details such as: used inet port for the
different endpoints, content and scheduling of requests.

'Endpoint'
An endpoint is where data comes in from, and is sent back to clients.
AN enpoint uses sockets of type tcp and udp for its communication
with clients. A protocol module specifies what endpoints it uses
in the return value from the server/0 function. An endpoint is not
tied to a specific computer. Typically an endpoint can be started
on the same computer as the server itself is located. If this
computer crashed the endpoint is moved to an other computer, thus
ensuring that published endpoints are kept alive in the clients
point of view. As two examples, HTTP uses one endpoint of type TCP;
DNS uses one TCP endpoint and one UDP endpoint.

'in function'
A protocol module exports the function in/2. This is where data
from the endpoints are parsed. The in function can communicate
directly or indirectly with the clients.

'Server function'
A protocol module exports the function server/0. This is where the
servers endpoints are specified and given default configurations.

'schedule_match'
A protocol module exports the function schedule_match/2. This
function decides whether a certain profile can service a certain
request.

'Erlet'
An Erlet is a module that is intended to extend a protocol modules
functionality. Typically many erlets are used together with one
protocol module. In the WEB server example, the protocol module
function in, parses the incoming requests. The erlets take care of
the actual communication with the client. There are erlets for e.g.
authorisation, cgi script execution, aliasing and server side
inculdes. This is in the WEB server example, one could let the in
function take care of all parsing and communication to.

An erlet exports load/, store/, do_pre/, do_data/, do_post/.

Details of writing a protocol module
------------------------------------
A protocol module exports a number of functions. These are server/0,
in/2 and schedule_match/2.
 
The in/2 function is called from the with the inet server, and
communicates with it via a simple protocol. This communications is
done using the following principles/methods. Some examples are
given with HTTP1.1 as protocol.

A client connects to the inet server. The inet server reads all
data the client has sent and calls the in function.
(http11:in(Database_handle, Received_data). The in function parses
Received_data, may perform updates to the database, and returns a
job list. This list of jobs is executed sequentially, starting with
the first element in the list, one job at a time.

A job can be one of: 

{send, Data} - Data is sent on this instance of the endpoint.

{send, Data, Ip_address, Port} - Data is sent using a fresh udp
socket.

{send, Data} sends Data back to the client. This is true for UDP
and TCP both. In the TCP case this, of course, means that data is
sent back on the accepted socket. In the UDP case this means that
Data is sent back using the very same socket that the Datagram was
received on, this is a feature required by a DNS server. If you,
in the UDP case, wish to send data back using a freshly opened UDP
socket you can use {send, Datagram, Ip_address, Port}.

close - This instance of the endpoint is closed.

{read, Number_of_bytes, Timeout} - Only applies to tcp, number_of_bytes
data is read from the endpoint. Timeout is given as the time in
milliseconds to wait until the read is to timeout. The result from
this read is supplied as argument to yet a new call to http11:in,
where a timeout is indicated by {error, timeout}.

[{Schedule_pattern0, Parsed_request0}, .., {Schedule_patternN,
Parsed_requestN}] - Parsed requests to be run through a sequence
of erlets. This sequence of erlets may, or may not, be executed on
the same host as the in function was executed on. Which host is
going to be used is determined by the schedule_match/2 function
together with the load balancing algorithm.


The database is initialized with (the set of initalized data is
going to grow)

* connections - mandatory, number of connections currently running
  that has been spawned from this endpoint.

* peer_ip - mandatory, the IP address of the peer.

* peer_port - mandatory, the port of the peer.

* endpoint_name - mandatory, the name of the endpoint where this
  data was received.

Database access functions:
gis_thread_database:
lookup(Database_handle, Key) -> List_of_found_elements
insert(Database_handle, {Key, Value}) -> true  

The schedule_match(Pattern, Patterns) function is called from the
inet server after the message from the client has been parsed. What
it does is, simply, returning true or false. True if Pattern is
one of Patterns, false if not. In the case of HTTP1.1 this is simply
a matter of pattern matching.

Details of writing an erlet.
----------------------------
load and store are used to initalize at start-up. every erlet has
its own database. {more to write. explain erlet -> erlet.hrl which
defines mnesia table.}
 
do_pre, do_data, and do_post are functions that are exported by
the erlet. This is directly coupled to the three erlet phases that
the inet server maintains; the pre phase, the data phase and the
post phase. The idea is that one erlet is of one and only one type.
For example an authorization erlet is of pre type, it thus exports
do_pre. An Erlet returns a job list to the inet server. Erlets of
different phases can return different elements in this job list,
according to the following figure

[Figure maming this clear:]
erlet type exports returns job list containing
---------- ------- ---------------------------
pre        do_pre  [{send, Data} | {send, Data, Ip_address, Port}] [next_erlet | do_data | do_post | end] 
data       do_data [{send, Data} | {send, Data, Ip_address, Port}] [next_erlet | do_post | end]
post       do_post [{send, Data} | {send, Data, Ip_address, Port}] [next_erlet | end]

(Note: there is, currently, nothing stopping an Erlet writer
exporting more than one of [do_pre, do_data, do_post], but we
discourage doing that)

for an explanation of the {send, Data} and {send, Data, Ip_address,
Port} jobs, see 'Details of writing a protocol module' above.

As an example; a call of erlet_auth:do_pre(Database_handle,
Received_data) may, in the case of failed authorization, return
the job list [{send, Fail_Message}, do_post]. That would mean:
first send Fail_message to the client and then execute the erlet
post phase. (The post phase typically handles logging).

Details of writing a protocol profile
-------------------------------------
A protocol profile is the configuration of a protocol module. This
includes many details. The idea is that what, for the client, looks
like ONE web server, may be many, differently configured, back end
servers.

Example of the first lines of a protocol profile for http 1.0,
http_10_bin_cgi.cfg.

# Protocol Module is the module that implements the protocol specific parts of
# the server.
ProtocolModule http10

# Endpoints
Endpoint tcp_socket 80

# Setup of replication and scheduling
# Server bases
SERVERBASE_A /ldisk/server_base_A
SERVERBASE_B /ldisk/server_base_B

# Replication
Replica http://otp.ericsson.se/bin-cgi/* /bin-cgi
Replica http://opt.ericsson.se/icons/* /icons	

# Scheduling
Schedule bin-cgi/
Schedule icons/	
Schedule pix/	

# Erlets: Protocol module run-time plug-in erlets written using the Erlang
# INET Server API (EISAPI). The INET server API makes it easy to
  add functionality

# to a server. Read more about EISAPI in the Reference Manual.
Erlets erlet_alias erlet_auth erlet_auth_mnesia erlet_esi erlet_actions
erlet_cgi erlet_include erlet_dir erlet_get erlet_head erlet_log
erlet_disk_log

Notes:
1) The semantics of the Schedule directive is determined by the
protocol module implementor. The protocol_module:schedule_match/2
functions governs the semantics.

2) There is currently no explicit connection inbetween the schedule
directive and the different alias directives of the erlet erlet_alias.
There may be an explicit connection in the future, though.

[1] UNIX network programming W. Richard Stevens, ISBN 0-13-949876-1.
[2] Concurrent Programming in Erlang, Joe Armstrong et al, ISBN 0-13-508301-X
