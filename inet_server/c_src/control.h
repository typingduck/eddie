#ifndef CONTROL_H
#define CONTROL_H

#include "server.h"

typedef unsigned short u16;
typedef unsigned long u32;

/* packet command codes */
#define PAK_REQUEST 1
#define PAK_REDIRECT 2
#define PAK_MESSAGE 3
#define PAK_BE_ERROR 4

/* packet header and data offsets */
#define PAK_LEN_OFF 0
#define PAK_COM_OFF 4
#define PAK_ID_OFF 5
#define PAK_DATA_OFF 9

int make_request_packet(Server *serv, int fd, int num_chars);
int make_backend_error_packet(Server *serv, int fd, u32 ip, u16 nbo_port);

#endif /* CONTROL_H */

/*
    The standard packet header:

    	length (4 byte int, network byte order) (includes length of the header)
	command (1 byte int)
	unique id (4 byte int, network byte order)

    Currently, there are 4 commands defined:

	PAK_REQUEST (child -> controller)

	    9 byte header
	    client IP (4 bytes)
	    listening IP (4 bytes)
	    listening port (2 byte int, network byte order)
	    http headers: name/value pairs, stored as an even number of
			    null-terminated strings

	PAK_REDIRECT (controller -> child)

	    9 byte header
	    redirect IP (4 bytes)
	    redirect port (2 byte int, network byte order)

	PAK_MESSAGE (controller -> child)

	    9 byte header
	    message string (null-terminated string)

	PAK_BE_ERROR (child -> controller)

	    9 byte header
	    listening IP (4 bytes)
	    listening port (2 byte int, network byte order)
	    redirect (failed) IP (4 bytes)
	    redirect (failed) port (2 byte int, network byte order)
	    client IP (4 bytes)

*/

/*
    lifecycle of a query:
    1. The query comes in, and gets buffered in the client's queue until we
	get a complete header.  From here, any further data recieved from
	the client is appended to the client's queue.
    2. send a PAK_REQUEST off, get a PAK_REDIRECT or PAK_MESSAGE back
    3. if PAK_REDIRECT, start connection to the backend and continue to 4.
	If PAK_MESSAGE, just send the message and close the connection to
	the client.
    4. when the connection is established, move everything to the backend's
	queue (plus, add a "X-Forwarded-For" field to the headers).  Flush
	the query to the backend (i.e. set it to output).  If the query has
	content-length, ensure that only that much data is sent to the
	backend - anything else will remain buffered for the moment.  Any
	further data from the client also goes to the backend's queue.
    5. buffer the reply from the backend in the client's queue until we
	get a complete header.
    6. Once we have the complete header of the backend's reply, start
	flushing the client's queue to the client.
    7. Once we've seen content-length chars go past, examine the backend's
	queue for any data.  If there's anything there, move it over to the
	client's queue.
    8. Once we get another complete query, send off a PAK_REQUEST.  When we
	get back a PAK_REDIRECT, compare the backend with the current
	connection.  If they're equal, go to state 4.  Otherwise, close the
	backend connection, and go to state 3.

*/
