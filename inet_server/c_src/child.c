#include <sys/types.h>
#include <signal.h>
#include <limits.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <ctype.h>
#include <syslog.h>
#include <sys/stat.h>
#include <unistd.h>

#include "backend.h"
#include "server.h"
#include "control.h"
#include "main.h"
#include "param.h"

/* Must be defined in an external module linked to this */
extern  void init_child();
extern  void close_child(Server *, int);
extern  void init_protocol(int);
extern  void init_connection(Server *, int);
extern  int handle_pre_backend(Server *, int);
extern  int protocol_write(Server *, int, int, int *);
extern  int protocol_read(Server *, int, int *, int, int);

typedef enum
{
    idle_persist, idle_timeout_die, idle_immediate_die
} Idle_Behaviour;

Idle_Behaviour die_when_idle;

typedef enum
{
    header_unfinished = -1, header_malformed = -2, header_empty = -3
} Header_State;

typedef enum
{
    transfer_normal, transfer_chunked
} Transfer_Encoding;

void shutdown_child_client(Server *me, int fd)
{

#ifdef DEBUG
    fprintf(stderr,"Shutting down (child) client, fd = %d\n", fd); 
#endif
    close_child(me, fd);
    close(fd);
    FD_CLR(fd, &me->except_fds);
    reset_server_read_fd(me, fd);
    reset_server_write_fd(me, fd);
    if (me->writebuf.num_chars[fd] >= 0)
    {
#ifdef DEBUG
            fprintf(stderr,"discarding pending output of %d chars\n", me->writebuf.num_chars[fd]); 
#endif
        delete_data(me, fd);
    }
    me->fd_state[fd] = fs_pre_backend;


    /* for ugly alarm handling set the port to 0 when it's deleted */ 
    me->fd_connection_port[fd] = 0;
}

void delete_client(Server *me, int fd)
{
    //static struct timeval my_timeout; 
    int free_start, free_end;

    // deleting a client in waiting to connect state so 
    // decrement the total_pending connects
    if (me->fd_state[fd] == fs_pending_connect)
    {
      syslog(LOG_INFO, "(child %d) decrenting total_pending_connects", getpid()); 
      me->total_pending_connects--; 
    }  // if (serv->fd_state[fd] ...

    /* shutdown the clients */
    shutdown_child_client(me, fd);
    if (me->fd_partner[fd] >= 0)
    {
        shutdown_child_client(me, me->fd_partner[fd]);
        me->fd_partner[me->fd_partner[fd]] = -1;
    }
    me->fd_partner[fd] = -1;
#ifdef DEBUG
    fprintf(stderr,"(child) client now has %d connections\n", me->num_connections - 1); 
#endif
    me->num_connections--;
    if (me->num_connections == 0 && die_when_idle != idle_persist)
    {
        if (die_when_idle == idle_immediate_die)
            _exit(0);

        /* schedule the select to timeout - this will kill this
            * child off if it happens. */
        me->glob_timeout = &(me->timeout);  /* use this timeout, 15/8/2000 */

    //syslog(LOG_INFO, "(child.c) pid %d scheduled itself to die ", getpid()); 



#ifdef DEBUG
        fprintf(stderr,"child %ld scheduled itself to suicide in %ld seconds\n", getpid(), me->glob_timeout->tv_sec); 
#endif
    }
    if (me->num_connections < me->max_connections && me->busy)
    {
        /* tell parent we're available to do more now */
        append_data(me, me->parent_fd, &free_start, &free_end, fs_output);
        if (add_data(me, me->parent_fd, "f", 0, 1, fs_output))
            /* succeeded in telling parent we're available - mark
                * ourselves non-busy */
            me->busy = 0;
        else
            /* try to tell the parent that we're available after we
                * flush whatever's pending */
            me->busy = -1;
#ifdef DEBUG
        fprintf(stderr,"child is no longer maxed out\n"); 
#endif
    }
}

/* 
 * Name: header_length
 * Purpose: examines the header of an HTTP query (`query` == 1) or response
 * 		('query' == 0), stored in 'string' of length 'num_chars'.
 *	return header_unfinished if header is unfinished, 
 *      return header_malformed if the header is malformed
 *	return the header length otherwise
 *	if non-NULL,
 *	    'version' will be set to 9 for http 0.9, 10 for 1.0, 11 for 1.1.
 *	    'keepalive' will be set true or false, depending on the default for
 *		    the version and the presence of Keepalive: headers.
 *	    'content_length' will be set to the value of any Content_Length:
 *		    headers, or -1 if none are present.
 *	    'transfer_encoding' will be set to the value of any
 *		    Transfer-Encoding: headers, or transfer_normal if none
 *		    are present.
 *	    'status' will be set to the reply status if 'query' == 0.
 */
int header_length(Server *me, char *string, int num_chars, int query,
	int *version, int *keepalive, int *content_length,
	Transfer_Encoding *transfer_encoding, int *status)
{
    int pos;
    int end;
    int vers = 9;
    int just_after_eol = 0;
    char *header;

    /* sigh - skip any CRLF that might come first */
    for (pos = 0; pos < num_chars &&
            (string[pos] == '\r' || string[pos] == '\n'); pos++)
        ;
    if (pos == num_chars)
        return header_empty;

    if (query == 0)
    {
	/* format of reply header:
	 *	HTTP/X.X ret_code reason\r\n(headers)\r\n\r\n
	 *	For version 0.9, no headers are returned */
	/* Look for the start of a reply header */
	if (!strncmp(&string[pos], "HTTP/1.0 ", 9))
	    vers = 10;
	else if (!strncmp(&string[pos], "HTTP/1.1 ", 9))
	    vers = 11;
	else
	{
	    /* assume it's a version 0.9 reply with no headers */
	    if (version)
		*version = 9;
	    if (keepalive)
		*keepalive = 0;
	    if (content_length)
		*content_length = -1;
	    return 0;
	}
	*status = -1;
	/* look for CONTINUE for two-pass POSTs */
	if (vers == 11 && status != NULL) {
            char *num_start, *num_end;
	    *status = -1;
	    for (; pos < num_chars && 
		   string[pos] != '\r' &&
		   string[pos] != '\n' &&
		   string[pos] != ' '; pos++) 
	    	; /* find space */
	    if (string[pos] != ' ') goto next; /* break if spc not found */
	    while(string[pos] == ' ') pos++; /* find next char */
	    num_start = &string[pos];
            for (; pos < num_chars && 
                   string[pos] != '\r' &&
                   string[pos] != '\n' &&
                   string[pos] != ' '; pos++) 
                ; /* find following space */
            if (string[pos] != ' ') goto next; /* break if spc not found */
	    errno=0;
	    *status = strtol(num_start, &num_end, 10);
	    if (errno == ERANGE || 
		num_end != &string[pos]) *status = -1;
next:
	    if (*status == -1)
	  	syslog(LOG_INFO, "unable to find status in 1.1 reply header");
	}
	/* ok - just skip the rest of the status line and get to the
	 * headers */
	for (; pos < num_chars && string[pos] != '\r' &&
		string[pos] != '\n'; pos++)
	    ;
	if (pos == num_chars)
	    return header_unfinished;
    }
    else
    {
	/* format of query header: command uri [version]\r\n(headers)\r\n\r\n */
	/* locate the first space */
	for (; pos < num_chars && string[pos] != ' ' &&
		string[pos] != '\r' && string[pos] != '\n'; pos++)
	    ;
	if (pos == num_chars)
	    return header_unfinished;
	else if (string[pos] != ' ')
	    return header_malformed;
	/* ok - now the (possibly absent) 2nd space */
	for (pos++; pos < num_chars && string[pos] == ' '; pos++)
	    ;
	for (; pos < num_chars && string[pos] != ' ' &&
		string[pos] != '\r' && string[pos] != '\n'; pos++)
	    ;
	if (pos == num_chars)
	    return header_unfinished;
	if (string[pos] == ' ')
	{
	    /* ok - we seem to have something > HTTP/0.9 */
	    /* skip these spaces */
	    for (pos++; pos < num_chars && string[pos] == ' '; pos++)
		;
	    /* locate the end of the line */
	    for (end = pos; end < num_chars && string[end] != '\r' &&
		    string[end] != '\n'; end++)
		;
	    if (end == num_chars)
		return header_unfinished;

	    /* find HTTP/1.0 or HTTP/1.1 */
	    if (pos + 8 != end)
		return header_malformed;
	    else if (!strncmp(&string[pos], "HTTP/1.0", 8))
		vers = 10;
	    else if (!strncmp(&string[pos], "HTTP/1.1", 8))
		vers = 11;
	    else
		return header_malformed;
	    pos = end;
	}
	else if (string[pos] == '\r' || string[pos] == '\n')
	{
	    vers = 9;
	    /* version 9 queries finish with a single CRLF, not two */
	    just_after_eol = 1;
	}
	else
	    return header_malformed;
    }
    if (version)
        *version = vers;
    if (keepalive)
        *keepalive = (vers == 11);
    if (content_length)
	*content_length = -1;
    if (transfer_encoding)
	*transfer_encoding = transfer_normal;
    /* now run through the headers, looking for a double \r\n.  We also
     * want to look out for specific headers:
     *		Connection
     *		Proxy-Connection
     *		Content-Length
     */
    while (pos < num_chars)
    {
        while (pos < num_chars && string[pos] != '\r' && string[pos] != '\n')
        {
            if (just_after_eol)
            {
                just_after_eol = 0;
		if (keepalive && (((header = "Connection:") &&
			strncasecmp(&string[pos], header, strlen(header)) == 0)
			|| ((header = "Proxy-Connection:") &&
			strncasecmp(&string[pos], header, strlen(header)) == 0)))
                {
                    pos += strlen(header);
                    while (pos < num_chars && string[pos] != '\r' &&
                            string[pos] != '\n')
                    {
                        if (strncasecmp(&string[pos], "Keep-Alive", 10) == 0)
                        {
                            *keepalive = 1;
                            pos += 10;        /* strlen("Keep-Alive") */
                            break;
                        }
                        else if (strncasecmp(&string[pos], "Close", 5) == 0)
                        {
                            *keepalive = 0;
                            pos += 5;        /* strlen("Close") */
                            break;
                        }
                        else
                            pos++;
                    }
                    pos--;
                }
		else if (content_length && (header = "Content-Length:") &&
			strncasecmp(&string[pos], header, strlen(header)) == 0)
		{
                    for (pos += strlen(header); pos < num_chars &&
			    string[pos] == ' '; pos++)
			;
		    *content_length = atoi(&string[pos]);
		}
		else if (transfer_encoding && (header = "Transfer-Encoding:") &&
			strncasecmp(&string[pos], header, strlen(header)) == 0)
		{
                    pos += strlen(header);
                    while (pos < num_chars && string[pos] != '\r' &&
                            string[pos] != '\n')
                    {
                        if (strncasecmp(&string[pos], "Chunked", 7) == 0)
                        {
                            *transfer_encoding = transfer_chunked;
                            pos += 7;        /* strlen("Chunked") */
                            break;
                        }
                        else
                            pos++;
                    }
                    pos--;
		}
            }
            pos++;
        }
        if (pos < num_chars)
        {
            if (string[pos] == '\r')
                pos++;
            if (string[pos] == '\n')
                pos++;
            if (just_after_eol)
                return pos;
            else
                just_after_eol = 1;
        }
    }
    return header_unfinished;
}

/*
 * Name: chunk_length
 * Purpose: examines 'string' for a chunk header.
 *	returns 0 if no complete chunk header is found
 *	returns (chunk length + chunk header length) (+ve) if successful
 *	returns a -ve number, the number of characters left before the end
 *		of the message, if we find the final 0-length chunk
 */
int chunk_length(Server *me, char *string, int length)
{
    int pos;
    int result = 0;
    int just_after_eol;

    for (pos = 0; pos < length && (string[pos] == '\r' || string[pos] == '\n');
	    pos++)
	;
    if (pos == length)
	return 0;
    for (; pos < length; pos++)
    {
	if (string[pos] >= '0' && string[pos] <= '9')
	    result = (result << 4) + string[pos] - '0';
	else if (string[pos] >= 'a' && string[pos] <= 'f')
	    result = (result << 4) + 10 + string[pos] - 'a';
	else if (string[pos] >= 'A' && string[pos] <= 'F')
	    result = (result << 4) + 10 + string[pos] - 'A';
	else
	    break;
    }
    if (pos == length)
	return 0;
    while (pos < length && string[pos] != '\r' && string[pos] != '\n')
	pos++;
    if (pos < length && string[pos] == '\r')
	pos++;
    if (pos < length && string[pos] == '\n')
	pos++;
    if (pos == length)
	return 0;
    if (result == 0)
    {
	/* Aha!  We've got the final chunk.  Get the rest of the chunk footer */
	just_after_eol = 1;
	while (pos < length)
	{
	    if (string[pos] == '\r' || string[pos] == '\n')
	    {
		if (string[pos] == '\r')
		    pos++;
		if (pos < length && string[pos] == '\n')
		    pos++;
		if (just_after_eol)
		    return -pos;
		else
		    just_after_eol = 1;
	    }
	    else
	    {
		just_after_eol = 0;
		pos++;
	    }
	}
	return 0;
    }
    return result + pos;
}



void child_establish_backend_connection(Server *me, int backend_fd)
{
    int client_fd = me->fd_partner[backend_fd];
    int length;
    int version;
    int content_length;
    int free_start, free_end;
    int extra = 0;
    struct sockaddr_in client_name;
    int client_namelen;

    int cc; 
    int add_x_for_hdr = 1;  // always try to add an X-F...


    set_server_read_fd(me, backend_fd);

    /* shift the tucked-away http query from client_fd to backend_fd.  See
     * control.h for what's going on here (search for 'lifecycle') */
    length = header_length(me,
	    &me->writebuf.buffer[me->writebuf.pos[client_fd]],
	    me->writebuf.num_chars[client_fd], 1, &version, NULL,
	    &content_length, NULL, NULL);
    move_data(me, client_fd, backend_fd, length);
    append_data(me, backend_fd, &free_start, &free_end,
                  me->fd_state[backend_fd]);


    if (version > 9)
    {
        for (cc = 0; (cc <= me->writebuf.num_chars[backend_fd])&&(add_x_for_hdr); cc++)
        {
          int position = cc + me->writebuf.pos[backend_fd]; 

          if ((me->writebuf.buffer[position] == 'x') || (me->writebuf.buffer[position] == 'X')) 
          {
            if (!strncasecmp(&me->writebuf.buffer[position], "X-Forwarded-For:", 16)) 
            {
              add_x_for_hdr = 0;
            }  // if
          }  // if
        }  // for

        // unbound strstr
        //if (!strstr(&me->writebuf.buffer[me->writebuf.pos[backend_fd]], 
        //        "X-Forwarded-For:"))

        /* see if there is an existing X-Forwarded-For */	
        if (add_x_for_hdr)
        {

          /* add an X-Forwarded-For field to the headers. */
          extra = 0;
          /* discard the final \r\n */
          if (me->writebuf.buffer[free_start + extra - 1] == '\n')
              extra--;
          if (me->writebuf.buffer[free_start + extra - 1] == '\r')
              extra--;
          client_namelen = sizeof(struct sockaddr_in);
          getpeername(client_fd, (struct sockaddr *) &client_name,
                  &client_namelen);
          /* Add an X-Forwarded-For line */
          sprintf(&me->writebuf.buffer[free_start + extra],
                  "X-Forwarded-For: %s\r\n\r\n", inet_ntoa(client_name.sin_addr));
          extra += strlen(&me->writebuf.buffer[free_start + extra]);
          me->writebuf.num_chars[backend_fd] += extra;
        }  // if (strstr(...

    }
    else
	extra = 0;
    if (me->writebuf.num_chars[client_fd] != -1)
	/* now move anything else over to the backend_fd */
	move_data(me, client_fd, backend_fd, -1);

    if (content_length == -1)
	content_length = 0;
    me->content_length[backend_fd] = length + extra + content_length;
    /* hmm - a bit of a kludge; add the length of the headers to the
     * content-length, since child_write doesn't distinguish between the
     * headers and the content, it just writes it! */
    me->content_length[client_fd] = 0;
    /* from now on, any data that comes from the client will be appended to
     * backend_fd's queue, while data from the backend will be appended to
     * client_fd's, until we actually get a complete header back from the
     * backend */
    change_state(me, backend_fd, fs_output);
    change_state(me, client_fd, fs_connected);
}  // child_establish_backend

void handle_control_packets(Server *me, int fd, int free_start,
        int chars_read)
{

    static char *packet_buffer = NULL;
    static int packet_buffer_len = 0;
    static int packet_buffer_pos = 0;

    char *old_packet_buffer;
    int pos;
    u32 tmp_l;
    u16 tmp_s;
    int port;
    int packet_length;
    int client_fd, backend_fd;

    /* store away the packet(s) in the buffer, so they don't get messed
     * with part-way through */
    if (chars_read + packet_buffer_pos >= packet_buffer_len)
    {
        packet_buffer_len = 2*(chars_read + packet_buffer_pos);
        old_packet_buffer = packet_buffer;
        packet_buffer = (char *) malloc(packet_buffer_len);
        if (old_packet_buffer)
        {
            if (packet_buffer_pos > 0)
                bcopy(old_packet_buffer, packet_buffer, packet_buffer_pos);
            free(old_packet_buffer);
        }
    }
    bcopy(&me->writebuf.buffer[free_start], &packet_buffer[packet_buffer_pos],
            chars_read);
    packet_buffer_pos = 0;
    /* process (possibly multiple) control packets that we've read */
    for (pos = 0; pos < chars_read; pos += packet_length)
    {
        bcopy(&(packet_buffer[pos + PAK_LEN_OFF]), &tmp_l, 4);
        packet_length = ntohl(tmp_l);
        if (chars_read - pos < 4 || chars_read - pos < packet_length)
        {
            /* child received partial control packet - store away what
             * we've got until the rest arrives */
            bcopy(&packet_buffer[pos], packet_buffer, chars_read - pos);
            packet_buffer_pos = chars_read - pos;
            return;
        }
        bcopy(&(packet_buffer[pos + PAK_ID_OFF]), &tmp_l, 4);
        /* extract the fd from the packet id */
        client_fd = ntohl(tmp_l) >> 16;
        if (me->fd_state[client_fd] != fs_requested_backend)
            /* The connection has closed since we sent off our PAK_REQUEST */
            return;
            /* TODO this will cause problems if, in the interim, the
             * connection closed, another connection arrived and was given
             * the same fd, and a second request packet was sent off to the
             * control port (thus setting the new connection's fd_state to
             * fs_requested_backend).  The reply to the first request
             * packet will be interpreted as replying to the second, and
             * the reply to the second will be discarded.  Hopefully, this
             * is a very rare, if not impossible, scenario, since a lot has
             * to happen in the space of time it takes the controller to
             * get the request packet and reply...  If it does occur, we
             * could potentially solve the problem by having the bottom 16
             * bits of the packet id be a random number, rather than the
             * pid, and require that the reply match this value, but then
             * we'd have to store that value somewhere for each connection.
             */
        switch (packet_buffer[pos + PAK_COM_OFF])
        {
            case PAK_REDIRECT:
                bcopy(&(packet_buffer[pos + PAK_DATA_OFF]), &tmp_l, 4);
                bcopy(&(packet_buffer[pos + PAK_DATA_OFF + 4]), &tmp_s, 2);
                port = ntohs(tmp_s);
                backend_fd = me->fd_partner[client_fd];
                if (backend_fd != -1)
                {
                    /* ok - we're handling a second or subsequent http
                     * query.  If the redirect points us where we're
                     * already connected, just continue! */
                    if (tmp_l == me->fd_connection_ip[backend_fd] &&
                            port == me->fd_connection_port[backend_fd])
                    {
                        child_establish_backend_connection(me, backend_fd);
                        /* TODO this will screw up if the backend timed out
                         * in the same select() the PAK_REDIRECT arrived
                         * from the controller.  We'll say "fine, we're
                         * already connected to that backend" here, and
                         * then the backend fd will read() 0 bytes & close
                         */
                        break;
                    }
                    else
                        shutdown_child_client(me, backend_fd);
                }  // if (backend_fd != -1)

                backend_fd = open_connection(me, tmp_l, port, 0, 1);

                me->fd_partner[client_fd] = backend_fd;
                if (backend_fd < 0)
                {
                    /* ok - that backend is apparently broken.  Report
                     * this to the controller, and then re-request a
                     * backend to handle the query */
                    make_backend_error_packet(me, client_fd, tmp_l, tmp_s);
                    make_request_packet(me, client_fd,
                            me->writebuf.num_chars[client_fd]);
                    break;
                }
                me->fd_partner[backend_fd] = client_fd;
                me->fd_status[backend_fd] = ft_backend;
                me->fd_connection_ip[backend_fd] = tmp_l;
                me->fd_connection_port[backend_fd] = port;
                if (me->fd_state[backend_fd] == fs_pending_connect)
                {
                    /* still waiting on backend to open - don't read */
                    reset_server_read_fd(me, client_fd);
                    /* we can check if the socket is ready by selecting on
                        * a write */
                    set_server_write_fd(me, backend_fd);
                }
                else
                {
                    set_server_read_fd(me, backend_fd);
                    child_establish_backend_connection(me, backend_fd);
                }
                break;
            case PAK_MESSAGE:
                /* discard the original packet sent by the client */
                delete_data(me, client_fd);
                /* add the body of the packet, skipping the packet header,
                 * as output back to the client.  This connection should
                 * terminate once the data is sent. */
                add_data(me, client_fd, &(packet_buffer[pos + PAK_DATA_OFF]),
                        0, packet_length - PAK_DATA_OFF, fs_output_then_die);
                break;
        }
    }
}

/*
 * Name: child_read
 * Purpose: read & respond to data on the child socket
 */
void child_read(Server *me, int fd)
{
    int free_start, free_end;
    struct sockaddr address;
    int address_size;
    int client_fd;
    int backend_fd;
    int query_fd;
    int chars_read;
    int keepalive;
    int content_length;
    int length;
    int retry;
    int version=0;
    Transfer_Encoding encoding;

    /* See control.h for what's going on here (search for 'lifecycle'). */
    /* Basically, we're going to append the data to fd's partner's queue,
     * unless it isn't there yet or we're still doing the initial stages of
     * parsing the headers.  Otherwise the data will go in fd's queue */
    if (me->fd_partner[fd] == -1 || me->fd_state[fd] <= fs_requested_backend)
	append_data(me, fd, &free_start, &free_end, me->fd_state[fd]);
    else
	append_data(me, me->fd_partner[fd], &free_start, &free_end,
		me->fd_state[me->fd_partner[fd]]);
    /* if we're scheduled to suicide because we're idle, don't */
    if (me->num_connections == 0 && die_when_idle != idle_persist)
    {
#ifdef DEBUG
        fprintf(stderr,"child %d given a stay of execution.\n", (int) getpid()); 
#endif
        set_select_timeout(NULL);
    }
    if (fd == me->listen_fd)
    {
        struct stat sbuf;

        /* We have a new connection */
        address_size = sizeof(struct sockaddr);
        client_fd = accept(me->listen_fd, &address, &address_size);
        if (client_fd < 0)
        {

	  switch (errno) {
	    /* non-blocking accepts, ignore following errors (see Stevens
               pp422-424 ) */
	  case EWOULDBLOCK:
	  case ECONNABORTED:
	  case EINTR:
	    return;
	  default:
            /* This is bad.  Looking at the error codes that accept() can
             * return, it appears that the only other one we're likely to get is
             * ENFILE, that is, we've filled the system file table.
             * Anyway, we should continue to service the connections we
             * already have, so we'll just tell our parent that we're busy,
             * so it goes on to another child, and hope the problem goes
             * away... */
            append_data(me, me->parent_fd, &free_start, &free_end, fs_output);
            add_data(me, me->parent_fd, "1f", 0, 2, fs_output);
            return;
	  }
        }

	/* check connection */
	if (fstat(client_fd, &sbuf) < 0) {
	  syslog(LOG_INFO, "pid %d accept returned a bad socket: %m",
		(int) getpid());
          close(client_fd);
	  return;
	}

        set_server_read_fd(me, client_fd);
#ifdef DEBUG
        fprintf(stderr,"Accepted a connection, fd = %d\n", client_fd); 
#endif
        /* set the backend connection so that we'll query the control
         * socket when data comes in on client_fd */
        change_state(me, client_fd, fs_pre_backend);
        me->fd_status[client_fd] = ft_client;
        me->fd_partner[client_fd] = -1;
#ifdef DEBUG
       fprintf(stderr,"client connections increased to %d\n", me->num_connections + 1); 
#endif
        if (++me->num_connections >= me->max_connections)
        {
            /* stop listening to the socket, and tell parent */
            reset_server_read_fd(me, me->listen_fd);
            append_data(me, me->parent_fd, &free_start, &free_end, fs_output);
            add_data(me, me->parent_fd, "1", 0, 1, fs_output);
            me->busy = 1;
#ifdef DEBUG
            fprintf(stderr,"%ld now at capacity - asking parent to go on (on fd %d)\n", getpid(), me->parent_fd); 
#endif
        }
        init_connection(me, client_fd);
    }
    else if (free_end == free_start)
         /* the remaining operations need space in the output buffer -
          * defer handling them if there's no space. */
         return;
    else if (fd == me->parent_fd)
    {
#ifdef DEBUG
        fprintf(stderr,"child received message from parent\n"); 
#endif

        /* FAIL POINT 1 ?? */
        /* parent's messages are content-free. */
        chars_read = read(fd, &(me->writebuf.buffer[free_start]),
                free_end - free_start);
        if (chars_read <= 0)
        {
        /* just a signal - ignore it */
            if (chars_read < 0 && errno == EINTR) return;        
            if (chars_read < 0 && errno == EAGAIN) return;        

            /* It appears the parent has died - stop accepting connections,
             * finish up with our current connections, then die */
            if (chars_read < 0)
                syslog(LOG_INFO, "pid %d failed to read from parent: %m\n",
                        (int) getpid());
            else
                syslog(LOG_INFO, "pid %d thinks parent closed its fd\n",
                        (int) getpid());
            reset_server_read_fd(me, me->listen_fd);
            if (me->num_connections == 0)
                _exit(0);
            else
                die_when_idle = idle_immediate_die;
        }
        if (me->writebuf.buffer[free_start] != 'x')
        {
            syslog(LOG_INFO, "pid %d received garbage on fd %d: %c (%d), "
                    "%d characters read\n", (int) getpid(), fd,
                    me->writebuf.buffer[free_start],
                    me->writebuf.buffer[free_start], chars_read);
            return;
        }
        if (me->num_connections < me->max_connections)
        {
            /* start to accept on the shared port */
            set_server_read_fd(me, me->listen_fd);
#ifdef DEBUG
            fprintf(stderr,"\t%d started listening on incoming port\n", (int) getpid()); 
#endif
        }
        else
        {
            /* still have too many connections - tell parent to ask someone
             * else */
            append_data(me, me->parent_fd, &free_start, &free_end, fs_output);
            add_data(me, me->parent_fd, "0", 0, 1, fs_output);
#ifdef DEBUG
            fprintf(stderr,"\t%d too busy - told parent no\n", (int) getpid()); 
#endif
        }
    }
    else if (fd == me->control_fd)
    {
#ifdef DEBUG
        fprintf(stderr,"child %ld received control reply\n", getpid()); 
#endif
        /* FAIL POINT 2 ?? */
        chars_read = read(fd, &(me->writebuf.buffer[free_start]),
                free_end - free_start);
        if (chars_read <= 0)
        {
        /* just a signal - ignore it */
            if (chars_read < 0 && errno == EINTR) return;        
            if (chars_read < 0 && errno == EAGAIN) return;        

            /* It appears the controlling process has died - stop accepting
             * connections, finish up with our current connections, then
             * die.  Any connections that tries to talk to the now-defunct
             * control port should also be killed on the spot. */
            if (chars_read < 0)
                syslog(LOG_INFO, "child pid %d failed to read from "
                        "control fd: %m\n", (int) getpid());
            else
                syslog(LOG_INFO, "child pid %d thinks controller closed "
                        "its fd\n", (int) getpid());
            reset_server_read_fd(me, me->listen_fd);
            reset_server_read_fd(me, me->control_fd);
            me->control_fd = -1;
            if (me->num_connections == 0)
                _exit(0);
            else
                die_when_idle = idle_immediate_die;
            return;
        }
        handle_control_packets(me, fd, free_start, chars_read);
    }
    else
    {
	
        if (me->fd_state[fd] == fs_pre_backend)
        {
	    struct stat sbuf;
	    if (fstat(fd, &sbuf) < 0) {
	        /* test code - if we *never* see this message, delete this 
		   test for speed */
		syslog(LOG_INFO, "pid %d badf fd %d in pre_backend: %m",
			(int)getpid(), fd);
            	delete_client(me, fd);
		return;
	    }

	    /* a *small* kludge - we want to continue and call
	       protocol_read if not ssl */
            if (handle_pre_backend(me, fd) >= 0)
	        return;
        }

        chars_read = protocol_read(me, fd, &retry, free_start,
		free_end - free_start);
#ifdef DEBUG
        fprintf(stderr,"child read %d chars on fd %d\n", chars_read, fd); 
#endif
        if (chars_read <= 0)
        {
            if (chars_read < 0)
            {
                if (retry) return;
#ifdef DEBUG
                else
                    fprintf(stderr,"failed reading from a client's "
                            "pending socket: %m\n");
#endif
            }
            if (chars_read == 0 && me->fd_status[fd] == ft_backend)
            {
                if (me->fd_status[me->fd_partner[fd]] == ft_client_keepalive &&
                        (me->writebuf.num_chars[me->fd_partner[fd]] > 0 ||
                        me->fd_state[me->fd_partner[fd]] ==
                        fs_requested_backend))
                {
                    /* let a keepalive client remain connected if there's data,
                     * or the reply to a PAK_REQUEST, pending, even if the
                     * backend has closed the connection */
                    me->fd_partner[me->fd_partner[fd]] = -1;
                    shutdown_child_client(me, fd);
                    return;
                }
                else if (me->writebuf.num_chars[me->fd_partner[fd]] > 0 && 
                        me->fd_state[me->fd_partner[fd]] == fs_output)
                {
                    /* the backend has closed the connection before we've
                     * finished flushing output to the client - finish,
                     * then close the connection to the frontend */
                    me->fd_partner[me->fd_partner[fd]] = -1;
                    shutdown_child_client(me, fd);
                    change_state(me, me->fd_partner[fd], fs_output_then_die);
		    return;
                }
            }
            delete_client(me, fd);
        }
        else if (me->fd_status[fd] <= ft_client)
        {
	    /* read something from the client */
	    backend_fd = me->fd_partner[fd];
	    if (backend_fd == -1 || me->fd_state[fd] <= fs_requested_backend)
		query_fd = fd;
	    else
		/* the data is destined for the backend's queue */
		query_fd = backend_fd;
	    /* First, append the data to any that was already present */
	    backdate_read(me, query_fd, &chars_read, &free_start);
	    add_data(me, query_fd, NULL, free_start, chars_read,
		    me->fd_state[query_fd]);
	    if (me->fd_state[fd] < fs_requested_backend)
	    {
		/* we haven't yet requested a backend - check if we've got
		 * a complete header yet */
		length = header_length(me, &me->writebuf.buffer[free_start],
			chars_read, 1, NULL, &keepalive, NULL, NULL, NULL);
		if (length == header_malformed)
		    delete_client(me, fd);	/* malformed header */
		else if (length == header_empty)
		    delete_data(me, query_fd);
		else if (length > 0)
		{
		    if (me->control_fd == -1)
			/* we've lost our control port, and don't know
			 * where to send this connection - kill it off */
			fatal(me, "lost control port in client read");
			// delete_client(me, fd);
		    else
		    {
			me->fd_status[fd] = (keepalive) ?
				ft_client_keepalive : ft_client;
			make_request_packet(me, fd, length);
		    }
		}
	    }
        }
        else
        {
	    client_fd = me->fd_partner[fd];
            if (me->fd_status[fd] == ft_backend &&
		    me->fd_state[client_fd] != fs_output)
	    {
		/* we're reading stuff from the backend, but haven't
		 * started outputting to the client - we must still be
		 * waiting on the headers from the backend, or we're doing
		 * transfer_chunked, and we're between chunks */
		backdate_read(me, client_fd, &chars_read, &free_start);
		add_data(me, client_fd, NULL, free_start, chars_read, 
			me->fd_state[client_fd]);
		if (me->content_length[client_fd] < 0)
		{
		    /* we previously ran out of data mid-chunk-header */
		    encoding = transfer_chunked;
		    length = 0;
		    keepalive =
			    (me->fd_status[client_fd] == ft_client_keepalive);
		    /* try to parse a chunk header again */
		    content_length = chunk_length(me,
			    &me->writebuf.buffer[me->writebuf.pos[client_fd]],
			    me->writebuf.num_chars[client_fd]);
		    if (content_length == 0)
			/* still unfinished */
			length = header_unfinished;
		    else if (content_length < 0)
		    {
			/* we got the final 0-length chunk - act as if
			    * we have a content-length for the whole
			    * message (which we now effectively do) */
			encoding = transfer_normal;
			content_length = -content_length;
		    }
		    /* else just drop through with the values we've got */
		}
		else {
		    length = header_length(me, &me->writebuf.buffer[free_start],
			    chars_read, 0, &version, &keepalive, 
			    &content_length, &encoding, 
				&me->reply_status[fd]);
		    if (debug > 2)
			syslog(LOG_INFO, "version %d reply status %d", 
			version, me->reply_status[fd]);
		}
		if (length == header_unfinished)
		    change_state(me, fd, fs_connected);
		else
		{
                    /* We've got a complete header back from the backend.
                     * See control.h for what's going on here (search for
                     * 'lifecycle') */
                    change_state(me, client_fd, fs_output);
                    me->fd_status[client_fd] = (keepalive) ?
                            ft_client_keepalive: ft_client;
                    if (encoding == transfer_chunked)
                    {
                        if (content_length < 0)
                            /* we've in the headers, and haven't started
                             * parsing chunks yet */
                            me->content_length[client_fd] = -length;
                        else
                            me->content_length[client_fd] = -content_length;
                    }
                    else if (content_length >= 0)
                        me->content_length[client_fd] = content_length + length;                        /* hmm - a bit of a kludge; add the length of the
                         * headers to the content-length, since child_write
                         * doesn't distinguish between the headers and the
                         * content, it just writes it! */
                    else
                        me->content_length[client_fd] = 0;
                
		}
		return;
	    }
#ifdef DEBUG
            fprintf(stderr,"\tforwarding to fd %d\n", me->fd_partner[fd]); 
#endif
            add_data(me, client_fd, NULL, free_start, chars_read, fs_output);
        }
    }
}


/*
 * Name: child_write
 * Args: server struct and a file descriptor describing a connection
 * Purpose: Write output to the specific connection
 */

void child_write(Server *me, int fd)
{
    int chars_to_write;
    char little_buffer[1];
    int start, length;
    int keepalive;
    int free_start, free_end;
    int retry;
    int chunk;
    int backend_fd;

    if (me->fd_state[fd] == fs_pending_connect)
    {
#ifdef DEBUG
        fprintf(stderr,"fd %d (pid %ld) is now ready to go\n", fd, getpid()); 
#endif
        /* While the socket is still non-blocking, test that we
         * successfully opened the connection.  Rely on the fact that web
         * servers don't send anything on their sockets until some sort of
         * request comes in, because otherwise, we'd eat the first char on
         * the socket. */

        /* NOT A FAIL POINT, THE SOCKET IS NON-BLOCKING HERE 3 ?? */
        read(fd, little_buffer, 1);

        if ((errno != EAGAIN) || (me->remove_backend)) 
        {
            /* ok - that backend is apparently broken.  Report this to the
             * controller, and then re-request a backend to handle the
             * query */
            if (me->control_fd == -1)
            {
                /* we've lost our control port, and don't know where to
                 * send this connection - kill it off */
		fatal(me, "lost control port in pending connect");
                delete_client(me, fd);
                return;
            }
            make_backend_error_packet(me, me->fd_partner[fd], 0, 0);
            make_request_packet(me, me->fd_partner[fd],
                    me->writebuf.num_chars[me->fd_partner[fd]]);
            shutdown_child_client(me, fd);
            me->fd_partner[me->fd_partner[fd]] = -1;

            /* flag to remove a backend */
            me->remove_backend = 0;
            me->fd_connect_start[fd] = 0;
            me->total_pending_connects--;

            /* if no more connects then reset timeout to NULL */
            if (me->total_pending_connects == 0)
              me->glob_timeout = NULL;

            return;
        }  // if ((errno != EAGAIN) || (me->remove_backend)) 

    me->total_pending_connects--;  // decrement total_pending_connects 

    // reset glob_timeout    
    if (me->total_pending_connects == 0)
      me->glob_timeout = NULL;




        /* make socket blocking again */
        fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) & ~O_NONBLOCK);
        child_establish_backend_connection(me, fd);
    }
    else
    {
        start = me->writebuf.pos[fd];
        length = chars_to_write = me->writebuf.num_chars[fd];
	if (me->content_length[fd] < 0)
	{
	    /* we're transmitting transfer_chunked data */
	    chunk = 1;
	    while (-me->content_length[fd] < chars_to_write && chunk > 0)
	    {
		chunk = chunk_length(me,
			&me->writebuf.buffer[me->writebuf.pos[fd] -
			me->content_length[fd]],
			me->writebuf.num_chars[fd] + me->content_length[fd]);
		if (chunk > 0)
		    me->content_length[fd] -= chunk;
		else if (chunk < 0)
		    /* final (size 0) chunk fully in the buffer */
		    me->content_length[fd] = -me->content_length[fd] - chunk;
	    }
	    if (me->content_length[fd] < 0 &&
		    chars_to_write > -me->content_length[fd])
		chars_to_write = -me->content_length[fd];
	}
	if (me->content_length[fd] > 0 &&
		chars_to_write > me->content_length[fd])
	    chars_to_write = me->content_length[fd];
        if (!protocol_write(me, fd, chars_to_write, &retry))
        {
	    if (retry)
		return;
            if (fd == me->control_fd)
            {
                /* It appears the controlling process has died - stop
                 * accepting connections, finish up with our current
                 * connections, then die.  Any connections that tries to
                 * talk to the now-defunct control port should also be
                 * killed on the spot. */
                reset_server_read_fd(me, me->listen_fd);
                reset_server_read_fd(me, me->control_fd);
                me->control_fd = -1;
                if (me->num_connections == 0)
                    _exit(0);
                else
                    die_when_idle = idle_immediate_die;
                return;
            }
        }
	backend_fd = me->fd_partner[fd];
        if (me->content_length[fd])
	{
	    /* adjust length to equal the number of chars actually written */
	    if (me->writebuf.num_chars[fd] > 0)
		length -= me->writebuf.num_chars[fd];
	    if (me->content_length[fd] > 0)
		/* we're writing to a client that has a keepalive connection -
		 * we need to know when we're finished, so we can work through
		 * pipelined queries from it in sequence */
		me->content_length[fd] -= length;
	    else if (me->content_length[fd] < 0)
	    {
		/* We're dealing with chunked data */
		me->content_length[fd] += length;
		if (me->content_length[fd] >= 0)
		{
		    /* We've sent all the complete chunks we've received,
		     * but the last packet must have ended before we got
		     * the next chunk's size - stop outputting and just
		     * buffer until we've got another complete chunk-header */
		    change_state(me, fd, fs_connected);
		    me->content_length[fd] = -1;
		    /* a bit of a kludge - a negative content length is
		     * used elsewhere to indicate chunked data, so ensure
		     * that it remains negative even though we have no
		     * known content length */
		}
	    }
            if (me->content_length[fd] == 0)
            {
		if (me->fd_status[fd] == ft_backend)
		    /* we've sent an entire query, so anything left in our
		     * queue is some subsequent query we should just store
		     * for now */
		    change_state(me, fd, fs_connected);
		else
		{
		    /* We've finished responding to the query!  Now deal with
		    * any buffered data that has arrived in the interim. */
		    if (me->writebuf.num_chars[backend_fd] > 0)
		    {
			/* have a partial query - deal with it */
			move_data(me, backend_fd, fd, -1);
			length = header_length(me,
				&me->writebuf.buffer[me->writebuf.pos[fd]],
				me->writebuf.num_chars[fd], 1, NULL, &keepalive,
				NULL, NULL, NULL);
			if (length == header_malformed)
			{
			    delete_client(me, fd);
			    /* malformed query */
			    return;
			}
			else if (length == header_unfinished)
			    change_state(me, fd, fs_ready_for_query);
			else if (length == header_empty)
			{
			    delete_data(me, fd);
			    change_state(me, fd, fs_ready_for_query);
			}
			else
			{
			    /* it's possible that a previously keepalive
			    * connection might change to non-keepalive, for
			    * instance if a subsequent query contains
			    * "Connection: close" */
			    if (!keepalive)
				me->fd_status[fd] = ft_client;
			    if (me->control_fd == -1)
			    {
				/* we've lost our control port, and don't know
				* where to send this connection - kill it off
				*/
				fatal(me, "lost control port in next query");
				delete_client(me, fd);
				return;
			    }
			    else
				make_request_packet(me, fd, length);
			}
		    }
		    else
			change_state(me, fd, fs_ready_for_query);
		}
            }
        }
	else if (me->fd_status[fd] == ft_client_keepalive && 
		 me->fd_state[fd] != fs_output_then_die) {
	    if (me->reply_status[backend_fd] == 100) {
		/* a continue was received  - set state back */
	        change_state(me, fd, fs_connected);
	    }
	    /* a keepalive reply with no content-length - we'll just have
	     * to hope we got it all out in one packet */
	    else {
	        change_state(me, fd, fs_ready_for_query);
	    }
	}
        if (me->busy == -1) 
        {
            append_data(me, me->parent_fd, &free_start, &free_end, fs_output);
            if (add_data(me, me->parent_fd, "f", 0, 1, fs_output))
                me->busy = 0;
        }
        if (me->fd_state[fd] == fs_output_then_die)
            delete_client(me, fd);

    }
}

/* 'extra' is true if this child is surplus to the persistant, minimum
 * number of children.  Such children should exit when they go idle. */
void child_main(Server *serv, int fd, int extra)
{
    FD_ZERO(&serv->read_fds);
    FD_ZERO(&serv->write_fds);
    FD_ZERO(&serv->except_fds);
    serv->parent_fd = fd;
    serv->max_connection_fd = 0;
    serv->fd_partner[serv->parent_fd] = serv->parent_fd;
    change_state(serv, serv->parent_fd, fs_connected);
    serv->fd_status[serv->parent_fd] = ft_other;
    set_server_read_fd(serv, serv->parent_fd);
    serv->control_fd = open_connection(serv, inet_addr(control_ip),
            serv->control_port, 1, 0);
    set_server_read_fd(serv, serv->control_fd);
    if (serv->control_fd < 0)
        fatal(serv, "unable to open control port: %m\n");
    serv->fd_partner[serv->control_fd] = serv->control_fd;
    change_state(serv, serv->control_fd, fs_connected);
    serv->fd_status[serv->control_fd] = ft_other;
    set_server_read_fd(serv, serv->control_fd);
#ifdef DEBUG
    fprintf(stderr,"child %ld thinks parent fd = %d\n", getpid(), serv->parent_fd); 
#endif
    die_when_idle = (extra) ? idle_timeout_die : idle_persist;
    srandom(getpid());
    signal(SIGINT, SIG_DFL);
    signal(SIGTERM, SIG_DFL);
    /* Initialise protocol dependent stuff */
    init_child(serv);
    server_main_loop(serv, child_write, child_read, NULL);
    /* printf("x"); fflush(stdout); */
#ifdef DEBUG
    fprintf(stderr,"surplus child %ld suicided after being idle too long.\n", getpid());
#endif
    _exit(0);
}
