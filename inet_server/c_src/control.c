#include <ctype.h>
#include <syslog.h>

#include "control.h"
#include "backend.h"
#include "main.h"
#include "child.h"

void write_long(char *buffer, int *offset, u32 value, char convert)
{
    u32 net_u32;

    if (convert)
	net_u32 = htonl(value);
    else
	net_u32 = value;
    if (offset)
    {
	bcopy(&net_u32, buffer + *offset, 4);
	*offset += 4;
    }
    else
	bcopy(&net_u32, buffer, 4);
}

void write_short(char *buffer, int *offset, u16 value, char convert)
{
    u16 net_u16;

    if (convert)
	net_u16 = htons(value);
    else
	net_u16 = value;
    if (offset)
    {
	bcopy(&net_u16, buffer + *offset, 2);
	*offset += 2;
    }
    else
	bcopy(&net_u16, buffer, 2);
}

void write_string(char *buffer, int *offset, char *value)
{
    if (offset)
    {
	strcpy(buffer + *offset, value);
	*offset += strlen(value) + 1;
    }
    else
	strcpy(buffer, value);
}

void write_string_n(char *buffer, int *offset, char *value, int len)
{
    if (offset)
    {
	strncpy(buffer + *offset, value, len);
	*offset += len;
	buffer[(*offset)++] = '\0';
    }
    else
    {
	strncpy(buffer, value, len);
	buffer[len] = '\0';
    }
}

/* Constructs and queues a request packet describing the http query pointed
 * to by 'start'.
 */
int make_request_packet(Server *serv, int fd, int num_chars)
{
    struct sockaddr_in client_name, local_name;
    int client_namelen, local_namelen;
    int free_start, free_end;
    int packet_length;
    char *request;
    int len;

    client_namelen = sizeof(struct sockaddr_in);
    if (getpeername(fd, (struct sockaddr *) &client_name, &client_namelen) < 0)
    {
	syslog(LOG_INFO, "make_request_packet: failed to get peer "
		"name: %m\n");
	return 0;
    }
    local_namelen = sizeof(struct sockaddr_in);
    if (getsockname(fd, (struct sockaddr *) &local_name, &local_namelen) < 0)
    {
	syslog(LOG_INFO, "make_request_packet: failed to get sock name: %m\n");
	return 0;
    }
    change_state(serv, fd, fs_requested_backend);
    /* locate the free end of the buffer (possibly appending to previous
     * control packets) */
    append_data(serv, serv->control_fd, &free_start, &free_end, fs_output);
    /* reserve the first 4 bytes of the packet for the size field */
    packet_length = PAK_COM_OFF;
    /* add command field to packet */
    serv->writebuf.buffer[free_start + packet_length++] = PAK_REQUEST;
    /* add unique ID field to packet */
    write_long(&serv->writebuf.buffer[free_start], &packet_length,
	    (getpid() & 0xffff) | (fd << 16), 1);
    /* add client IP field to packet */
    write_long(&serv->writebuf.buffer[free_start], &packet_length,
	    client_name.sin_addr.s_addr, 0);
    /* add local IP field to packet */
    write_long(&serv->writebuf.buffer[free_start], &packet_length,
	    local_name.sin_addr.s_addr, 0);
    /* add local port field to packet */
    write_short(&serv->writebuf.buffer[free_start], &packet_length,
	    serv->port, 1);
    /* write the http headers */
    request = &serv->writebuf.buffer[serv->writebuf.pos[fd]];
    while (request[0] == '\r' || request[0] == '\n')
	request++;
    for (len = 0; len < num_chars && !isspace(request[len]); len++)
	;
    if (len < num_chars)
    {
	write_string(&serv->writebuf.buffer[free_start], &packet_length,
		"method");
	write_string_n(&serv->writebuf.buffer[free_start], &packet_length,
		request, len);
	num_chars -= len;
	request += len;
	for (len = 0; len < num_chars && isspace(request[len]); len++)
	    ;
	if (len < num_chars)
	{
	    num_chars -= len;
	    request += len;
	    for (len = 0; request[len] && !isspace(request[len]); len++)
		;
	    write_string(&serv->writebuf.buffer[free_start], &packet_length,
		    "uri");
	    write_string_n(&serv->writebuf.buffer[free_start], &packet_length,
		    request, len);
	    num_chars -= len;
	    request += len;
	    for (len = 0; len < num_chars && isspace(request[len]); len++)
		;
	    if (len < num_chars)
	    {
		num_chars -= len;
		request += len;
		for (len = 0; request[len] && !isspace(request[len]); len++)
		    ;
		write_string(&serv->writebuf.buffer[free_start],
			&packet_length, "version");
		write_string_n(&serv->writebuf.buffer[free_start],
			&packet_length, request, len);
		/* each line should end with \r\n.  Be a bit lenient, and
		 * allow just \r or \n, too.  End of request is indicated
		 * by two such lines in a row */
		if (request[len] == '\r')
		    len++;
		if (request[len] == '\n')
		    len++;
		num_chars -= len;
		request += len;
		/* find "field: value\r\n" on successive lines, adding them
		 * to the buffer as field\0value\0 */
		while (num_chars > 0 && request[0] != '\r' &&
			request[0] != '\n')
		{
		    for (len = 0; len < num_chars && request[len] != ':' &&
			    request[len] != '\r' && request[len] != '\n'; len++)
			;
		    if (len < num_chars && request[len] == ':')
		    {
			write_string_n(&serv->writebuf.buffer[free_start],
				&packet_length, request, len);
			for (len++; len < num_chars && isspace(request[len]);
				len++)
			    ;
			num_chars -= len;
			request += len;
			for (len = 0; len < num_chars && request[len] != '\r' &&
				request[len] != '\n'; len++)
			    ;
			write_string_n(&serv->writebuf.buffer[free_start],
				&packet_length, request, len);
		    }
		    if (request[len] == '\r')
			len++;
		    if (request[len] == '\n')
			len++;
		    num_chars -= len;
		    request += len;
		}
	    }
	}
    }
    /* set first 4 bytes of packet to the total packet length */
    write_long(&serv->writebuf.buffer[free_start], NULL,
	    packet_length, 1);
    /* add the packet to the output buffer properly */
    if (free_end - free_start < packet_length)
	fatal(serv, "request packet overflowed the buffer\n");
    add_data(serv, serv->control_fd, NULL, free_start, packet_length,
	    fs_output);
    return 1;
}

int make_backend_error_packet(Server *serv, int fd, u32 ip, u16 nbo_port)
{
    struct sockaddr_in local_name;
    struct sockaddr_in remote_name;
    int local_namelen;
    int remote_namelen;
    int packet_length;
    int free_start, free_end;

    local_namelen = sizeof(struct sockaddr_in);
    if (getsockname(fd, (struct sockaddr *) &local_name, &local_namelen) < 0)
    {
	syslog(LOG_INFO, "make_backend_error_packet: failed to get sock "
		"name: %m\n");
	return 0;
    }
    /* locate the free end of the buffer (possibly appending to previous
     * control packets) */
    append_data(serv, serv->control_fd, &free_start, &free_end, fs_output);
    packet_length = PAK_COM_OFF;
    /* write control code */
    serv->writebuf.buffer[free_start + packet_length++] = PAK_BE_ERROR;
    /* write unique id */
    write_long(&serv->writebuf.buffer[free_start], &packet_length,
	    (getpid() & 0xffff) | (fd << 16), 1);
    /* add local IP field to packet */
    write_long(&serv->writebuf.buffer[free_start], &packet_length,
	    local_name.sin_addr.s_addr, 0);
    /* add local port field to packet */
    write_short(&serv->writebuf.buffer[free_start], &packet_length,
	    serv->port, 1);
    if (serv->fd_partner[fd] == -1)
    {
	/* the connection failed straight away, so we've still got the ip
	 * and port (already in network byte order) to use */
	write_long(&serv->writebuf.buffer[free_start], &packet_length, ip, 0);
	write_short(&serv->writebuf.buffer[free_start], &packet_length,
		nbo_port, 0);
    }
    else
    {
	/* else the partner (which is the failed backend fd) has the ip and
	 * port stored in serv->fd_connection_* */
	write_long(&serv->writebuf.buffer[free_start], &packet_length,
		serv->fd_connection_ip[serv->fd_partner[fd]], 0);
	write_short(&serv->writebuf.buffer[free_start], &packet_length,
		serv->fd_connection_port[serv->fd_partner[fd]], 1);
    }
    /* add client IP field to packet */
    remote_namelen = sizeof(struct sockaddr_in);
    if (getpeername(fd,  (struct sockaddr *) &remote_name,
	    &remote_namelen) == -1)
    {
	syslog(LOG_INFO, "make_backend_error_packet: failed to getpeer "
		"name: %m\n");
	return 0;
    }
    write_long(&serv->writebuf.buffer[free_start], &packet_length,
	    remote_name.sin_addr.s_addr, 0);
    /* write length */
    write_long(&serv->writebuf.buffer[free_start], NULL, packet_length, 1);
    /* add the packet to the output buffer properly */
    if (free_end - free_start < packet_length)
	fatal(serv, "unable to fit backend_error packet in static buffer\n");
    add_data(serv, serv->control_fd, NULL, free_start, packet_length,
	    fs_output);
    return 1;
}
