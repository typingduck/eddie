
#ifndef SERVER_H
#define SERVER_H

#include <string.h>
#include <strings.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/time.h>

/* for ugly alarm hack */
#include <arpa/inet.h>

#include "param.h"

/* maximum select timeout */
#define SELECT_TIMEOUT 10 

#define MAX_FD 256 
/* #define MAX_FD FD_SETSIZE */

/* important relationships in Fd_State:
    * top 3 states must be (in order): fs_connected fs_output fs_output_then_die
    * fs_pre_backend, fs_no_backend, fs_ready_for_query must all be <
	    fs_requested_backend
*/
typedef enum
{
    fs_pre_backend, fs_no_backend, fs_ready_for_query, fs_requested_backend,
    fs_pending_connect, fs_connected, fs_output, fs_output_then_die
} Fd_State;

/* important relationships in Fd_Status:
    * first 2 states must be: ft_client_keepalive ft_client (in that order)
*/
typedef enum
{
    ft_client_keepalive, ft_client, ft_backend, ft_other
} Fd_Status;

typedef struct
{
    char *buffer;
    int buffer_len;
    int pos[MAX_FD];
    int num_chars[MAX_FD];
    int next_busy[MAX_FD];
    int prev_busy[MAX_FD];
    int first_busy, last_busy;
} Buffer;

typedef struct
{
    int port;
    int listen_fd;
    int parent_fd;
    int max_connection_fd;
    int num_connections;
    int max_connections;
    int busy;
    int fd_partner[MAX_FD];		/* fd pairs each refer to one
					another using fd_partner */
    Fd_State fd_state[MAX_FD];		/* tracks the changing state of the
					connection on this fd */
    Fd_Status fd_status[MAX_FD];	/* labels an fd, indicating its purpose.
					Should be fairly unchanging */
    unsigned long fd_connection_ip[MAX_FD];
    short fd_connection_port[MAX_FD];	/* these two fields record the remote
					ip and port a fd is connected to,
					if we initiated the connection */

    short reply_status[MAX_FD];		/* contains server response status */
    time_t fd_connect_start[MAX_FD];    /* record the start time for 
                                        the connect */

    int content_length[MAX_FD];		/* the expected number of bytes to
					be written to this fd, or (if -ve)
					indicates either no content-length
					known, or that our encoding is
					transfer_chunked */
    int control_fd;
    int control_port;
    fd_set read_fds;
    fd_set write_fds;
    fd_set except_fds;
    Buffer writebuf;

    struct timeval timeout;   /* timeout struct for the select */
    struct timeval *glob_timeout;  /* used by select */
    int total_pending_connects; /* total non-blocking connects */
    int remove_backend;  /* flag for child_write, am I adding too many AWW? */
/*
    char last_http_req[REQ_LEN]; // simply buffer to store last HTTP request for debug 
*/

} Server;

#define DONT_CARE 0

Server *server_new(unsigned long ip, int port, int listen_queue);
void set_select_timeout(struct timeval *timeout);
void server_main_loop(Server *me, void (*write_fn)(Server *, int),
	void (*read_fn)(Server *, int), void (*except_fn)(Server *, int));
void shutdown_ipc(Server *serv);

void set_server_read_fd(Server *serv, int fd);
void reset_server_read_fd(Server *serv, int fd);
void set_server_write_fd(Server *serv, int fd);
void reset_server_write_fd(Server *serv, int fd);

void change_state(Server *serv, int fd, Fd_State new_state);

int locate_space(Server *serv, int *start, int *end);
int generic_write(int fd, Server *serv, int chars_to_write);

int add_data(Server *serv, int fd, char *buffer, int start, int length,
	Fd_State data_type);
void move_data(Server *serv, int from_fd, int to_fd, int length);
void swap_data(Server *serv, int fd1, int fd2);
void delete_data(Server *serv, int fd);
void append_data(Server *serv, int fd, int *start, int *end,
	Fd_State data_type);

char *grow_buffer(Server *serv, int *free_start, int *free_end, int we_free,
		int opt_len);

void backdate_read(Server *serv, int fd, int *chars_read, int *start);

#endif /* SERVER_H */
