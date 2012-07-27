
#include "server.h"
#include "param.h"

typedef struct
{
    int children[MAX_CHILDREN];
    int child_pipe[MAX_CHILDREN];
    char child_busy[MAX_CHILDREN];
    int fd_to_child[MAX_FD];
    int num_children;
    int curr_child;
} Children;

void terminate(int sig_num);
void parent_main_loop(int control_port, unsigned long listen_ip,
	int listen_port);
