#include <signal.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <syslog.h>
#include <stdarg.h>
#include <unistd.h>

#include "param.h"
#include "parent.h"
#include "control.h"

void fatal(Server *serv, char *msg, ...)
{
    void terminate(int sig_num);
    char full_msg[4096];
    va_list param_list;

    va_start(param_list, msg);
    sprintf(full_msg, "(fatal) %s", msg);
    vsyslog(LOG_INFO, full_msg, param_list);
    if (serv)
    {
	shutdown_ipc(serv);
	if (serv->parent_fd == -1)
	    terminate(SIGINT);
    }
    exit(1);
}

void usage (void)
{
	fprintf(stderr, "usage;\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "relay [options] <control_port> [<listen_ip>] <listen_port>]]\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "    -r  0x%x\t\tsize of reserve at end of buffer (bytes)\n", BUF_RESERVE);
	fprintf(stderr, "    -m  %d\t\tminimum number of children\n", MIN_CHILDREN);
	fprintf(stderr, "    -M  %d\t\tmaximum number of children\n", MAX_CHILDREN);
	fprintf(stderr, "    -c  %d\t\tmaximum number of connections per child\n", MAX_CONNECTIONS);
	fprintf(stderr, "    -t  %d\t\tchild idle time base (seconds)\n", IDLE_TIME_BASE);
	fprintf(stderr, "    -T  %d\t\tchild idle time range\n", IDLE_TIME_RANGE);
	fprintf(stderr, "    -b  0x%x\t\tchild's buffer size (bytes)\n", BUF_LEN);
	fprintf(stderr, "    -l  %.2f\t\tchild's buffer growth rate\n", (float)BUF_GROW_RATE);
	fprintf(stderr, "    -C  %s\tcontrol ip address\n", control_ip);
	fprintf(stderr, "    -a  %d\t\twatchdog timer\n", DEF_ALARM_TIME);
	fprintf(stderr, "    -d  %d\t\tdebug message\n", debug);
	fprintf(stderr, "    -h  \t\tprint this messages\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "    The numbers displayed are the compiled in defaults.\n");
	fprintf(stderr, "\n");

	exit(5);
}

char *ntoa(unsigned int ip)
{
    struct in_addr ia;

    ia.s_addr = ip;

    return inet_ntoa(ia);
}

int main(int argc, char *args[])
{
    int control_port;
    unsigned int listen_ip;
    int listen_port;
    int ok = 1;

	extern char *optarg;
	extern int optind;
	int ch;

    /* sanity checking */
    if (sizeof(u16) != 2)
	fatal(NULL, "typedef for u16 does not result in a 16-bit int\n");
    if (sizeof(u32) != 4)
	fatal(NULL, "typedef for u32 does not result in a 32-bit int\n");

	/* get the allowed command line options */

	while ((ch = getopt(argc, args, "h:r:m:M:c:t:T:b:l:C:a:d:")) != -1)
		switch (ch) {

		case 'r': /* reserve at end of buffer (BUF_RESERVE) */
				buffer_reserve = atoi(optarg);
				break;
		case 'm': /* minimum children (MIN_CHILDREN) */
				min_children = atoi(optarg);
				break;
		case 'M': /* maximum children (MAX_CHILDREN) */
				max_children = atoi(optarg);
				break;
		case 'c': /* maximum connections (MAX_CONNECTIONS) */
				max_connections = atoi(optarg);
				break;
		case 't': /* child idle time base (IDLE_TIME_BASE) */
				idle_time_base = atoi(optarg);
				break;
		case 'T': /* child idle time range (IDLE_TIME_RANGE) */
				idle_time_range = atoi(optarg);
				break;
		case 'b': /* child buffer size (BUF_LEN) */
				buffer_len = atoi(optarg);
				break;
		case 'l': /* buffer growth rate (BUF_GROW_RATE) */
				buffer_grow_rate = atof(optarg);
		case 'C': /* control ip (CONTROL_IP) */
				control_ip = optarg;
				break;

		case 'a': /* alarm, default */
				alarm_time = atoi(optarg);
				break;

		case 'd': /* set debug level */
				if (optarg)
				  debug = atoi(optarg);
				else
				  debug = 1; /* -d  == -d 1 */
				break;
		case 'h': /* output the options and their faults. */
		default:
				usage();
		}

	argc -= optind;
	args += optind;

	/* more sanity checking */
	if (min_children < 1)
		fatal(NULL, "error - MIN_CHILDREN (= %d)\n", min_children);
	if (max_children < 1)
		fatal(NULL, "error - MAX_CHILDREN (= %d)\n", max_children);
	if (max_children < min_children)
		fatal(NULL, "error - MAX_CHILDREN (= %d) must be greater than"
			"MIN_CHILDREN (= %d).\n", max_children, min_children);
    if (max_children + 4 > MAX_FD)
		fatal(NULL, "error - MAX_FD (= %d) is not large enough to "
			"accommodate MAX_CHILDREN (= %d)\n", MAX_FD, max_children);
    if (6 + 2*max_connections > MAX_FD)
		fatal(NULL, "error - MAX_FD (= %d) is not large enough to "
			"accommodate MAX_CONNECTIONS (= %d)\n", MAX_FD,
			max_connections);

	if (idle_time_base < 1)
		fatal(NULL, "error - idle time base (0x%x) must be greater than "
			"zero.\n", idle_time_base);
	if (idle_time_range < 1)
		fatal(NULL, "error - idle time range (0x%x) must be greater than "
			"zero.\n", idle_time_range);

	if (buffer_reserve < 1)
		fatal(NULL, "error - buffer reserve (0x%x) must be greater than "
			"zero.\n", buffer_reserve);
	if (buffer_len < 1)
		fatal(NULL, "error - buffer size (0x%x) must be greater than "
			"zero.\n", buffer_len);
	if (buffer_reserve > buffer_len)
		fatal(NULL, "error - buffer size (0x%x) must be larger than the "
			"buffer reserver (0x%x)", buffer_len, buffer_reserve);

	if(inet_addr(control_ip) <= (u32) 0)
        fatal(NULL, "error - control ip address (%s) is malformed.\n", 
			control_ip);


#ifdef SYSV
    if (MAX_FD > FD_SETSIZE)
	fatal(NULL, "MAX_FD (= %d) set larger than available FDs (= %d)\n",
		MAX_FD, FD_SETSIZE);
#else
    if (MAX_FD > getdtablesize())
	fatal(NULL, "MAX_FD (= %d) set larger than available FDs (= %d)\n",
		MAX_FD, getdtablesize());
#endif

    if (argc < 1 || argc > 3)
	ok = 0;
    else
    {
	control_port = atoi(args[0]);
	if (control_port == 0 && args[0][0] != '\0')
	    ok = 0;
	if (argc > 1)
	{
	    listen_ip = inet_addr(args[1]);
	    if (listen_ip <= (u32) 0)
		ok = 0;
	}
	else
	    listen_ip = INADDR_ANY;	/* don't care what ip we listen on */
	if (argc > 2)
	{
	    listen_port = atoi(args[2]);
	    if (listen_port == 0 && args[2][0] != '\0')
		ok = 0;
	}
	else
	    listen_port = DONT_CARE;
    }

    if (!ok)
    {
		usage();
    }
    signal(SIGINT, terminate);
    signal(SIGTERM, terminate);

    openlog("Eddie relay", 0, LOG_DAEMON);
    syslog(LOG_INFO, "accepting on %s:%d\n", ntoa(listen_ip), listen_port);

    parent_main_loop(control_port, listen_ip, listen_port);

    return 0;
}
