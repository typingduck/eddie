
/* initial number of children pre-forked, and minimum number persisting
 * during idle times */

#ifndef MIN_CHILDREN
#define MIN_CHILDREN 8
#endif

/* Maximum number of children that may be forked in total.  Each child uses
 * up a fd in the parent, so MAX_CHILDREN <= MAX_FD - 4 (the 4 is stdin,
 * stdout, stderr and the listen port) */
#ifndef MAX_CHILDREN
#define MAX_CHILDREN 252
#endif

/* Number of connections each child will accept.  Each connection uses up 2
 * fds in the child, so MAX_CONNECTIONS*2 <= MAX_FD - 6 (the 6 is stdin,
 * stdout, stderr, the listen port, the socket to the parent  and the
 * socket to the erlang control port) */
#ifndef MAX_CONNECTIONS
#define MAX_CONNECTIONS 16
#endif

/* idle surplus children will suicide after a random duration:
	(IDLE_TIME_BASE + 0.0->1.0 * IDLE_TIME_RANGE) seconds
*/
#ifndef IDLE_TIME_BASE
#define IDLE_TIME_BASE 10
#endif

#ifndef IDLE_TIME_RANGE
#define IDLE_TIME_RANGE 30
#endif

#ifndef REQ_LEN
#define REQ_LEN 512
#endif


#ifndef DEF_ALARM_TIME
#define DEF_ALARM_TIME 3
#endif


/* shared input buffer length (shared by all connections handled by a
 * single child, not between children) */
#define BUF_LEN 0x20000

/* this quantity is multiplied directly with the current BUF_LEN, without
 * parentheses, so we can have fractions even though it's integer
 * arithmetic */
#define BUF_GROW_RATE 3/2

/* How close to the end of the buffer we get before we start thinking about
 * wrapping to the start again.  Also used as a reserve for resizing the
 * buffer - when we do an append_data, we ensure that there's at least an
 * additional BUF_RESERVE bytes, too (and if not, we grow/compact the
 * buffer) */
#define BUF_RESERVE 0x5120

/* This is the IP address where we will find the controller for the 
 * relay.  */
#define CONTROL_IP "127.0.0.1"

extern int min_children;
extern int max_children;
extern int max_connections;
extern int idle_time_base;
extern int idle_time_range;
extern int buffer_len;
extern float buffer_grow_rate;
extern int buffer_reserve;
extern char *control_ip;
extern int debug;
extern int alarm_time;
