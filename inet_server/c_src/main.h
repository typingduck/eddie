#include "server.h"

extern char *glob_program_name;

void fatal(Server *serv, char *msg, ...);

/* Uncomment the following to have debugging messages */
#define DEBUG 1  /**/

