/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.0, (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.erlang.org/EPL1_0.txt
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Original Code is Erlang-4.7.3, December, 1998.
 * 
 * The Initial Developer of the Original Code is Ericsson Telecom
 * AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
 * Telecom AB. All Rights Reserved.
 * 
 * Contributor(s): ______________________________________.''
 */
/* Copyright (C) 1996 Ericsson Telecom
 * 
 * Created:  15 Jan 1996 by tobbe@erix.ericsson.se
 * 
 * Function: Makes it possible to send and receive Erlang
 *           messages from the (Unix) command line. 
 *           NB: We don't free any memory at all since we only
 *               live for a short while.   
 *
 * Modified: 9 Apr 1999 by tobbe@eddieware.org
 *           I've taken the latest erl_call.c from the
 *           Open Source Erlang 47.4.1 release and added
 *           support for SSH (use the -ssh switch).
 *           NB: This is a hack (yet another). Hopefully,
 *               erl_call will support ssh in the future,
 *               obsoleting this program.
 *
 * $Id: ecmd.c,v 1.1 2000/10/27 22:20:24 dredd Exp $
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef __WIN32__
#include <winsock.h>
#include <direct.h>
#include <windows.h>
#include <winbase.h>
#define MAXHOSTNAMELEN 260

#elif VXWORKS
#include <stdio.h>
#include <string.h>
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <time.h>
/*
#include <symLib.h>
#include <sysSymTbl.h>
#include <sysLib.h>
#include <tickLib.h>
#include <a_out.h>
*/
#include "netdb.h"
#include "erl_malloc.h"

#else
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/param.h> 
#include <netdb.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <stdarg.h>

#include "erl_interface.h"

#if !defined (__WIN32__) && !defined (VXWORKS)
extern int gethostname();
#endif

#ifdef __WIN32__
static void initWinSock(void);
#endif

#ifdef DEBUG
#define D(A) A
#else
#define D(A)
#endif

#define PRINT(t) \
{ \
  print_term(stderr,t); \
  fprintf(stderr,"\n"); \
}

extern char *optarg;
extern int optind,optopt;

/*
 * Some nice global variables
 * (I don't think "nice" is the right word actually... -gordon)
 */
static int remotep=0,startp=0,cookiep=0,modp=0,evalp=0,randomp=0;
static int use_long_name=0; /* indicates if -name was used, else -sname or -n */
static int debugp=0,verbosep=0,modsize=0,haltp=0,use_ssh=0;
static char* progname = NULL;
static char *cookie=NULL,*host=NULL,*node=NULL;
static char *hidden=NULL;
static char *apply=NULL,*erl_script=NULL;
static char *module=NULL,*modname=NULL;
static unsigned char buf[BUFSIZ];
static char argbuf[BUFSIZ];

#if (0) /* ifdef USE_LOCAL_HEAP */
#define MEGA_SIZE 1024*16
static unsigned int megabuf[MEGA_SIZE];
#endif /* USE_LOCAL_HEAP */

static unsigned int *erl_heap=NULL;
static long erl_heap_size=0;

#if defined(VXWORKS)
static int unique_id(void){
    return taskIdSelf();
}
#endif

static void usage_noexit() {
  fprintf(stderr,"\nUsage: %s [-[demqrsvy]] [-c Cookie] [-h HiddenName] \n", progname);
  fprintf(stderr,"            [-x ErlScript] [-a [Mod [Fun [Args]]]]\n");
  fprintf(stderr,"            (-n Node | -sname Node | -name Node)\n\n");
#ifdef __WIN32__
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a \"erlang length [[a,b,c]]\"\n");
#else
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a 'erlang length [[a,b,c]]'\n");
#endif
  fprintf(stderr,"         -c  cookie string; by default read from ~/.erlang.cookie\n");
  fprintf(stderr,"         -d  direct Erlang output to ~/.ecmd.out.<Nodename>\n");
  fprintf(stderr,"         -e  evaluate contents of standard input (e.g echo \"X=1,Y=2,{X,Y}.\"|erl_call -e ...)\n");
  fprintf(stderr,"         -h  specify a name for the erl_call client node\n");
  fprintf(stderr,"         -m  read and compile Erlang module from stdin\n");
  fprintf(stderr,"         -n  name of Erlang node, same as -name\n");
  fprintf(stderr,"         -name  name of Erlang node, expanded to a fully qualified\n");
  fprintf(stderr,"         -sname name of Erlang node, short form will be used\n");
  fprintf(stderr,"         -q  halt the Erlang node (overrides the -s switch)\n");
  fprintf(stderr,"         -r  use a random name for the erl_call client node\n");
  fprintf(stderr,"         -s  start a new Erlang node if necessary\n");
  fprintf(stderr,"         -v  verbose mode, i.e print some information on stderr\n");
  fprintf(stderr,"         -x  use specified erl start script, default is erl\n");
  fprintf(stderr,"         -ssh  Use SSH instead of RSH for remote startup.\n");
}

static void usage_arg(const char *switchname) {
  fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
  usage_noexit();
  exit(1);
}

static void usage_error(const char *switchname) {
  fprintf(stderr, "Illegal argument \'%s\'.\n", switchname);
  usage_noexit();
  exit(1);
}

static void usage() {
  usage_noexit();
  exit(0);
}

/*
 * A hack so we don't use the Erlang library one since it
 * seems to be crashing 
 * Sep'99 geoff@eddieware.org
 */
#define MAX_MESSAGE 2048
static void ecmd_err_quit(const char * fmt, ...)
{
    char s[MAX_MESSAGE];
    va_list args;

    va_start(args, fmt);
    vsprintf(s, fmt, args);
    va_end(args);

    fprintf(stderr, "%s", s);
    fflush(stderr);
    exit(1);
}

/*
 * Get host entry (by address or name)
 */
static struct hostent* get_hostent(char *host)
{
  if (isdigit((int) *host)) {
    struct in_addr ip_addr;
    int b1, b2, b3, b4;
    long addr;
      
    if (sscanf(host, "%d.%d.%d.%d", &b1, &b2, &b3, &b4) != 4) return NULL;
    addr = inet_addr(host);
    ip_addr.s_addr = htonl(addr);
      
    return erl_gethostbyaddr((char *)&ip_addr,sizeof(struct in_addr), AF_INET);
  }

  return erl_gethostbyname(host);
} /* get_hostent */


#define COOKIE_FILE "/.erlang.cookie"

static char *get_cookie(void)
{
  char fname[256],*home,*cookie;
  int fd,len;

  home = getenv("HOME");
  if(!home)
    home = ".";
  strcpy(fname, home);
  strcat(fname, COOKIE_FILE);
  if ((fd = open(fname, O_RDONLY)) < 0)
    erl_err_sys("<ERROR> open cookie file");

  if ((len = read(fd, buf, BUFSIZ)) < 0)
    erl_err_sys("<ERROR> reading cookie file (1)");
  else if (len == BUFSIZ)
    erl_err_sys("<ERROR> reading cookie file (2)");
  
  cookie = (char *) malloc(len+1);
  memcpy(cookie, buf, len);
  cookie[len] = '\0';
  /*
   * Remove trailing newline
   */
  if (cookie[len-1] == '\n')
    cookie[len-1] = '\0';

  if (verbosep) {
    fprintf(stderr,"Got cookie=<");
    write(2,cookie,len);
    fprintf(stderr,">\n");
  }

  return cookie;

} /* get_cookie */

/*
 * Fill the 'argbuf' buffer with the string used 
 * for starting the 'erl_script' program.
 */
static char *build_argbuf(int len, char *host_name)
{

  /*
   * Use 'erl' as the Erlang startup script ?
   */  
  sprintf(&argbuf[len], "exec %s ", erl_script ? erl_script : "erl" );
  len = strlen(argbuf);
  
  /*
   * Turn off interaction with the tty
   */
  sprintf(&argbuf[len], "-noshell -noinput ");
  len = strlen(argbuf);
  
  /*
   * Use long/short node name.
   */
  sprintf(&argbuf[len], 
	  use_long_name ? "-name %s " : "-sname %s ", 
	  node);
  len = strlen(argbuf);
  
  /*
   * Add the cookie to be used.
   */
  if (cookiep) {
    sprintf(&argbuf[len], "-setcookie %s ", cookie);
    len = strlen(argbuf);
  }

  /*
   * Terminate the argbuf string
   */
  argbuf[len] = '\0';

} /* build_argbuf */
  
/* 
 * Start an Erlang system
 */
#define MAX_ARGS 16
static void start_erl_sys(char *node, char *host_name)
{
#ifndef __WIN32__
  int childpid,nullfd,len=0;
  char *argv[4];

  /* Create the process where to execute the Executable 
   */
  if ( (childpid = fork()) < 0 )
    erl_err_sys("<ERROR> when trying to fork");
  else if (childpid > 0) {    /* Parent */
    return;
  }
  else {                      /* Child */

    /*
     * Deal with any debug output.
     */
    if (debugp) {
      char debugfile[MAXPATHLEN];
      char *home=getenv("HOME");
      int dbgfd=-1;
      sprintf(debugfile,"%s/%s.%s",home,".ecmd.out",node);
      if ((dbgfd=open(debugfile, O_WRONLY | O_CREAT | O_APPEND, 0644)) >= 0) {
	time_t t = time(NULL);
	dup2(dbgfd,1);
	dup2(dbgfd,2);
	fprintf(stderr,"\n\n===== Log started ======\n%s \n",ctime(&t));
	fprintf(stderr,"ecmd: %s %s %s\n",argv[0],argv[1],argv[2]);
      }
    }    

    if (!remotep) {
      /*
       * Local system !!
       */

      build_argbuf(len,host_name);

      if (verbosep)
	fprintf(stderr,"sh -c %s\n", argbuf);

      argv[0] = "sh";
      argv[1] = "-c";
      argv[2] = argbuf;
      argv[3] = NULL;
      /* 
       * Start Erlang
       */
      execvp(argv[0], argv);
    }
    else {

      /*
       * Remote system !!
       */

      /*sprintf(&argbuf[len], "sh -c ");
       *len = strlen(argbuf);
       */
      build_argbuf(len,host_name);

      /*
       * Use RSH or SSH ?
       */
      if (use_ssh) {
	argv[0] = "ssh";
	argv[1] = "-n";
	argv[2] = "-f";
	argv[3] = host_name;
	argv[4] = argbuf;
	argv[5] = NULL;

	if (verbosep)
	  fprintf(stderr,"%s %s %s %s %s\n", argv[0], argv[1], argv[2], argv[3], argv[4]);
      }
      else {
	argv[0] = "rsh";
	argv[1] = "-n";
	argv[2] = host_name;
	argv[3] = argbuf;
	argv[4] = NULL;

	if (verbosep)
	  fprintf(stderr,"%s %s %s %s\n", argv[0], argv[1], argv[2], argv[3]);
      }
      
      /* 
       * Start Erlang
       */
      execvp(argv[0], argv);

      if (debugp) {
	if (use_ssh) 
	  fprintf(stderr,"exec failed: (%d) %s %s %s %s %s\n",
		  errno,argv[0],argv[1],argv[2],argv[3],argv[4]);
	else
	  fprintf(stderr,"exec failed: (%d) %s %s %s %s\n",
		  errno,argv[0],argv[1],argv[2],argv[3]);
      }
      
    }
    /* NOT REACHED */
  }
#endif /* not __WIN32__ */
} /* start_erl_sys */


/* 
 * This function does only return on success.
 */
static int do_connect(char *nodename, char *node, char *host_name)
{
  int retry=1, started=0, did_start=0;
  int fd;

  while (retry) {
    retry = 0;
    if ((fd = erl_connect(nodename)) < 0)
      switch (fd) {
      case ERL_NO_DAEMON:
	ecmd_err_quit("<ERROR> No epmd running !");	
	break;
      case ERL_CONNECT_FAIL:
	ecmd_err_quit("<ERROR> Error connect failed !");	
	break;
      case ERL_TIMEOUT:
	ecmd_err_quit("<ERROR> Connect timed out!");
	break;
      case ERL_NO_PORT:
	if (!started++) {
	  start_erl_sys(node, host_name);
	  did_start = 1;
	}
	retry = 1;
	break;
      default:
	ecmd_err_quit("<ERROR> Error during connect, got: %d !", fd);	
	break;
      }
  } /* while */

  return fd;

} /* do_connect */

#define SKIP_SPACE(s) while(isspace((int) *(s))) (s)++
#define EAT(s) while (!isspace((int) *(s)) && (*(s) != '\0')) (s)++

static void split_apply_string(char *str, 
			       char **mod, 
			       char **fun, 
			       char **args)
{
  char *begin=str;
  char *start="start";
  char *empty_list="[]";
  int len;

  SKIP_SPACE(str);
  if (*str == '\0') 
    ecmd_err_quit("<ERROR> Wrong format of apply string (1) !");
  EAT(str);
  len = str-begin;
  *mod = (char *) calloc(len + 1, sizeof(char));
  memcpy(*mod, begin, len);

  SKIP_SPACE(str);
  if (*str == '\0') {
    *fun = (char *) calloc(strlen(start)+1, sizeof(char));
    strcpy(*fun, start);
    *args = (char *) calloc(strlen(empty_list)+1, sizeof(char));
    strcpy(*args, empty_list);
    return;
  }
  begin = str;
  EAT(str);
  len = str-begin;
  *fun = (char *) calloc(len + 1, sizeof(char));
  memcpy(*fun, begin, len);

  SKIP_SPACE(str);
  if (*str == '\0') {
    *args = (char *) calloc(strlen(empty_list)+1, sizeof(char));
    strcpy(*args, empty_list);
    return;
  }

  *args = (char *) calloc(strlen(str) + 1, sizeof(char));
  strcpy(*args, str);
  
  return;

} /* split_apply_string */


/* 
 * Read from stdin until EOF is reached.
 * Allocate the buffer needed.
 */
static int read_stdin(char **buf)
{
  char *tmp;
  int bsize=BUFSIZ,len=0,i;

  tmp = (char *) malloc(bsize);
  len = 0;
  while (1) {
    if ((i = read(0, &tmp[len], bsize-len)) < 0)
      erl_err_sys("<ERROR> when reading stdin");
    else if (i == 0) 
      break;
    else {
      len += i;
      if ((len+50) > bsize) {
	bsize = len * 2;
	tmp = (char *) realloc(tmp, bsize);
      }
      else
	continue;
    }
  } /* while */

  *buf = tmp;
  return len;

} /* read_stdin */

/*
 * Get the module from stdin.
 */
static int get_module(char **mbuf, char **mname)
{
  char *tmp;
  int len,i;

  len = read_stdin(mbuf);
  /*
   * Now, get the module name.
   */
  if ((tmp = strstr(*mbuf, "-module(")) != NULL) {
    char *start;
    tmp += strlen("-module(");
    while ((*tmp) == ' ') tmp++; /* eat space */
    start = tmp;
    while (1) {
      if (isalnum((int) *tmp) || (*tmp == '_')) {
	tmp++;
	continue;
      }
      else 
	break;
    } /* while */
    i = tmp - start;
    *mname = (char *) calloc(i+1, sizeof(char));
    memcpy(*mname, start, i);
  }

  return len;

} /* get_module */

/*
 * --- M A I N ---
 */
#if !defined(VXWORKS)
int main(int argc, char *argv[])
#else
int erl_call(int argc, char **argv)
#endif
{
    int i,fd,creation;
    struct hostent *hp;
    char host_name[MAXHOSTNAMELEN];
    char nodename[MAXNODELEN];
    char *p=NULL;
    char *ct=NULL; /* temporary used when truncating nodename */
    ETERM *reply;

#if (0) /* ifdef USE_LOCAL_HEAP */
    erl_heap = megabuf;
    erl_heap_size = MEGA_SIZE;
#endif /* USE_LOCAL_HEAP */

    progname = argv[0];

    /* Get the command line options */
    i=1;
    while (i < argc) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
	    case 's':
	      if (strcmp(argv[i], "-sname") == 0) { /* -sname NAME */
		if (i+1 >= argc)
		  usage_arg("-sname ");

		node = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(node, argv[i+1]);
		i++;
		use_long_name = 0;
	      }
	      else if (strcmp(argv[i],"-s") == 0) {
		startp = 1;
	      }
	      else if (strcmp(argv[i], "-ssh") == 0) {
		use_ssh = 1;
	      }
	      else
		usage_error(argv[i]);
	      break;

	    case 'q':
	      if (strlen(argv[i]) == 2) {
		haltp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'v':
	      if (strlen(argv[i]) == 2) {
	      verbosep = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'y':
	      if (strlen(argv[i]) == 2) {
	      use_ssh = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'd':
	      if (strlen(argv[i]) == 2) {
	      debugp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'r':
	      if (strlen(argv[i]) == 2) {
	      randomp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'e':
	      if (strlen(argv[i]) == 2) {
	      evalp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'm':
	      if (strlen(argv[i]) == 2) {
	      modp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'c':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-c ");
		cookiep = 1;
		cookie = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(cookie, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;
	      
	    case 'n':
	      if (strcmp(argv[i], "-name") == 0) {  /* -name NAME */
		if (i+1 >= argc)
		  usage_arg("-name ");

		node = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(node, argv[i+1]);
		i++;
		use_long_name = 1;
	      }
	      else if (strcmp(argv[i],"-n") == 0) { /* -n NAME */
		if (i+1 >= argc)
		  usage_arg("-n ");
		
		node = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(node, argv[i+1]);
		i++;
		use_long_name = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'h':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-h ");
		hidden = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(hidden, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'x':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-x ");
		erl_script = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(erl_script, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'a':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-a ");
		apply = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(apply, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case '?':
	    default:
	      usage();
	    }
	}
	else
	  usage();
	i++;

    } /* while */
    
	
    /*
     * Can't have them both !
     */
    if (modp && evalp)
      usage();

    /*
     * Read an Erlang module from stdin.
     */
    if (modp) 
      modsize = get_module(&module, &modname);

    if (verbosep || debugp)
      fprintf(stderr,"Node = %s\nCookie = %s\n"
	      "Flags = %s %s %s\n"
	      "Module: name = %s , size = %d\n"
	      "Apply = %s\n",
	      node,
	      (cookie ? cookie : ""),
	      (startp? "startp" : ""),
	      (verbosep? "verbosep" : ""),
	      (debugp? "debugp" : ""),
	      (modname ? modname : ""), modsize,
	      (apply ? apply : "" ));

    /* 
     * What we, at least, requires !
     */
    if (node == NULL)
      usage();

    if (!cookiep)
      cookie = NULL;

    creation = (time(NULL) % 3) + 1; /* "random" in range 1-3 */

    /* Initiate the erl_interface stuff */
    erl_init((Erl_Heap *) erl_heap, erl_heap_size);

    if (hidden == NULL) {
      /* As default we are c17@gethostname */
      i = randomp ? (time(NULL) % 997) : 17;
      hidden = (char *) malloc(3 + 2 ); /* c17 or cXYZ */
#if defined(VXWORKS)
      sprintf(hidden, "c%d",
	  i < 0 ?  (int) unique_id() : i);
#else
      sprintf(hidden, "c%d",
	  i < 0 ?  (int) getpid() : i);
#endif
    }
    {
      /* A name for our hidden node was specified */
      char h_hostname[MAXHOSTNAMELEN];
      char h_nodename[MAXNODELEN];
      char *h_alivename=hidden;
      struct in_addr h_ipadr;
      char* ct;

#ifdef __WIN32__
      /*
       * XXX Extremly ugly, but needed to get erl_gethostbyname() below
       * to work.
       */
      initWinSock();
#endif

      gethostname(h_hostname, MAXHOSTLEN);
      if ((hp = erl_gethostbyname(h_hostname)) == 0) {
	  ecmd_err_quit("<ERROR> erl_call: Can't gethostbyname()");
      }
      if (use_long_name == 0) /* shortnames */
	if ((ct = strchr(hp->h_name, '.')) != NULL)
	  *ct = '\0';  
      strcpy(h_hostname, hp->h_name);
      memcpy(&h_ipadr.s_addr, *hp->h_addr_list, sizeof(struct in_addr));
      sprintf(h_nodename, "%s@%s", h_alivename, h_hostname);
      
      if (!erl_connect_xinit(h_hostname, h_alivename, h_nodename,
			     (Erl_IpAddr) &h_ipadr, cookie, creation))
	ecmd_err_quit("<ERROR> when trying to xinit connect !");
    }
    if ((p = strchr((const char *) node, (int) '@')) == 0) {
      strcpy(host_name, erl_thishostname());
      host = host_name;
    }
    else {
      *p = 0;
      host = p+1;
    }

    /* 
     * Expand name to a real name (may be ip-address) 
     */
    if ((hp = get_hostent(host)) == 0)
      ecmd_err_quit("<ERROR> get_host_ent");
    /* 
     * Remote or local ?
     */
    remotep = (strcmp(hp->h_name, erl_thishostname()) == 0) ? 0 : 1;
    /* 
     * Use shortnames ? 
     */
    if (use_long_name == 0) 
      if ((ct = strchr(hp->h_name, '.')) != NULL)
	*ct = '\0';  
    strcpy(host_name, hp->h_name);
    sprintf(nodename, "%s@%s", node, host_name);

    /* 
     * Try to connect. Start an Erlang system if the
     * start option is on and no system is running.
     */
    if (startp && !haltp) {
      fd = do_connect(nodename, node, host_name);
    }
    else if ((fd = erl_connect(nodename)) < 0) {
      /* We failed to connect ourself */
      if (haltp)
	  exit(0);
      else 
	  ecmd_err_quit("<ERROR> erl_connect failed");
    }

    /* If we are connected and the halt switch is set */
    if (fd && haltp) {
      erl_rpc(fd, "erlang", "halt", erl_format("[]"));
      exit(0);
    }

    if (verbosep)
      fprintf(stderr,"We are now connected to node: <%s> !\n",nodename);

    /*
     * Compile the module read from stdin.
     */
    if (modp && (modname != NULL)) {
      char fname[256];

      strcpy(fname, modname);
      strcat(fname, ".erl");
      
      if (!(reply = erl_rpc(fd, "file", "write_file", 
			   erl_format("[~s,~w]", fname, 
				      erl_mk_binary(module, modsize)))))
	erl_err_msg("<ERROR> when writing source file: %s !\n", fname);

      if (!(reply = erl_rpc(fd, "c", "c", erl_format("[~a,[]]", modname))))
	erl_err_msg("<ERROR> when compiling file: %s !\n", fname);

      if (!erl_match(erl_format("{ok,_}"), reply))
	erl_err_msg("<ERROR> compiler errors !\n");
    }
    /*
     * Eval the Erlang functions read from stdin/
     */
    if (evalp) {
      char *evalbuf;
      int len;

      len = read_stdin(&evalbuf);
      if (!(reply = erl_rpc(fd, "lib", "eval_str", 
			    erl_format("[~w]", erl_mk_binary(evalbuf,len)))))
	erl_err_msg("<ERROR> when evaluating input: %s !\n", evalbuf);
      else
	erl_print_term(stdout,reply);
    }
    /*
     * Any Erlang call to be made ?
     */
    if (apply != NULL) {
      char *mod,*fun,*args;
      ETERM *e;

      split_apply_string(apply, &mod, &fun, &args);
      if (verbosep)
	fprintf(stderr,"Mod = %s, Fun = %s, Args = %s\n", mod, fun, args);

      if (!(e = erl_format(args))) 
	exit(-1);

      if (!(reply = erl_rpc(fd, mod, fun, e)))
	exit(-1);
      else
	erl_print_term(stdout,reply);
    }

    return(0);
}

#ifdef __WIN32__
/*
 * XXX This should not be here.  This is a quick fix to make erl_call
 * work at all on Windows NT.
 */
static void
initWinSock(void)
{
    WORD wVersionRequested;  
    WSADATA wsaData; 
    int err; 
    static int initialized;

    wVersionRequested = MAKEWORD(1, 1); 
    if (!initialized) {
	initialized = 1;
	err = WSAStartup(wVersionRequested, &wsaData); 
 
	if (err != 0) {
	    erl_err_msg("<ERROR> erl_connect_init: Can't initialize windows sockets: %d",
			err);
	}
  
	if ( LOBYTE( wsaData.wVersion ) != 1 || 
	    HIBYTE( wsaData.wVersion ) != 1 ) { 
	    erl_err_msg("<ERROR> erl_connect_init: This version of windows sockets "
			"not supported");
	    WSACleanup(); 
	}
    }
}
#endif
