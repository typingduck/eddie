/*
The contents of this file are subject to the Erlang Public License,
Version 1.0, (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.eddieware.org/EPL

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

The Original Code is Eddie-0.83b1.

The Initial Developer of the Original Code is Ericsson Telecom
AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
Telecom AB. All Rights Reserved.

Contributor(s): ______________________________________.
Mar 99 - maurice@eddieware.org - Fixes for Solaris/FreeBSD/Linux
*/

/*
WARNING: If this code is converted to continuous operation 
something will have to be done to handle incoming packets
which are currently ignored.
*/
 
#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <syslog.h>
#include "strs.h"
#include "memfs.h"
#include "ints.h"

#if defined(NEED_SYS_TYPES_H) || defined(HAVE_SYS_TYPES_H)
# include <sys/types.h>
#endif

#ifdef USE_DLPI
# ifdef HAVE_SYS_DLPI_H
#  include <sys/dlpi.h>
# else
#  error "Cannot find dlpi.h"
# endif
# include <stropts.h>
#endif

#ifdef NEED_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef NEED_NETINET_IN_H
# include <netinet/in.h>
#endif

#ifdef HAVE_NET_IF_ARP_H
# include <net/if_arp.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#elif HAVE_SYS_FCNTL_H
# include <sys/fcntl.h>
#else
# error "Could not find fcntl header file"
#endif

#include <sys/ioctl.h>

#ifndef HAVE_ETHER_ADDR_LEN
# ifdef HAVE_ETHERADDRL
#  define ETHER_ADDR_LEN ETHERADDRL
# else
#    define ETHER_ADDR_LEN 6
#  endif
# endif

#include <net/if.h>

#ifdef HAVE_NET_IF_PACKET_H
# include <net/if_packet.h>
#endif

#ifdef NEED_SYS_ETHERNET_H
# include <sys/ethernet.h>
#endif

#if defined(NEED_NET_ETHERNET)
# include <net/ethernet.h>
#endif

#if defined(NEED_NETINET_IF_ETHER_H)
# include <netinet/if_ether.h>
#endif

#ifndef HAVE_ether_header
# ifdef HAVE_ethhdr
#  define ether_header ethhdr
#  define ether_dhost h_dest 
#  define ether_shost h_source 
#  define ether_type h_proto 
# else
#  error "Cannot construct ether_header structure"
# endif
#endif

#ifdef HAVE_NET_BPF_H
# include <sys/time.h>
# include <net/bpf.h>
#endif

# include <math.h>

#include "rights.h"
#include "garp.h" 
#include "ifget.h"
#include "err.h"

int hwaddr(char *s,char *hwaddr);
#ifndef ETHER_HEADER_USES_ETHER_ADDR
# define HWADDR(x,y) hwaddr(x, y)
#else
/* this is very ugly */
# define HWADDR(x,y) hwaddr((char *) &(x), y)
#endif
int ipaddr(char *,char *);

static char *cpu_vendor_os = CPU_VENDOR_OS;

#if defined(USE_BPF)
# define MAX_TRY_FILT 16
# define BPFBASE "/dev/bpf"

int find_bpf(void);		/* find a bpf device */
int set_bpf_interface(int, char *);	/* set physical interface to be used */
int send_bpf(int , char *, int);	/* send a packet */
#endif 

#if defined(USE_DLPI)
# define DLPIBASE "/dev/"
int open_dlpi(char *);		/* open the dlpi device */
int strioctl(int, int, int, int, char *);
int send_dlpi(int, char *, int);
#endif

int main(int argc,char *argv[]) 
{
	char frame[60];
	struct ether_header *ether_header;
	struct eth_arphdr *eth_arphdr;
	int fd;
	int i;
	char *ifarg;
	char *hwarg;
	char *iparg;

#if defined(USE_SOCKET)
	struct sockaddr sa;
#endif
  
	if(argc != 4) 
	{
		fprintf(stderr,"Usage: garp interface hwaddr ip-address\n");
		exit(2);
	}

	syslog(LOG_INFO, "Eddie: %s %s %s %s", argv[0], argv[1], argv[2], argv[3]);

	check_rights(argv[0]);

	ifarg =  argv[1];
	hwarg = argv[2];
	iparg = argv[3];

	memset((void *)frame,0,sizeof(frame));
	ether_header=(struct ether_header *)frame;
	HWADDR(ether_header->ether_dhost,"ff:ff:ff:ff:ff:ff");
	HWADDR(ether_header->ether_shost,hwarg);
	ether_header->ether_type=htons(ETHERTYPE_ARP);
  
	eth_arphdr=(struct eth_arphdr *)(frame+sizeof(struct ether_header));
  
	eth_arphdr->ar_hrd=htons(ARPHRD_ETHER);
	eth_arphdr->ar_pro=htons(ETHERTYPE_IP);
	eth_arphdr->ar_hln=6;
	eth_arphdr->ar_pln=4;
	eth_arphdr->ar_op=htons(ARPOP_REQUEST);
	HWADDR(eth_arphdr->ar_sha,hwarg);
	ipaddr(eth_arphdr->ar_sip,iparg);
	HWADDR(eth_arphdr->ar_tha,"00:00:00:00:00:00");
	ipaddr(eth_arphdr->ar_tip,iparg);
#ifdef DEBUG
	for (i = 0; i < sizeof(frame); i++)
	{
		if (!(i % 20))
			printf("\n");
		printf("%02x ", frame[i] & 0xff);
	}
	printf("\n");
#endif
 
#if defined(USE_SOCKET)
	if((fd=socket(AF_INET,SOCK_PACKET,htons(ETH_P_802_3))) < 0) 
	{
		perror("garp socket");
		exit(2);
	}
  
	fcntl(fd,F_SETFL,O_NDELAY);
	sa.sa_family=AF_INET;
	strcpy(sa.sa_data,ifarg);
  
	if(sendto(fd,frame,sizeof(frame),0,&sa,sizeof(sa)) < 0) 
	{
		perror("garp sendto");
		exit(2);
	}
#elif defined(USE_BPF)
	fd = find_bpf();
        if (fd == -1)
        {
                perror("Unable to open bpf interface");
                exit(1);
        }
        if (set_bpf_interface(fd, ifarg) == -1)
        {
                perror("Unable to setup bpf interface");
                exit(1);
        }
	if (send_bpf(fd,frame,sizeof(frame)) == -1)
		exit(1);
#elif defined(USE_DLPI)
	fd = open_dlpi(ifarg);
        if (fd == -1)
        {
                perror("Unable to open dlpi interface");
                exit(1);
        }
        if (set_dlpi_interface(fd, ifarg) == -1)
        {
                perror("Unable to setup dlpi interface");
                exit(1);
        }
	if (send_dlpi(fd, frame, sizeof(frame)) == -1)
		exit(1);
#else
# error "Raw packet interface undefined"
#endif

	return(0);
}

int hwaddr(char *s,char *hwaddr)
{
	int a,b,c,d,e,f;
  
	if(sscanf(hwaddr,"%x:%x:%x:%x:%x:%x",&a,&b,&c,&d,&e,&f) != 6) 
		return(-1);
	else 
	{
		s[0]=(unsigned char)a;
		s[1]=(unsigned char)b;
		s[2]=(unsigned char)c;
		s[3]=(unsigned char)d;
		s[4]=(unsigned char)e;
		s[5]=(unsigned char)f;
		return(0);
	}
}

int ipaddr(char *s,char *ipaddr)  
{
	int a,b,c,d;
  
	if(sscanf(ipaddr,"%d.%d.%d.%d",&a,&b,&c,&d) != 4) 
		return(-1);
	else 
	{
		s[0]=(unsigned char)a;
		s[1]=(unsigned char)b;
		s[2]=(unsigned char)c;
		s[3]=(unsigned char)d;      
		return(0);
	}
}

#if defined(USE_BPF)

/* try to open the device driver return -1 on failure */
int find_bpf(void)
{
	int i;
	char *bpffilename;
	int bpffd;
	int len;

	/* Allocate space for file name */
	len = strlen(BPFBASE) + (int) log(MAX_TRY_FILT) +2;
	bpffilename = (char *) malloc (len);
	if (!bpffilename)
	{
		errno = ENOMEM;
		return(-1);
	}

	/* loop through file names */
	for (i = 0; i < MAX_TRY_FILT; i++)
	{
		sprintf(bpffilename, "%s%d", BPFBASE, i);
retry:
		bpffd = open(bpffilename, O_RDWR, 0666);
		if (bpffd != -1)
			return(bpffd);
		else
		{
			/* If interrupted before open */
			if (errno == EINTR)
				goto retry;
			/* if not busy give up - probably unable to open */
			/* interface */
			if (errno == EBUSY)
				continue;
			/* failed and no good way to handle it */
			return(-1);
		}
	}
	/* failed to find a suitable interface so give up */
	errno = EBUSY;
	return(-1);
}

/* set bpf interface - return -1 on failure, return fd on success */
int set_bpf_interface(int fd, char *ifname)
{
	struct ifreq ifr;
	strncpy(ifr.ifr_name, ifname, IFNAMSIZ);
	if (ioctl(fd, BIOCSETIF, &ifr) == -1)
	{
		/* unable to set interface */
		return(-1);
	}
	return(fd);
}

int send_bpf(int fd, char *frame, int sz)
{
	if (write(fd,frame,sz) != sz)
		return(-1);
	return(sz);
}

#endif

#ifdef USE_DLPI

# define DLPI_MAXDLBUF 8192
# define DLPI_MAXDLADDR 1024
# define SAP 0x0806		/* ARP protocol */

int open_dlpi(char *ifname)
{
	char *ifstr;
	char *ifbase;
	int l;
	int fd;
	l = strcspn(ifname,"0123456789.");
	ifbase = calloc(l+1, sizeof(char));
	if (!ifbase)
	{
		return(-1);
	}
	strncpy(ifbase, ifname, l);
	ifstr = malloc(strlen(DLPIBASE)+strlen(ifbase)+1);
	if (!ifstr)
	{
		return(-1);
	}
	strcpy(ifstr, DLPIBASE);
	strcat(ifstr, ifbase);
#ifdef DEBUG
	fprintf(stderr, "if: %s\n", ifstr);
#endif
	fd = open(ifstr, O_RDWR);
	if (fd == -1)
	{
		return(-1);
	}
	return(fd);
}

/* set dlpi interface - return -1 on failure, return fd on success */
int set_dlpi_interface(int fd, char *ifname)
{
	int l;
	char *unit;
	dl_attach_req_t attach_req;
	dl_bind_req_t bind_req;
	struct strbuf ctl;
	int flags;
	long buf [DLPI_MAXDLBUF];

	l = strcspn(ifname,"0123456789.");
	unit = ifname + l;
#ifdef DEBUG
	fprintf(stderr, "unit: %s\n", unit);
#endif
	
	attach_req.dl_primitive = DL_ATTACH_REQ;
	attach_req.dl_ppa = atoi(unit);
#ifdef DEBUG
	fprintf(stderr, "ppa: %d\n", attach_req.dl_ppa);
#endif

	ctl.maxlen = 0;
	ctl.len = sizeof(attach_req);
	ctl.buf = (char *) &attach_req;

	if (putmsg(fd, &ctl, (struct strbuf*) NULL, 0) < 0)
		return(-1);

	ctl.maxlen = DLPI_MAXDLBUF;
	ctl.len = 0;
	ctl.buf = (char *) buf;

	flags = 0;

	if (getmsg(fd, &ctl, (struct strbuf*) NULL, &flags) < 0)
		return(-1);

	if (*buf != DL_OK_ACK)
	{
		errno = 0;
		return(-1);
	}
	
	bind_req.dl_primitive = DL_BIND_REQ;
	bind_req.dl_sap = SAP;
	bind_req.dl_service_mode = DL_CLDLS;	/* Datagram? */
	bind_req.dl_max_conind = 0;
	bind_req.dl_conn_mgmt = 0;
	bind_req.dl_xidtest_flg = 0;
#ifdef DEBUG
	fprintf(stderr, "sap: %d\n", bind_req.dl_sap);
#endif

	ctl.maxlen = 0;
	ctl.len = sizeof(bind_req);
	ctl.buf = (char *) &bind_req;

	if (putmsg(fd, &ctl, (struct strbuf*) NULL, 0) < 0)
		return(-1);

	ctl.maxlen = DLPI_MAXDLBUF;
	ctl.len = 0;
	ctl.buf = (char *) buf;

	flags = 0;

	if (getmsg(fd, &ctl, (struct strbuf*) NULL, &flags) < 0)
		return(-1);

	if (*buf != DL_BIND_ACK)
	{
		errno = 0;
		return(-1);
	}

#ifdef HAVE_DLIOCRAW
	/* sun supports raw mode */
	if (strioctl(fd, DLIOCRAW, -1, 0, NULL) < 0)
	{
		return(-1);
	}
#endif

	return(0);
}

#ifdef HAVE_DLIOCRAW
int strioctl(int fd, int cmd, int timeout, int len, char *dp)
{
	struct strioctl sioc;
	int rc;
	sioc.ic_cmd = cmd;
	sioc.ic_timout = timeout;
	sioc.ic_len = len;
	sioc.ic_dp = dp;
	rc = ioctl(fd, I_STR, &sioc);
	if (rc < 0)
		return(rc);
	return(sioc.ic_len);
}
#endif

int send_dlpi(int fd, char *frame, int sz)
{
#ifdef HAVE_DLIOCRAW
	if (write(fd,frame,sz) != sz)
		return(-1);
#else
# error "General DLPI not implemented"
#endif
	return(sz);
}
#endif
