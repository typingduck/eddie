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

#include "config.h"

#include "ints.h"
#include "memfs.h"
#include "strs.h"

#include "rights.h"

#include <stdio.h>
#include <errno.h>
#include <syslog.h>
#include "ifget.h"
#include "err.h"

#ifdef NEED_SYS_TYPES_H
# include <sys/types.h>
#endif

#ifdef NEED_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef NEED_NETINET_IN_H
# include <netinet/in.h>
#endif

#ifdef NEED_NET_ETHERNET_H
# include <net/ethernet.h>
#endif
#ifdef NEED_SYS_ETHERNET_H
# include <sys/ethernet.h>
#endif 
#ifdef NEED_NET_IF_ARP_H
# include <net/if_arp.h>
#endif 

#ifndef HAVE_ETHER_ADDR_LEN
# ifdef HAVE_ETHERADDRL
#  define ETHER_ADDR_LEN ETHERADDRL
# else
#    error "Unable to determine value of ETHER_ADDR_LEN"
#  endif
# endif

#ifdef NEED_SYS_SOCKIO_H
# include <sys/sockio.h>
#endif
#ifdef NEED_SYS_SOCKETIO_H
# include <sys/socketio.h>
#endif
#ifdef NEED_LINUX_SOCKIOS_H
# include <linux/sockios.h>
#endif

#include <net/if.h>
#include <sys/ioctl.h>
#include <arpa/inet.h>

#ifdef NEED_NET_IF_DL_H
# include <net/if_dl.h>
#endif
#ifdef NEED_NET_IF_ARP
# include <net/if_arp.h>
#endif
#ifdef NEED_NETINET_IF_ETHER_H
# include <netinet/if_ether.h>
#endif

static char *cpu_vendor_os = CPU_VENDOR_OS;

/* retrieve iferq structures using SIOCGIFCONF */
/* this code based on Stevens Unix Network Programming */
/* vol 1 p 434 */

char *getifreq(int sockfd, int *len)
{
	char *buf;
	int bufLen;
	int nbuf;
	struct ifconf ifc;

	*len = 0;
	nbuf=MIN_IFS;

	while (nbuf < MAX_IFS)
	{
		bufLen=nbuf*sizeof(struct ifreq);
		if((buf=malloc(bufLen)) == NULL)
			error(2,"ifget: Out of memory");

		ifc.ifc_len=bufLen;
		ifc.ifc_buf=buf;

		if(ioctl(sockfd,SIOCGIFCONF,&ifc) < 0) 
		{
			if(errno != EINVAL || *len != 0)
				error(2,"ifget: ioctl error");
		} 
		else 
		{
			if(ifc.ifc_len == *len)
			{
				*len = ifc.ifc_len;
				break;
			}
			*len=ifc.ifc_len;
		}

		free(buf);
		buf = NULL;
		nbuf+=INC_IFS;
	}
	return(buf);
}

print_arp(u_int8_t eaddr[ETHER_ADDR_LEN])
{
        int i;
        printf("%02x", eaddr[0]);
        for (i = 1 ; i < ETHER_ADDR_LEN; i++)
        {
                printf(":%02X", eaddr[i]);
        }
}

void ifget(int sockfd, enum mode mode, char *name)
{
	char *buf,*ptr,*colon;
	int len;
	int l;
	struct ifreq *ifr;
	struct sockaddr_in *sa_in, *sin;
#ifdef  HAVE_arpreq
	struct arpreq arpreq;
#endif
#ifndef USE_IFALIAS
	int cnt = 0;
#endif

	buf = getifreq(sockfd, &len);

	for(ptr=buf;ptr < buf+len;) 
	{
		ifr=(struct ifreq *)ptr;

		if (mode == IP) 
		{
			sa_in=(struct sockaddr_in *)&ifr->ifr_addr;
			if(strncmp(name,inet_ntoa(sa_in->sin_addr), IFNAMSIZ) == 0)
				printf("%s\n",ifr->ifr_name);
#ifdef DEBUG
			fprintf(stderr, "%s %s %s\n", name, inet_ntoa(sa_in->sin_addr), ifr->ifr_name);
#endif
		} 
		else if (mode == IF) 
		{
			/* This is rather questionable on linux and solaris */
			/* It will probably go in later versions */
			/* We return names of devices which are aliased. */
			/* Under BSD this makes no sense so we fake it */
			/* Note: the numbers under BSD are just placeholders */
#ifdef USE_IFALIAS
			if((colon=memchr(ifr->ifr_name,':',strlen(ifr->ifr_name))) != NULL) 
			{
				*colon='\0';

				if(strncmp(name,ifr->ifr_name, IFNAMSIZ) == 0)
					printf("%s\n",++colon);
			}
#else
			if(strncmp(name,ifr->ifr_name, IFNAMSIZ) == 0)
			{
				sa_in=(struct sockaddr_in *)&ifr->ifr_addr;
				if (sa_in->sin_family == AF_INET)
				{
					if (cnt > 0)
					{
						printf("%d\n", cnt);
					}
					cnt++;
				}
			}
#endif
#ifdef DEBUG
			fprintf(stderr, "%s %s\n", name, ifr->ifr_name);
#endif
		} 
		else if (mode == HW)
		{
			if(strncmp(name,ifr->ifr_name, IFNAMSIZ) == 0) 
			{
#ifdef HAVE_sockaddr_dl
				print_arp(LLADDR((struct sockaddr_dl *) &ifr->ifr_addr));
#elif defined(HAVE_SIOCGIFHWADDR)
				if(ioctl(sockfd,SIOCGIFHWADDR,ifr) == 0) 
				{
					print_arp(ifr->ifr_hwaddr.sa_data);
				}
#elif defined(HAVE_arpreq)
				memset(&arpreq, 0, sizeof(arpreq));
				sin = ((struct sockaddr_in *) &arpreq.arp_pa);
				memset(sin, 0, sizeof(*sin));
				sa_in=(struct sockaddr_in *)&ifr->ifr_addr;
				memcpy(&sin->sin_addr, &sa_in->sin_addr, sizeof(struct in_addr));
				if(ioctl(sockfd,SIOCGARP,&arpreq) == 0) 
				{
					print_arp(arpreq.arp_ha.sa_data);
				}
#else
# error "Unable to work out hardware address"
#endif
			break;
			}
		}
		else
		{
			error(3, "Invalid mode");
		}

#ifdef SA_LEN_IN_SOCKADDR
		l = ifr->ifr_addr.sa_len + sizeof(ifr->ifr_name);
                if (l < sizeof(*ifr))
                        l = sizeof(*ifr);
#else
		l = sizeof(ifr->ifr_name)+sizeof(struct sockaddr);
#endif

		ptr+=l;
	}
	free(buf);
}

void usage()
{
	error(2,"Usage: ifget ip address | if interface | hw interface\n");
}

int main(int argc,char *argv[]) 
{
	int sockfd;
	enum mode mode;

	if(argc != 3)
		usage();

	syslog(LOG_INFO, "Eddie: %s %s %s", argv[0], argv[1], argv[2]);

	check_rights(argv[0]);

	if(strcmp(argv[1],"ip") == 0)
		mode=IP;
	else if(strcmp(argv[1],"if") == 0)
		mode=IF;
	else if(strcmp(argv[1],"hw") == 0)
		mode=HW;
	else
		usage();

	if((sockfd=socket(AF_INET,SOCK_DGRAM,0)) == -1)
		error(2,"ifget: Bad socket");

	ifget(sockfd, mode, argv[2]);

	return(0);
}

