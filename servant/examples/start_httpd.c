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
*/

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

#define BUF_SIZE 8192

int kill(pid_t pid, int sig);
void sub(char *buf,char *src,char *dest);

int main(int argc,char *argv[]) {
  FILE *pidFile,*genericConfigFile,*configFile;
  char pidFilePath[256],genericConfigFilePath[256],configFilePath[256];
  char buf[BUF_SIZE];
  pid_t pid;

  if(argc != 5) {
    fprintf(stderr,"Usage: start_httpd ip-address port httpd-path conf-dir\n");
    exit(2);
  }
  
  sprintf(pidFilePath,"/var/tmp/httpd-%s:%s.pid",argv[1],argv[2]);
  
  if((pidFile=fopen(pidFilePath,"r")) != NULL) {
    fscanf(pidFile,"%u",&pid);
    kill(pid,SIGTERM);
    fclose(pidFile);
  } 
  
  sprintf(genericConfigFilePath,"%s/httpd.conf",argv[4]);
  
  if((genericConfigFile=fopen(genericConfigFilePath,"r")) == NULL) {
    perror(genericConfigFilePath);
    exit(2);
  }

  sprintf(configFilePath,"/var/tmp/httpd-%s:%s.conf",argv[1],argv[2]);
  
  if((configFile=fopen(configFilePath,"w")) == NULL) {
    perror(configFilePath);
    fclose(genericConfigFile);
    exit(2);
  }

  while(fgets(buf,BUF_SIZE,genericConfigFile) != NULL) {
    sub(buf,"@IP",argv[1]);
    sub(buf,"@PORT",argv[2]);
    fputs(buf,configFile);
  }

  fclose(genericConfigFile);
  fclose(configFile);

  if(execl(argv[3],argv[3],"-f",configFilePath,NULL) == -1)
    perror(argv[3]);
  
  return(0);
}

void sub(char *buf,char *src,char *dest) {
  char *s;

  if((s=strstr(buf,src)) != NULL) {
    if(strlen(src) != strlen(dest))
      memmove(s+strlen(dest),s+strlen(src),strlen(s+strlen(src))+1);
    
    memcpy(s,dest,strlen(dest));
  }
}
