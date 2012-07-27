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
#include <sys/types.h>
#include <signal.h>

int kill(pid_t pid, int sig);

int main(int argc,char *argv[]) {
  FILE *pidFile;
  char pidFilePath[256];
  pid_t pid;

  if(argc != 3) {
    fprintf(stderr,"Usage: stop_httpd ip-address port\n");
    exit(2);
  }
  
  sprintf(pidFilePath,"/var/tmp/httpd-%s:%s.pid",argv[1],argv[2]);
  
  if((pidFile=fopen(pidFilePath,"r")) != NULL) {
    fscanf(pidFile,"%u",&pid);
    kill(pid,SIGTERM);
    fclose(pidFile);
  } else
    fprintf(stderr,"stop_httpd: Could not stop old httpd");

  return(0);
}
