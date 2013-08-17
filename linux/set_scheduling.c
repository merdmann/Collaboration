/*
 * bind.c
 *
 *  Created on: Jul 11, 2012
 *      Author: merdmann
 */
#include <stdio.h>
#include <sched.h>
#include <stdlib.h>
#include <unistd.h>
#include <sched.h>
#include <errno.h>
#include <sys/resource.h>

int set_scheduling( int policy ) {
	struct sched_param params;

        errno = 0;
        params.sched_priority = (policy == SCHED_FIFO || policy == SCHED_RR ) ? 99 : 0;

	if( sched_setscheduler(getpid(), policy, &params )<0) {
    		perror("*** setschedler failed**");
    		return -1;
        }

  	return 0;
}
