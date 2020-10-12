/*
  From https://github.com/thekvs/esnappy:
  Copyright (c) 2011 Konstantin V. Sorokin
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
  3. Neither the name of the copyright holder nor the names of contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTOR(S) ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTOR(S) BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/
// vim: shiftwidth=4 expandtab
#ifndef __ASYNC_QUEUE_H_INCLUDED__
#define __ASYNC_QUEUE_H_INCLUDED__

#include <sys/queue.h>
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <err.h>

#include <erl_nif.h>

#ifdef __cplusplus
extern "C" {
#endif

TAILQ_HEAD(queue, async_queue_entry);

struct async_queue_entry {
    TAILQ_ENTRY(async_queue_entry) entries;
    void *data;
};

typedef struct __async_queue {
    struct queue *q;
    ErlNifMutex  *mutex;
    ErlNifCond   *cond;
    int           waiting_threads;
    int           len;
} async_queue_t;

async_queue_t* async_queue_create(char* mutex_name, char* condvar_name);
int async_queue_length(async_queue_t *aq);
void* async_queue_pop(async_queue_t *aq);
void async_queue_push(async_queue_t *aq, void *data);
void async_queue_destroy(async_queue_t *aq);

#define ALLOC(size)             enif_alloc(size)
#define MUTEX_LOCK(mutex)       enif_mutex_lock(mutex)
#define MUTEX_UNLOCK(mutex)     enif_mutex_unlock(mutex)
#define MUTEX_DESTROY(mutex)    enif_mutex_destroy(mutex)
#define COND_SIGNAL(condvar)    enif_cond_signal(condvar)
#define COND_DESTROY(condvar)   enif_cond_destroy(condvar)

#ifdef __cplusplus
} // extern "C"
#endif

#endif
