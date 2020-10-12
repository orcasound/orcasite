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
#include "async_queue.h"

async_queue_t*
async_queue_create(char* mutex_name, char* condvar_name)
{
    async_queue_t *aq;

    aq = ALLOC(sizeof(*aq));

    if (!aq) {
        errx(1, "enif_alloc() failed");
    }

    aq->q = ALLOC(sizeof(*(aq->q)));

    if (!(aq->q)) {
        errx(1, "enif_alloc() failed");
    }

    TAILQ_INIT(aq->q);

    aq->waiting_threads = aq->len = 0;

    aq->mutex = enif_mutex_create(mutex_name);

    if (!aq->mutex) {
        errx(1, "enif_mutex_create() failed");
    }

    aq->cond = enif_cond_create(condvar_name);

    if (!aq->cond) {
        errx(1, "enif_cond_create() failed");
    }

    return aq;
}

int
async_queue_length(async_queue_t *aq)
{
    int length;

    MUTEX_LOCK(aq->mutex);
    length = aq->len - aq->waiting_threads;
    MUTEX_UNLOCK(aq->mutex);

    return length;
}

void *
async_queue_pop(async_queue_t *aq)
{
    struct async_queue_entry *en;
    void *d;

    MUTEX_LOCK(aq->mutex);

    d = NULL;
    aq->waiting_threads++;
    while (TAILQ_EMPTY(aq->q)) {
        enif_cond_wait(aq->cond, aq->mutex);
    }
    aq->waiting_threads--;

    en = TAILQ_FIRST(aq->q);
    TAILQ_REMOVE(aq->q, en, entries);
    d = en->data;
    aq->len--;
    enif_free(en);

    MUTEX_UNLOCK(aq->mutex);

    return d;
}

void
async_queue_push(async_queue_t *aq, void *data)
{
    struct async_queue_entry *en;

    MUTEX_LOCK(aq->mutex);

    en = ALLOC(sizeof(*en));
    en->data = data;
    TAILQ_INSERT_TAIL(aq->q, en, entries);
    aq->len++;

    COND_SIGNAL(aq->cond);
    MUTEX_UNLOCK(aq->mutex);
}

void
async_queue_destroy(async_queue_t *aq)
{
    struct async_queue_entry *en;

    while (!TAILQ_EMPTY(aq->q)) {
        en = TAILQ_FIRST(aq->q);
        TAILQ_REMOVE(aq->q, en, entries);
        enif_free(en);
    }

    COND_DESTROY(aq->cond);
    MUTEX_DESTROY(aq->mutex);

    enif_free(aq->q);
    enif_free(aq);
}

