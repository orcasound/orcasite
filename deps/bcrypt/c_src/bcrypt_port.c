/*
 * Copyright (c) 2011 Hunter Morris <hunter.morris@smarkets.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "erl_blf.h"
#include "ei.h"

#define dec_int16(s) ((((unsigned char*)(s))[0] << 8) | \
                      (((unsigned char*)(s))[1]))

#define BUFSIZE (1 << 16)
#define CMD_SALT 0
#define CMD_HASHPW 1

#define DATASIZE 1024

extern int ts_bcrypt(char *, const char *, const char *);
extern void encode_salt(char *, u_int8_t *, u_int16_t, u_int8_t);


static void
fail(int place) {
    fprintf(stderr, "Something went wrong %d\n", place);
    exit(1);
}

/* These methods came from the Erlang port command tutorial:
 * http://www.erlang.org/doc/tutorial/c_port.html#4.2
 */
static int
read_buf(int fd, char *buf, int len)
{
    int i, got = 0;
    do {
        if ((i = read(fd, buf+got, len-got)) <= 0) {
            if (i == 0) return got;
            if (errno != EINTR)
                fail(-1);
            i = 0;
        }
        got += i;
    } while (got < len);
    return (len);
}

static int
read_cmd(char *buf)
{
    int len;
    if (read_buf(0, buf, 2) != 2)
        return 0;
    len = dec_int16(buf);
    if (read_buf(0, buf, len) != len)
        return 0;
    return 1;
}

static void
write_buf(int fd, const char *buf, int len)
{
    int i;
    int done = 0;

    do {
        if ((i = write(fd, buf+done, len-done)) < 0) {
            if (errno != EINTR)
                fail(-2);
            i = 0;
        }
        done += i;
    } while (done < len);
}

static void
write_cmd(const char *buf, int len)
{
    unsigned char li;

    li = (len >> 8) & 0xff;
    write_buf(1, (char *) &li, 1);
    li = len & 0xff;
    write_buf(1, (char *) &li, 1);
    write_buf(1, buf, len);
}

static void
process_reply(int cmd, const char *result)
{
    ei_x_buff res_buf;

    if (ei_x_new_with_version(&res_buf) != 0)
        fail(-10);
    if (ei_x_encode_tuple_header(&res_buf, 2) != 0)
        fail(-11);
    if (ei_x_encode_long(&res_buf, (long) cmd) != 0)
        fail(-12);
    if (ei_x_encode_binary(&res_buf, result, (long) strlen( (const char *) result)) != 0)
        fail(-13);

    write_cmd(res_buf.buff, res_buf.index);

    if (ei_x_free(&res_buf) != 0)
        fail(-14);
}

static void
process_error_reply(int cmd, const char *error)
{
    ei_x_buff res_buf;

    if (ei_x_new_with_version(&res_buf) != 0)
        fail(-20);
    if (ei_x_encode_tuple_header(&res_buf, 2) != 0)
        fail(-21);
    if (ei_x_encode_long(&res_buf, (long) cmd) != 0)
        fail(-22);
    if (ei_x_encode_atom(&res_buf, error) != 0)
        fail(-23);

    write_cmd(res_buf.buff, res_buf.index);

    if (ei_x_free(&res_buf) != 0)
        fail(-24);
}

static void
process_encode_salt(
    int salt_size,
    char *salt,
    long rounds)
{
    char encoded_salt[64];

    if (16 != salt_size) {
        process_error_reply(CMD_SALT, "invalid_salt_length");
    } else if (rounds < 4 || rounds > 31) {
        process_error_reply(CMD_SALT, "invalid_rounds");
    } else {
        memset(encoded_salt, 0, 64);
        encode_salt(encoded_salt, (u_int8_t *) salt, (u_int16_t) salt_size, (u_int8_t) rounds);
        process_reply(CMD_SALT, encoded_salt);
    }
}

static void
process_hashpw(
    int password_size,
    char *password,
    int salt_size,
    char *salt)
{
    char encrypted[DATASIZE+1];

    memset(encrypted, 0, DATASIZE+1);
    if (ts_bcrypt(encrypted, password, salt)) {
        process_error_reply(CMD_HASHPW, "invalid_salt");
    } else {
        process_reply(CMD_HASHPW, encrypted);
    }
}

static void
process_command(char *buf)
{
    int index = 0;
    int version = 0;
    int arity = 0;
    int type;
    long cmd;
    long len;
    long rounds;
    int data_size;
    char data[DATASIZE+1];
    int salt_size;
    char salt[DATASIZE+1];

    memset(data, 0, DATASIZE+1);
    memset(salt, 0, DATASIZE+1);

    if (ei_decode_version(buf, &index, &version) != 0)
        fail(1);

    // Three tuple: {Cmd, Port, Data}
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
        fail(2);
    if (arity != 2)
        fail(3);
    if (ei_decode_long(buf, &index, &cmd) != 0)
        fail(4);

    // All commands have a two tuple for Data
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
        fail(6);
    if (arity != 2)
        fail(7);

    // First arg is always a binary
    if (ei_get_type(buf, &index, &type, &data_size) != 0)
        fail(8);
    if (type != ERL_BINARY_EXT)
        fail(9);
    if (data_size < 0 || data_size > DATASIZE)
        fail(10);
    if (ei_decode_binary(buf, &index, data, &len) != 0)
        fail(11);

    switch (cmd) {
        case CMD_HASHPW:
            // Two tuple: {Pass, Salt}
            if (ei_get_type(buf, &index, &type, &salt_size) != 0)
                fail(12);
            if (type != ERL_BINARY_EXT)
                fail(13);
            if (salt_size < 0 || salt_size > DATASIZE)
                fail(14);
            if (ei_decode_binary(buf, &index, salt, &len) != 0)
                fail(15);

            process_hashpw(data_size, data, salt_size, salt);
            break;
        case CMD_SALT:
            // Two tuple: {Csalt, LogRounds}
            if (ei_decode_long(buf, &index, &rounds) != 0)
                fail(16);

            process_encode_salt(data_size, data, rounds);
            break;
        default:
            fail(17);
    }
}

static void
loop(void)
{
    char buf[BUFSIZE];

    while (read_cmd(buf) == 1) {
        process_command(buf);
    }
}

int
main(int argc, char *argv[])
{
    ei_init();
    loop();
    return 0;
}
