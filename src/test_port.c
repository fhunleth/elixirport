#include <err.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "erlcmd.h"

//#define DEBUG
#ifdef DEBUG
#define debug(...) fprintf(stderr, __VA_ARGS__)
#else
#define debug(...)
#endif

static void test_port_request_handler(const char *req, void *cookie)
{
    // Commands are of the form {Command, Arguments}:
    // { atom(), [term()] }
    int req_index = sizeof(uint16_t);
    if (ei_decode_version(req, &req_index, NULL) < 0)
        errx(EXIT_FAILURE, "Message version issue?");

    int arity;
    if (ei_decode_tuple_header(req, &req_index, &arity) < 0 ||
            arity != 2)
        errx(EXIT_FAILURE, "expecting 2 tuple");

    char cmd[MAXATOMLEN];
    if (ei_decode_atom(req, &req_index, cmd) < 0)
        errx(EXIT_FAILURE, "expecting command atom");
    debug("test_port_request_handler: %s\n", cmd);

    char resp[ERLCMD_BUF_SIZE];
    int resp_index = sizeof(uint16_t); // Space for payload size
    ei_encode_version(resp, &resp_index);
    if (strcmp(cmd, "ping") == 0) {
	debug("Pong!!!\n");
        ei_encode_atom(resp, &resp_index, "pong");
    } else if (strcmp(cmd, "add") == 0) {
        long x;
        long y;

        if (ei_decode_tuple_header(req, &req_index, &arity) < 0 ||
            arity != 2)
           errx(EXIT_FAILURE, "add requires 2 numbers");

        if (ei_decode_long(req, &req_index, &x) < 0 ||
            ei_decode_long(req, &req_index, &y) < 0)
           errx(EXIT_FAILURE, "expecting long?");

	debug("Adding %ld + %ld\n", x, y);
        ei_encode_long(resp, &resp_index, x + y);
    } else
        errx(EXIT_FAILURE, "unknown command: %s", cmd);

    erlcmd_send(resp, resp_index);
}

int main(int argc, char *argv[])
{
    struct erlcmd handler;
    erlcmd_init(&handler, test_port_request_handler, NULL);

    for (;;) {
	// In most port programs that I write, I have multiple file
	// descriptors that require polling, so use poll even though
	// it's not needed.
	struct pollfd fdset[1];

	fdset[0].fd = STDIN_FILENO;
	fdset[0].events = POLLIN;
	fdset[0].revents = 0;

	int rc = poll(fdset, 1, -1);
	if (rc < 0) {
	    // Retry if EINTR
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "poll");
	}

	if (fdset[0].revents & (POLLIN | POLLHUP))
	    erlcmd_process(&handler);

    }

    return 0;
}
