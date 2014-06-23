#include <err.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <ei.h>

//#define DEBUG
#ifdef DEBUG
#define debug(...) fprintf(stderr, __VA_ARGS__)
#else
#define debug(...)
#endif

void test_port_request_handler(const char *buffer);

/*
 * Erlang request/response processing
 */
#define ERLCMD_BUF_SIZE 1024
struct erlcmd
{
    char buffer[ERLCMD_BUF_SIZE];
    size_t index;
};

/**
 * @brief Initialize command processing from Erlang.
 */
void erlcmd_init(struct erlcmd *handler)
{
    memset(handler, 0, sizeof(*handler));
}

/**
 * @brief Send a message back to Erlang
 */
void erlcmd_send(char *response, size_t len)
{
    uint16_t be_len = htons(len - sizeof(uint16_t));
    memcpy(response, &be_len, sizeof(be_len));

    size_t wrote = 0;
    do {
	ssize_t amount_written = write(STDOUT_FILENO, response + wrote, len - wrote);
	if (amount_written < 0) {
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "write");
	}

	wrote += amount_written;
    } while (wrote < len);
}

/**
 * @brief Dispatch commands in the buffer
 * @return the number of bytes processed
 */
static size_t erlcmd_try_dispatch(struct erlcmd *handler)
{
    // Check for length field
    if (handler->index < sizeof(uint16_t))
	return 0;

    uint16_t be_len;
    memcpy(&be_len, handler->buffer, sizeof(uint16_t));
    ssize_t msglen = ntohs(be_len);
    if (msglen + sizeof(uint16_t) > sizeof(handler->buffer))
	errx(EXIT_FAILURE, "Message too long");
    debug("Got %d bytes. Need %d\n", (int) handler->index, (int) (msglen + sizeof(uint16_t)));

    // Check whether we've received the entire message
    if (msglen + sizeof(uint16_t) > handler->index)
	return 0;

    debug("decode message\n");
    test_port_request_handler(handler->buffer);

    return msglen + sizeof(uint16_t);
}

/**
 * @brief call to process any new requests from Erlang
 */
void erlcmd_process(struct erlcmd *handler)
{
    debug("Going to read %d bytes\n", (int) (sizeof(handler->buffer) - handler->index));
    ssize_t amount_read = read(STDIN_FILENO, handler->buffer, sizeof(handler->buffer) - handler->index);
    if (amount_read < 0) {
	// EINTR is ok to get, since we were interrupted by a signal.
	if (errno == EINTR)
	    return;

	// Everything else is unexpected.
	err(EXIT_FAILURE, "read");
    } else if (amount_read == 0) {
	// EOF. Erlang process was terminated.
	// This happens after a release or if there was an error. */
	debug("Exiting port!\n");
	exit(EXIT_SUCCESS);
    }

    handler->index += amount_read;
    debug("Processing %d bytes...\n", (int) handler->index);
    for (;;) {
	size_t bytes_processed = erlcmd_try_dispatch(handler);

	if (bytes_processed == 0) {
	    /* Only have part of the command to process. */
	    break;
	} else if (handler->index > bytes_processed) {
	    /* Processed the command and there's more data. */
	    memmove(handler->buffer, &handler->buffer[bytes_processed], handler->index - bytes_processed);
	    handler->index -= bytes_processed;
	} else {
	    /* Processed the whole buffer. */
	    handler->index = 0;
	    break;
	}
    }
}

void test_port_request_handler(const char *req)
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
    erlcmd_init(&handler);

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
