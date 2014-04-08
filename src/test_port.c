#include <err.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <erl_interface.h>
#include <ei.h>

//#define DEBUG
#ifdef DEBUG
#define debug(...) fprintf(stderr, __VA_ARGS__)
#else
#define debug(...)
#endif

void test_port_request_handler(ETERM *emsg);

/*
 * Erlang request/response processing
 */
#define ERLCMD_BUF_SIZE 1024
struct erlcmd
{
    unsigned char buffer[ERLCMD_BUF_SIZE];
    size_t index;
};

/**
 * @brief Initialize command processing from Erlang.
 */
void erlcmd_init(struct erlcmd *handler)
{
    erl_init(NULL, 0);
    memset(handler, 0, sizeof(*handler));
}

/**
 * @brief Send a message back to Erlang
 */
void erlcmd_send(ETERM *response)
{
    unsigned char buf[1024];

    if (erl_encode(response, buf + sizeof(uint16_t)) == 0)
	errx(EXIT_FAILURE, "erl_encode");

    ssize_t len = erl_term_len(response);
    uint16_t be_len = htons(len);
    memcpy(buf, &be_len, sizeof(be_len));

    len += sizeof(uint16_t);
    ssize_t wrote = 0;
    do {
	ssize_t amount_written = write(STDOUT_FILENO, buf + wrote, len - wrote);
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
    debug("Got %d bytes. Need %d\n", handler->index, msglen + sizeof(uint16_t));

    // Check whether we've received the entire message
    if (msglen + sizeof(uint16_t) > handler->index)
	return 0;

    debug("erl_decode\n");
    ETERM *emsg = erl_decode(handler->buffer + sizeof(uint16_t));
    if (emsg == NULL)
	errx(EXIT_FAILURE, "erl_decode");

    debug("test_port_request_handler\n");
    test_port_request_handler(emsg);

    erl_free_term(emsg);

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
    debug("Processing %d bytes...\n", handler->index);
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

void test_port_request_handler(ETERM *emsg)
{
    // Commands are of the form {Command, Arguments}:
    // { atom(), [term()] }

    ETERM *cmd = erl_element(1, emsg);
    if (cmd == NULL)
	errx(EXIT_FAILURE, "bad request");

    ETERM *resp;
    if (strcmp(ERL_ATOM_PTR(cmd), "ping") == 0) {
	resp = erl_format("pong");
    } else {
	resp = erl_format("error");
    }
    erlcmd_send(resp);

    erl_free_term(resp);
    erl_free_term(cmd);
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
