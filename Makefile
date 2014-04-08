MIX = mix
CFLAGS = -O2 -pedantic -Wall -Wextra -Wno-unused-parameter

.PHONY: all elixir-code clean

all: elixir-code

elixir-code:
	$(MIX) compile

priv/test_port: src/test_port.c
	mkdir -p priv
	$(CC) $(CFLAGS) -o $@ $<

clean:
	$(MIX) clean
	rm -f priv/test_port
