ERLC := erlc

LDLIBS := -lcurses
CFLAGS := -Wall -ansi -ggdb

all: cui aggro.beam a_shell.beam

%.beam: %.erl
	$(ERLC) $<
