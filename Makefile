.PHONY: all compile test clean

all: compile

compile:
	rebar compile

clean:
	rebar clean

test:
	rebar eunit
