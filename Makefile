.PHONY: all compile test clean dialyze

all: compile

dialyze:
	rebar dialyze


compile:
	rebar compile

clean:
	rebar clean

test:
	rebar eunit
