ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := ssim

all:
	./rebar check-deps
	./rebar get-deps
	./rebar compile
	./rebar generate -f
	chmod a+x rel/ssim/bin/ssim