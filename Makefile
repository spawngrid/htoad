HERE=$(shell pwd)
KERL_CONFIG_OPTS=""

ifeq ($(shell which erl),)
	ERL=erlang/bin/erl
	REBAR=". $(HERE)/erlang/activate && $(HERE)/rebar"
else
	ERL=$(shell which erl)
	REBAR="./rebar"
endif

all: release

release: $(ERL) compile
		@rm -rf rel/htoad
		@$(REBAR) generate

compile: $(ERL) deps
		@$(REBAR) compile

deps: $(ERL)
		@$(REBAR) get-deps

$(ERL):
		@KERL_CONFIGURE_OPTIONS=$(KERL_CONFIG_OPTS) KERL_INSTALL_AGNERIZED_REBAR=n HOME=$(HERE) ./kerl build R15B r15b
		@KERL_CONFIGURE_OPTIONS=$(KERL_CONFIG_OPTS) KERL_INSTALL_AGNERIZED_REBAR=n HOME=$(HERE) ./kerl install r15b erlang

