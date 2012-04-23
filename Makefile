HERE=$(shell pwd)
KERL_CONFIG_OPTS=""

ERL=erlang/bin/erl
REBAR=. $(HERE)/erlang/activate && $(HERE)/rebar

all: release

release: $(ERL) compile
		@rm -rf rel/htoad
		@$(REBAR) generate

compile: $(ERL) deps
		@$(REBAR) compile

deps: $(ERL)
		@$(REBAR) get-deps

$(ERL):
		@KERL_CONFIGURE_OPTIONS=$(KERL_CONFIG_OPTS) KERL_INSTALL_AGNERIZED_REBAR=n HOME=$(HERE) ./kerl build R15B01 r15b01
		@KERL_CONFIGURE_OPTIONS=$(KERL_CONFIG_OPTS) KERL_INSTALL_AGNERIZED_REBAR=n HOME=$(HERE) ./kerl install r15b01 erlang

