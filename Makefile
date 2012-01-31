all: release

release: compile
		@rm -rf rel/htoad
		@rebar generate

compile: deps
		@rebar compile

deps:
		@rebar get-deps