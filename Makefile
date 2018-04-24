REBAR3 = ./rebar3

all:deps compile

deps:
	@$(REBAR3) deps

compile:deps clean
	@$(REBAR3) compile

shell:compile
	@$(REBAR3) shell

eunit_test:
	@$(REBAR3) eunit

ct_test:
	@$(REBAR3) ct

clean:
	@$(REBAR3) clean

release:
	@$(REBAR3) release

prod:
	@$(REBAR3) as prod tar -o .

dev:
	@$(REBAR3) as dev tar -o .
