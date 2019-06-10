BASE_DIR = $(shell pwd)

ERLANG_IMAGE = erlang:22.0.2

DOCKERIZE = docker run --rm \
							 				 --volume "$(BASE_DIR)":/app \
											 --workdir "/app" \
							 				 -it

################################################################################
# Makefile API
################################################################################

.PHONY: deps
deps:
	$(DOCKERIZE) $(ERLANG_IMAGE) rebar3 as dev compile

.PHONY: tests
tests:
	$(DOCKERIZE) $(ERLANG_IMAGE) rebar3 ct

.PHONY: run
run:
	$(DOCKERIZE) --publish 9000:9000 $(ERLANG_IMAGE) rebar3 as dev shell
