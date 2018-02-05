KERL_VERSION = 20.2

ifdef KERL_VERSION
  SHELL := /bin/bash
  KERL_PATH = $(shell kerl list installations | grep ${KERL_VERSION} | perl -pe 's/^[^ ]* //')
  PATH := $(KERL_PATH)/bin:$(PATH)
endif

default: compile

compile:
	rebar3 compile

doc:
	rebar3 edoc

realclean:
	rebar3 clean
	rm -Rf _build
	rm -f test/*.beam
	rm -f rebar.lock
	rm -f *.tgz
	rm -Rf log/*

ct:
	mkdir -p log
	rebar3 as test clean
	rebar3 ct

ct-cover:
	mkdir -p log
	rebar3 as test clean
	rebar3 ct
	rebar3 cover
	rebar3 covertool generate

dialyzer:
	rebar3 dialyzer

.PHONY: test
test: ct dialyzer
