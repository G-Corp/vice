# Copyright (c) 2013-2015, Loïc Hoguin <essen@ninenines.eu>
# Copyright (c) 2016, Grégoire Lejeune
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

.PHONY: doc
REBAR_ENV ?= default

all: compile-erl

# Verbosity.

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

# Utils

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))

# Common

CP = cp
CP_R = cp -r
RM = rm
RM_RF = rm -rf
RM_F = rm -f
MKDIR_P = mkdir -p

# Config

NODE_HOST ?= 127.0.0.1
NODE_NAME ?= ${current_dir}-$(shell bash -c 'echo $$RANDOM')
ifneq ("$(wildcard config/$(current_dir).config)","")
  ERL_CONFIG="config/$(current_dir).config"
endif
ifneq ("$(wildcard config/$(current_dir)-$(REBAR_ENV).config)","")
  ERL_CONFIG="config/$(current_dir)-$(REBAR_ENV).config"
endif

# Core functions.

empty :=
space := $(empty) $(empty)
tab := $(empty) $(empty)
comma := ,

define newline


endef

# Template

# define internet
#   echo -e "GET http://google.com HTTP/1.0\n\n" | nc google.com 80 > /dev/null 2>&1
#   echo $?
# endef
# 
# INTERNET=$(shell $(call internet))

define render_template
  $(verbose) printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
endef

# Erlang

ERL = erl +A0 -noinput -boot start_clean

define erlang
$(ERL) -noshell -s init stop -eval "$(subst $(newline),,$(subst ",\",$(1)))"
endef

define get_version.erl
  {ok, [{application, _, X}]} = file:consult("$(1)"),
  {vsn, VSN} = lists:keyfind(vsn, 1, X),
  io:format("~s", [VSN]).
endef

define get_app_name.erl
  {ok, [{application, AppName, _}]} = file:consult("$(1)"),
  io:format("~s", [AppName]).
endef

ifndef WS
ifdef SP
WS = $(subst a,,a $(wordlist 1,$(SP),a a a a a a a a a a a a a a a a a a a a))
else
WS = $(tab)
endif
endif

# rebar3

FIND_REBAR = \
                REBAR_BIN=; \
                for x in ./rebar3 rebar3; do \
                if type "$${x%% *}" >/dev/null 2>/dev/null; then REBAR_BIN=$$x; break; fi; \
                done; \
                if [ -z "$$REBAR_BIN" ]; then echo 1>&2 "Unable to find rebar3"; exit 2; fi
REBAR = $(FIND_REBAR); $$REBAR_BIN

# Project

APP_SRC=$(shell find src -name "*.app.src")
ifeq ($(APP_SRC),)
else
  APP_VERSION=$(shell $(call erlang,$(call get_version.erl,${APP_SRC})))
  APP_NAME=$(shell $(call erlang,$(call get_app_name.erl,${APP_SRC})))
endif

# mix

MIX = mix

# Default tasks
ifeq ($(HAS_ELIXIR), 1)

compile-ex: elixir clean
	$(verbose) $(MIX) deps.get
	$(verbose) $(MIX) deps.compile
	$(verbose) $(MIX) compile

elixir:: ## Generate Elixir bindings (mix.exs and libs)
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

distclean-ex: clean-ex
	$(verbose) $(RM_F) mix.lock

clean-ex:
	$(verbose) $(RM_RF) _build deps

dist-ex: clean compile-ex

COMPILE=compile-erl compile-ex
CLEAN=clean-erl clean-ex
DISTCLEAN=distclean-erl distclean-ex
DIST=dist-erl dist-ex
else
COMPILE=compile-erl
CLEAN=clean-erl
DISTCLEAN=distclean-erl
DIST=dist-erl
endif

ifdef NO_LINT
LINT=
else
lint:
	$(verbose) $(REBAR) lint

LINT=lint
endif

ifdef NO_XREF
XREF=
else
xref:
	$(verbose) $(REBAR) xref

XREF=xref
endif

update-packages-registry: ## Updage packages registry
	$(verbose) $(REBAR) as $(REBAR_ENV) update

ifdef NO_REGISTRY_UPDATE
compile-erl:
else
compile-erl: update-packages-registry
endif
	$(verbose) $(REBAR) as $(REBAR_ENV) compile

tests: ## Run tests
	$(verbose) $(REBAR) eunit

doc:: ## Generate doc
ifndef NO_DOC
	$(verbose) $(REBAR) edoc
endif

dist: $(DIST) ## Create a distribution

clean:: $(CLEAN) ## Clean

distclean:: $(DISTCLEAN) ## Clean the distribution

dev: compile-erl
ifdef ERL_CONFIG
	$(verbose) erl -pa _build/$(REBAR_ENV)/lib/*/ebin _build/$(REBAR_ENV)/lib/*/include -config ${ERL_CONFIG} -name ${NODE_NAME}@${NODE_HOST} -setcookie ${current_dir}
else
	$(verbose) erl -pa _build/$(REBAR_ENV)/lib/*/ebin _build/$(REBAR_ENV)/lib/*/include -name ${NODE_NAME}@${NODE_HOST} -setcookie ${current_dir}
endif

dist-erl: clean compile-erl tests $(LINT) $(XREF) doc

clean-erl:
	$(verbose) $(RM_RF) _build test/eunit

distclean-erl: clean-erl
	$(verbose) $(RM_F) rebar.lock

info: ## Display application informations
	$(verbose) echo "App source file: $(APP_SRC)"
	$(verbose) echo "App name:        $(APP_NAME)"
	$(verbose) echo "App version:     $(APP_VERSION)"

tag: DO_TAG ?= $(shell read -p "tag version $(APP_VERSION) (y/n) [n]: " pwd; echo $$pwd)

tag: ## Create a git tag
	$(verbose) echo $(if $(findstring $(DO_TAG),y,Y,yes,Yes,YES),$(shell git tag $(APP_VERSION)),)

# Elixir

local.hex: ## Install hexfor Mix
	$(MIX) local.hex --force

local.rebar: ## Install rebar for Mix
	$(MIX) local.rebar --force

# Update

BU_MK_REPO ?= https://github.com/botsunit/bu.mk
BU_MK_COMMIT ?=
BU_MK_BUILD_DIR ?= .bu.mk.build

bu-mk: ## Update bu.mk
	git clone $(BU_MK_REPO) $(BU_MK_BUILD_DIR)
ifdef BU_MK_COMMIT
	cd $(BU_MK_BUILD_DIR) && git checkout $(BU_MK_COMMIT)
endif
	$(CP) $(BU_MK_BUILD_DIR)/bu.mk ./bu.mk
	$(RM_RF) $(BU_MK_BUILD_DIR)

# Help

help: ## Show this help.
	$(verbose) echo "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\033[33m\1\\033[m:\2/' | column -c2 -t -s :)"

