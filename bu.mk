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

# Verbosity.

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

# Common

CP = cp
CP_R = cp -r
RM = rm
RM_RF = rm -rf
RM_F = rm -f
MKDIR_P = mkdir -p

# Core functions.

empty :=
space := $(empty) $(empty)
tab := $(empty) $(empty)
comma := ,

define newline


endef

# Template

define render_template
  $(verbose) printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
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

# mix

MIX = mix

