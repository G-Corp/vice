include bu.mk

.PHONY: doc docker-compose.yml

compile-erl:
	$(verbose) $(REBAR) compile

compile-ex: elixir
	$(verbose) $(MIX) deps.get
	$(verbose) $(MIX) compile

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

tests:
	$(verbose) $(REBAR) eunit

doc:
	$(verbose) $(REBAR) as doc edoc

dist: dist-erl dist-ex doc

release: dist-ex dist-erl
	$(verbose) $(REBAR) hex publish

dist-erl: compile-erl tests

distclean-erl:
	$(verbose) rm -f rebar.lock

dist-ex: compile-ex

distclean-ex:
	$(verbose) rm -f mix.lock

distclean: distclean-ex distclean-erl
	$(verbose) rm -rf _build test/eunit deps ebin c_src/build priv/evic_facedetect.so

dev: compile-erl
	$(verbose) erl -pa _build/default/lib/*/ebin _build/default/lib/*/include -config config/evic.config

