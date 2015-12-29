# Configuration

JOREL_CONFIG ?= $(CURDIR)/jorel.config
JOREL ?= $(CURDIR)/jorel
export JOREL

JOREL_URL ?= https://github.com/emedia-project/jorel/wiki/jorel

help::
	$(verbose) printf "%s\n" "" \
		"Jorel targets:" \
		"  jorel.release    Create a release with Jorel" 

distclean:: distclean-jorel

$(JOREL):
	$(gen_verbose) $(call core_http_get,$(JOREL),$(JOREL_URL))
	$(verbose) chmod +x $(JOREL)

jorel.release: $(JOREL) $(JOREL_CONFIG)
	@$(JOREL) release

distclean-jorel:
	$(gen_verbose) rm -rf $(JOREL)
