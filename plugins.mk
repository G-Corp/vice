# Configuration

JOREL_CONFIG ?= $(CURDIR)/jorel.config
JOREL ?= $(DEPS_DIR)/jorel/jorel
export JOREL

JOREL_URL ?= https://github.com/emedia-project/jorel/wiki/jorel

help::
	$(verbose) printf "%s\n" "" \
		"Jorel targets:" \
		"  jorel.release        Create a release with Jorel" \
		"  jorel.exec cmd=CMD   Execute the Jorel command specified" \
	  "" \
		"Jorel rules accepts the following options :" \
		" * o=OUTPUT_DIR" \
		" * n=REL_NAME" \
		" * v=REL_VERSION" \
		" * c=CONFIG_FILE"

jorel.release: $(JOREL) $(JOREL_CONFIG)
	$(verbose) make jorel.exec cmd=release

jorel.exec: $(JOREL) $(JOREL_CONFIG)
ifndef cmd
	$(error Usage: $(MAKE) jorel.exec cmd=CMD)
endif
	$(eval x := )
ifdef o
	$(eval x := --output-dir $o $x)
endif
ifdef n
	$(eval x := --relname $n $x)
endif
ifdef v
	$(eval x := --relvsn $v $x)
endif
ifdef c
	$(eval x := --config $c $x)
endif
	$(verbose) $(JOREL) $(cmd) $x

