# Configuration

JOREL_CONFIG ?= jorel.config
JOREL ?= $(DEPS_DIR)/jorel/jorel
export JOREL

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

jorel.release: 
	$(verbose) make jorel.exec cmd=release

$(JOREL_CONFIG):
	$(verbose) make jorel.exec cmd=gen_config

ifeq ($(cmd),gen_config)
jorel.exec: jorel.run
else
jorel.exec: $(JOREL_CONFIG) jorel.run
endif

jorel.run: app $(JOREL)
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

$(JOREL): deps

