HAS_ELIXIR=1

include bu.mk

clean::
	$(verbose) rm -rf .c_build priv/evic_facedetect.so

distclean::
	$(verbose) rm -rf .c_build priv/evic_facedetect.so

