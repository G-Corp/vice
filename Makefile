HAS_ELIXIR=1
# NO_XREF=1

include bu.mk

clean::
	$(verbose) $(RM_RF) .c_build priv/vice_facedetect.so

distclean::
	$(verbose) $(RM_RF) doc
	$(verbose) $(RM_RF) lib
	$(verbose) $(RM_RF) .c_build priv/vice_facedetect.so

changelog: ## Generate CHANGELOG
	$(verbose) github_changelog_generator

release: dist lint tag ## Tag and release to hex.pm
	$(verbose) $(REBAR) hex publish

