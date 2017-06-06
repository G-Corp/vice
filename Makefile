HAS_ELIXIR=1

include bu.mk

clean::
	$(verbose) rm -rf .c_build priv/vice_facedetect.so

distclean::
	$(verbose) rm -rf doc
	$(verbose) rm -rf .c_build priv/vice_facedetect.so

changelog: ## Generate CHANGELOG
	$(verbose) github_changelog_generator

release: dist lint tag ## Tag and release to hex.pm
	$(verbose) $(REBAR) hex publish

