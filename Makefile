tags:
	cabal build --ghc-options="-package-db=/home/coot/.cabal/store/ghc-8.6.5/package.db -plugin-package=ghc-tags-plugin -fplugin=Plugin.GhcTags -fplugin-opt=Plugin.GhcTags:/home/coot/repos/zephyr/tags"

.PHONY: tags
