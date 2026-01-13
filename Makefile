.PHONY: build
build: requires_nix_shell
	cabal v2-build all

.PHONY: shell
shell:
	nix develop --no-update-lock-file

.PHONY: nix-build
nix-build:
	nix develop --no-update-lock-file --show-trace -c cabal v2-build all

.PHONY: nix-test
nix-test:
	nix develop --no-update-lock-file --show-trace -c cabal run wingriders-plutarch-test

.PHONY: test
# You can call it with:
# make test
# make test rx=Subword
# make test rx="'Multi-word substring'"
# make test rx="'/The first substring/ && /And the second one/'"
rx = ""
test: requires_nix_shell
	cabal run wingriders-plutarch-test -- -p $(rx)

.PHONY: export
export: requires_nix_shell
	cabal run export

# Source dirs to run the formatter on
PWD := $(shell pwd)
FORMAT_SOURCES := $(shell git ls-files | grep -E '.*\.hs'        \
				          | awk -v P="$(PWD)" '{ print P "/" $$0 }')

.PHONY: format
format: requires_nix_shell
	fourmolu -i ${FORMAT_SOURCES}

.PHONY: format_check
format_check:
	fourmolu --mode check ${FORMAT_SOURCES}

.PHONY: requires_nix_shell
# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "$(IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ "$(IN_NIX_SHELL)" ] || (echo "    run 'make shell' first" && false)

.PHONY: clean
clean:
	cabal v2-clean
