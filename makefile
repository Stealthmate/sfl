STACK=stack
STACK_TEST=$(STACK)
GHC_OPTIONS=--ghc-options="-Werror -Wunused-imports -Wunused-binds -Wincomplete-patterns" --ghc-options="-j"
GHC_OPTIONS_OPTIMIZED=--ghc-options="-O1"
HLINT=hlint

clean:
	$(STACK) clean

## Default
build: build-deps
	$(STACK) build $(GHC_OPTIONS)

build-snapshot:
	$(STACK) build --only-snapshot

build-deps: build-snapshot
	$(STACK) build --only-dependencies

build-fast: build-deps
	$(STACK) build --fast $(GHC_OPTIONS)

build-optimized: build-deps
	$(STACK) build $(GHC_OPTIONS) $(GHC_OPTIONS_OPTIMIZED)

build-docs:
	$(STACK) build --haddock $(GHC_OPTIONS)

## Tests
build-snapshot-test:
	$(STACK) build --only-snapshot --test --no-run-tests

build-deps-test: build-snapshot-test
	$(STACK_TEST) build --only-dependencies --test --no-run-tests

build-test: build-deps-test
	$(STACK_TEST) build --test --no-run-tests --fast $(GHC_OPTIONS)

run-hlint:
	$(HLINT) src --report
	$(HLINT) ./test --report

run-doctest:
	$(STACK_TEST) exec doctest

run-tests: build-test
	$(STACK_TEST) test --test-arguments=--fail-fast

run-tests-all: run-hlint run-doctest run-tests

## Dev
# Note: GHCi supports only a single option for ghc
# GHCI_OPTIONS=--ghc-options="-j" --ghc-options="-Wunused-imports" --ghc-options="-Wincomplete-patterns" --ghc-options="-Wunused-binds"
RUN_GHCI=$(STACK) ghci $(GHCI_OPTIONS)
RUN_GHCI_TEST=$(STACK_TETS) ghci
run-ghci: docker
	$(RUN_GHCI)

run-ghci-test:
	$(RUN_GHCI_TEST) sfl:lib sfl:test:sfl-test