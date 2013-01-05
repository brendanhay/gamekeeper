SHELL := /bin/bash
CABAL := `which cabal-dev`

#
# Build
#

.PHONY: install build install clean dist test conf prof

all: build

build:
	$(CABAL) build

install:
	$(CABAL) install

clean:
	$(CABAL) clean

#
# Configure
#

conf:
	$(CABAL) configure
	$(MAKE) build

bench:
	$(CABAL) configure --enable-benchmarks
	$(MAKE) build

test:
	$(CABAL) configure --enable-tests
	$(MAKE) build

prof:
	$(CABAL) configure --enable-executable-profiling
	$(MAKE) build

#
# Interactive
#

ghci:
	$(CABAL) ghci
