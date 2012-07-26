CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: install build conf clean prof

all: install

install:
	$(CABAL) install

build:
	$(CABAL) build

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean

prof: clean
	$(CABAL) configure --enable-executable-profiling
	$(MAKE) install
	./gamekeeper --service=prof


