CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: build install conf clean prof

all: build

build:
	$(CABAL) build

install:
	$(CABAL) install

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean

prof: clean
	$(CABAL) configure --enable-executable-profiling
	$(MAKE) build
	./gamekeeper --service=prof


