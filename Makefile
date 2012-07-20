CABAL=`which cabal-dev`
METRICS=../network-metrics

#
# Targets
#

.PHONY: build install deps conf clean prof

all: build

build: deps
	$(CABAL) build

install: deps
	$(CABAL) install

deps:
	$(MAKE) -C $(METRICS)
	$(CABAL) add-source $(METRICS)

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean

prof: clean
	$(CABAL) configure --enable-executable-profiling
	$(MAKE) build
	./gamekeeper --service=prof


