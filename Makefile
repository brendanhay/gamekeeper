.PHONY: build all clean conf prof

build:
	cabal-dev build

all: conf install build

install:
	cabal-dev install

clean:
	cabal-dev clean

conf:
	cabal-dev configure

prof: clean
	cabal-dev configure --enable-executable-profiling
	$(MAKE) build
	cat ../script/prefix.log | ./gamekeeper --service=prof


