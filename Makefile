CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: install build test conf clean prof

all: build

build:
	$(CABAL) build

install:
	$(CABAL) install

clean:
	$(CABAL) clean

conf: clean
	$(CABAL) configure
	$(MAKE) build

bench:
	$(CABAL) configure --enable-benchmarks
	$(MAKE) build

test:
	$(CABAL) configure --enable-tests
	$(MAKE) build

prof: clean
	$(CABAL) configure --enable-executable-profiling
	$(MAKE) build

ghci:
	$(CABAL) ghci
