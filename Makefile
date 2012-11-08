CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: install build conf clean prof

all: build

build:
	$(CABAL) build

install:
	$(CABAL) install

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean

prof:
	$(CABAL) configure --enable-executable-profiling
	$(MAKE) build

ghci:
	$(CABAL) ghci
