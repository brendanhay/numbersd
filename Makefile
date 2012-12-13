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

#
# Release
#

.PHONY: dist assets wipe

# Where numersd serves assets from
RES := assets

copy:
	-@mkdir -p dist/tar/$(RES)
	cp -rf $(RES)/numbersd.* dist/tar/$(RES)/
	cp -f README.md LICENSE dist/build/numbersd/numbersd dist/tar/

dist: copy
	$(eval VER = $(shell dist/tar/numbersd -V | awk '{sub(/-.*/,"",$$3);print $$3}'))
	cd dist/tar && tar -czf ../numbersd-$(VER).tar.gz *
	-@rm -rf dist/tar

#
# Assets
#

YUI=java -jar yuicompressor-2.4.7.jar
FLAGS=--charset utf-8

assets: wipe numbersd.css numbersd.js

numbersd.css:
	$(MAKE) $(RES)/stylesheets/dashboard-min.css -B
	cd $(RES)/stylesheets && cat \
	bootstrap-min.css \
	dashboard-min.css \
	 > ../numbersd.css

numbersd.js:
	$(MAKE) $(RES)/javascripts/dashboard-min.js -B
	cd $(RES)/javascripts && cat \
	jquery-min.js \
	d3.v2-min.js \
	rickshaw-min.js \
	bootstrap-min.js \
	 > ../numbersd.js

wipe:
	-@rm $(RES)/numbersd.css $(RES)/numbersd.js

%-min.css: %.css
	$(YUI) $(FLAGS) --type css -o $@ $<

%-min.js: %.js
	$(YUI) $(FLAGS) --type js -o $@ $<
