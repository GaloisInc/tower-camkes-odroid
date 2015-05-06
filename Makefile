IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

default: test

test: create-sandbox
test: serial-test

.PHONY: serial-test
serial-test:
	cabal run serial-test -- --out-dir=serial_test_out
	# Make rameses...
