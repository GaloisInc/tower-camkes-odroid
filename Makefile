IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

default: test

test: create-sandbox
test: serial-test
test: can-test

.PHONY: serial-test
serial-test:
	cabal run serial-test -- --out-dir=serial_test_out

.PHONY: can-test
can-test:
	cabal run can-test -- --out-dir=can_test_out


# Eventially in Travis we want to make rameses, too...
