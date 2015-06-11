IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

.PHONY: test
test: serial-test
test: can-test

.PHONY: serial-test
serial-test:
	cabal run serial-test -- --src-dir=serial_test_out --lib-dir=ivory_serial

.PHONY: can-test
can-test:
	cabal run can-test -- --src-dir=can_test_out --lib-dir=ivory_can
