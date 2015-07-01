IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

.PHONY: test
test: serial-test
test: can-test
test: camera_vm-test

.PHONY: serial-test
serial-test:
	cabal run serial-test -- --src-dir=serial_test_out --lib-dir=ivory_serial

.PHONY: can-test
can-test:
	rm -rf can_test_out
	cp -r test/can_test/can_test_artifacts ./can_test_out
	cabal run can-test -- --src-dir=can_test_out --lib-dir=ivory_can

.PHONY: camera_vm-test
camera_vm-test:
	rm -rf camera_vm_test_out
	cp -r test/camera_vm/camera_vm_artifacts ./camera_vm_test_out
	cabal run camera_vm-test -- --src-dir=camera_vm_test_out --lib-dir=camera_vm_tower
