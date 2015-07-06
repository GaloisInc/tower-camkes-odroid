IVORY_REPO ?= ../ivory
TOWER_REPO ?= ../tower
include Makefile.sandbox

UART_DATA      := data/uart
CAN_DATA       := data/can
CAMERA_VM_DATA := data/camera_vm

.PHONY: test
test: serial_test_out
test: can_test_out
test: camera_vm_test_out

.PHONY: serial_test_out
serial_test_out:
	rm -rf $@
	mkdir $@
	cp -r $(UART_DATA)/* ./$@/
	cabal run serial-test -- --src-dir=$@ --lib-dir=ivory_serial

.PHONY: can_test_out
can_test_out:
	rm -rf $@
	mkdir $@
	cp -r $(CAN_DATA)/* ./$@/
	cp -r test/can_test/can_test_artifacts/* ./$</
	cabal run can-test -- --src-dir=$@ --lib-dir=ivory_can

.PHONY: camera_vm_test_out
camera_vm_test_out:
	rm -rf $@
	mkdir $@
	cp -r $(CAMERA_VM_DATA)/* ./$@/
	cp -r test/camera_vm/camera_vm_test_artifacts/* ./$@
	cabal run camera_vm-test -- --src-dir=$@ --lib-dir=camera_vm_tower
