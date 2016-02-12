include stack.mk

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
	stack build . --exec 'serial-test --src-dir=$@ --lib-dir=ivory_serial'

.PHONY: can_test_out
can_test_out:
	rm -rf $@
	mkdir $@
	cp -r $(CAN_DATA)/* ./$@/
	cp -r test/can_test/can_test_artifacts/* ./$</
	stack build . --exec 'can-test --src-dir=$@ --lib-dir=ivory_can'

.PHONY: camera_vm_test_out
camera_vm_test_out:
	rm -rf $@
	mkdir $@
	cp -r $(CAMERA_VM_DATA)/* ./$@/
	cp -r test/camera_vm/camera_vm_test_artifacts/* ./$@
	stack build . --exec 'camera_vm-test --src-dir=$@ --lib-dir=camera_vm_tower'

TRAVIS_STACK ?= stack --no-terminal --system-ghc --skip-ghc-check
travis-test:
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make test
