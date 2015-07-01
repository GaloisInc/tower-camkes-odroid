# Include assumes this is driven by seL4 build.
# othercamkestargets.mk must come first: the main camkes makefile
# is included at the end of camkesmakefile.mk

CFLAGS += -DODROID

-include apps/camera_vm_test_out/othercamkestargets.mk
-include apps/camera_vm_test_out/componentlibs.mk
-include apps/camera_vm_test_out/camkesmakefile.mk
-include ramses.mk

