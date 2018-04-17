# -*- mode: snippet -*-
# name: Kernel module Makefile
# expand-env: ((yas-indent-line 'fixed) (yas-wrap-around-region 'nil))
# key: kmodm
# --

# 1. command line
# 2. a build symlink in current directory
# 3. current kernel release build directory
KDIR ?= $(if $(shell test -L build && test -d build && echo build-found),$(shell readlink build),/lib/modules/$(shell uname -r)/build)

# required by sudo make install, otherwise PWD is empty
PWD := $(shell pwd)

.PHONY: all clean install

all:
	$(MAKE) -C $(KDIR) M=$(PWD) modules

clean:
	$(MAKE) -C $(KDIR) M=$(PWD) clean

install:
	$(MAKE) -C $(KDIR) M=$(PWD) modules_install
