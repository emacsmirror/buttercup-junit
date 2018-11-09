# Copyright (C) 2016-2018 by Ola Nilsson <ola.nilsson@gmail.com>

export PATH:=$(PATH):$(HOME)/.cask/bin

CWD := $(shell pwd)
CASK ?= cask.sh
EMACS ?= emacs
ifeq ($(EMACS),t)
EMACS = emacs
endif

TESTOPTS = -L . -L tests -batch -l buttercup-junit

JUNIT ?= buttercup-junit.xml

BUTTERCUP_FLAGS += $(if $(BUTTERCUP_NO_COLOR),--no-color)

PACKAGE_USER_DIR := $(shell $(CASK) package-directory)

build: cask-install
	$(CASK) build

cask-install:
	$(CASK) install

check test: cask-install
	$(CASK) emacs $(TESTOPTS) -f buttercup-run-discover $(BUTTERCUP_FLAGS) tests

lint: package-lint

package-lint: cask-install
	$(CASK) emacs -Q --batch -l package-lint.el \
     --eval "(setq package-user-dir \"$(PACKAGE_USER_DIR)\")" \
	 --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))" \
	 -f package-lint-batch-and-exit buttercup-junit.el

package: cask-install
	$(CASK) package

report: $(JUNIT)

$(JUNIT): cask-install
	mkdir -p $(@D)
	$(CASK) emacs $(TESTOPTS) -f buttercup-junit-run-discover --xmlfile $(JUNIT) $(if $(OUTER),--outer-suite "$(OUTER)") tests

stdout:
	$(CASK) emacs $(TESTOPTS) -f buttercup-junit-run-discover --xmlfile $(JUNIT) --junit-stdout --outer-suite foo

clean:
	$(CASK) clean-elc
	rm -rf .cask dist $(JUNIT)

.PHONY: build cask-install check clean package lint report stdout test
