# Copyright (C) 2016-2019 by Ola Nilsson <ola.nilsson@gmail.com>

export PATH:=$(PATH):$(HOME)/.cask/bin

CWD := $(shell pwd)
CASK ?= cask.sh
EMACS ?= emacs
ifeq ($(EMACS),t)
EMACS = emacs
endif

BATCHOPTS = --batch -q -l .emacs/lisp/init.el
EMACS_BATCH = $(EMACS) $(BATCHOPTS)
EMACS_PACKAGING = $(EMACS) --batch -q -l .emacs/lisp/packaging.el
TESTFILEFLAGS =
EMACS_TEST = $(EMACS) $(BATCHOPTS) $(TESTFILEFLAGS)

TESTOPTS = -L . -L tests -batch -l buttercup-junit

JUNIT ?= buttercup-junit.xml

BUTTERCUP_FLAGS += $(if $(BUTTERCUP_NO_COLOR),--no-color)

PACKAGE_USER_DIR := $(shell $(CASK) package-directory)

EMACSVER := $(shell $(EMACS) --batch --eval '(message emacs-version)')

all: build

setup:
	$(EMACS_BATCH)

build: cask-install
	$(CASK) build

cask-install:
	$(CASK) install

check test: cask-install
	$(CASK) emacs $(TESTOPTS) -f buttercup-run-discover $(BUTTERCUP_FLAGS) tests

lint: elisp-lint package-lint

elisp-lint: cask-install
	$(CASK) emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch \
	 $(if $(filter-out 24.3%,$(EMACSVER)),,--no-byte-compile) \
	 --no-indent --no-indent-character --no-fill-column buttercup-junit.el

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
