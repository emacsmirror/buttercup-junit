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
TESTFILEFLAGS = -L . -L tests -l buttercup-junit
EMACS_TEST = $(EMACS) $(BATCHOPTS) $(TESTFILEFLAGS)

JUNIT ?= buttercup-junit.xml
BUTTERCUP_FLAGS += $(if $(BUTTERCUP_NO_COLOR),--no-color)
EMACSVER := $(shell $(EMACS) --batch --eval '(message emacs-version)')

all: build

setup:
	$(EMACS_BATCH)

build:
	$(EMACS_BATCH) --eval '(byte-recompile-directory "'$(PWD)'" 0)'

report: $(JUNIT)

.PHONY: $(JUNIT)
$(JUNIT):
	mkdir -p $(@D)
	$(EMACS_TEST) -f buttercup-junit-run-discover --xmlfile $(JUNIT) $(if $(OUTER),--outer-suite "$(OUTER)") tests

stdout:
	$(EMACS_TEST) -f buttercup-junit-run-discover --xmlfile $(JUNIT) --junit-stdout --outer-suite tests

check test:
	$(EMACS_TEST) -f buttercup-run-discover $(BUTTERCUP_FLAGS) tests

lint: elisp-lint package-lint

elisp-lint:
	$(EMACS_BATCH) -l elisp-lint.el -f elisp-lint-files-batch \
	 $(if $(filter-out 24.3%,$(EMACSVER)),,--no-byte-compile) \
	 --no-indent --no-indent-character --no-fill-column buttercup-junit.el

package-lint:
	$(EMACS_BATCH) -l package-lint.el -f package-lint-batch-and-exit buttercup-junit.el

## Packaging
buttercup-junit-pkg.el: buttercup-junit.el
	$(EMACS_PACKAGING) buttercup-junit.el -f generate-description-file

dist/buttercup-junit-readme.txt: buttercup-junit.el
	mkdir -p $(dir $@)
	$(EMACS_PACKAGING) buttercup-junit.el -f generate-readme > $@

package_name := $(shell $(EMACS_PACKAGING) buttercup-junit.el -f package-file-name 2>/dev/null)
package_dir = $(basename $(package_name))

package: dist/$(package_name) dist/buttercup-junit-readme.txt

dist/$(package_name): buttercup-junit.el
	mkdir -p $(dir $@)
	cp $< $@

## Cleaning

lisp-clean:
	rm -f *.elc tests/*.elc tests/test-support/*.elc

clean: lisp-clean
	rm -rf dist

reallyclean: clean
	rm -rf .emacs/elpa .emacs/.emacs-custom.el

.PHONY: all build clean elisp-lint lisp-clean package package-lint reallyclean report setup test
