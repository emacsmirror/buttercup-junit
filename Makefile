# Copyright (C) 2016-2018 by Ola Nilsson <ola.nilsson@gmail.com>

export PATH:=$(PATH):$(HOME)/.cask/bin

CWD := $(shell pwd)
CASK ?= cask.sh
EMACS ?= emacs
ifeq ($(EMACS),t)
EMACS = emacs
endif

TESTOPTS = -L . -L tests -batch -l buttercup-junit -f package-initialize

JUNIT ?= buttercup-junit.xml

BUTTERCUP_FLAGS += $(if $(BUTTERCUP_NO_COLOR),--no-color)

build: cask-install
	$(CASK) build

cask-install:
	$(CASK) install

check test: cask-install
	$(CASK) emacs $(TESTOPTS) -f buttercup-run-discover $(BUTTERCUP_FLAGS) tests

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

.PHONY: build cask-install check clean package report stdout test
