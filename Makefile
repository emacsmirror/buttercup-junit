
export PATH:=$(PATH):$(HOME)/.cask/bin

CWD := $(shell pwd)
CASK = cask.sh
EMACS ?= emacs
ifeq ($(EMACS),t)
EMACS = emacs
endif

export emacs
# .PHONY: install-cask
# install-cask:
# 	-[ -d $(CASKDIR) ] && rm -rf $(CASKDIR)
# 	git clone https://github.com/cask/cask.git $(CASKDIR)
# 	sed -i s_https_http_g $(CASKDIR)/cask-bootstrap.el $(CASKDIR)/cask-cli.el
# 	git -C $(CASKDIR) commit -m "Use http sources" -- cask-bootstrap.el cask-cli.el
# 	git -C $(CASKDIR) config branch.master.rebase true
# 	$(CASK) upgrade-cask

TESTOPTS = -L . -L tests -batch -l buttercup-junit -f package-initialize

JUNIT ?= buttercup-junit.xml

BUTTERCUP_FLAGS += $(if $(BUTTERCUP_NO_COLOR),--no-color)

cask-install:
	$(CASK) install

build: cask-install
	$(CASK) build

package: cask-install
	$(CASK) package

test check: cask-install
	$(CASK) emacs $(TESTOPTS) -f buttercup-run-discover $(BUTTERCUP_FLAGS) tests

report: $(JUNIT)

$(JUNIT): cask-install
	mkdir -p $(@D)
	$(CASK) emacs $(TESTOPTS) -f buttercup-junit-run-discover --xmlfile $(JUNIT) $(if $(OUTER),--outer-suite "$(OUTER)") tests

stdout:
	$(CASK) emacs $(TESTOPTS) -f buttercup-junit-run-discover --xmlfile $(JUNIT) --junit-stdout --outer-suite foo

clean:
	$(CASK) clean-elc
	rm -rf .cask dist $(JUNIT)

.PHONY: build cask-install check clean package report test
