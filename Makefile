
export PATH:=$(PATH):/home/olani/.cask/bin

CWD := $(shell pwd)
CASK = cask
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

TESTOPTS = -Q -L . -L tests -batch -l buttercup-junit -f package-initialize

JUNIT ?= buttercup-junit.xml

cask-install:
	$(CASK) install

build: cask-install
	$(CASK) build

package: cask-install
	$(CASK) package

test: cask-install
	$(CASK) exec $(EMACS) $(TESTOPTS) -f buttercup-run-discover

report: $(JUNIT)

$(JUNIT): cask-install
	$(CASK) exec $(EMACS) $(TESTOPTS) -f buttercup-junit-run-discover --xmlfile $(JUNIT) $(if $(OUTER),--outer-suite "$(OUTER)")

stdout:
	$(CASK) exec $(EMACS) $(TESTOPTS) -f buttercup-junit-run-discover --xmlfile $(JUNIT) --junit-stdout --outer-suite foo

clean:
	$(CASK) clean-elc
	rm -rf .cask dist $(JUNIT)

.PHONY: clean report test package build cask-install
