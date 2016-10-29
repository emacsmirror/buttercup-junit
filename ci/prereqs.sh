#!/bin/sh
set -e

# Install Emacs build deps
sudo apt-get -qq update
sudo apt-get -y -qq install emacs24-nox libxpm-dev libncurses5-dev texinfo \
	 liblockfile-dev librsvg2-dev autoconf automake autotools-dev sharutils zlib1g-dev
sudo DEBIAN_FRONTEND=noninteractive apt-get -y -qq build-dep emacs24-nox

# Install or update evm
if [ -d ~/.evm/.git ]; then
	git -C ~/.evm/ pull
else
	curl -fsSkL --retry 9 --retry-delay 9 https://raw.github.com/rejeep/evm/master/go | bash
fi
mkdir -p ~/.evms
evm config path ~/.evms

# Install one Emacs version, why not the latest release
evm use emacs-25.1 || evm install --use emacs-25.1
if [ -d ~/.cask/.git ]; then
	git -C ~/.cask/ pull
	#cask upgrade-cask
else
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

# InstalL the required Emacs versions from source
for ver in 25.1 24.5 24.4 24.3 24.2; do
	evm use emacs-$ver || evm install emacs-$ver
	#cask install
done
# evm use emacs-24.4         || evm install emacs-24.4
# evm use emacs-24.3         || evm install emacs-24.3
# evm use emacs-24.2         || evm install emacs-24.2
# evm use emacs-24.1         || evm install emacs-24.1
# evm use emacs-git-snapshot || evm install emacs-git-snapshot
evm use emacs-25.1
