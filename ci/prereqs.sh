#!/bin/sh

emacs_build_deps="libncurses5-dev texinfo liblockfile-dev librsvg2-dev \
 libgif-dev libtiff-dev xaw3dg-dev libpng-dev libjpeg-dev libm17n-dev \
 libotf-dev libgpm-dev libdbus-1-dev autoconf automake autotools-dev \
 quilt libxaw7-dev sharutils imagemagick libgtk-3-dev libgnutls28-dev \
 libxml2-dev libselinux1-dev libmagick++-dev libgconf2-dev \
 libasound2-dev libacl1-dev zlib1g-dev libxpm-dev"

set -e
set -x

install_prereqs() {
	if [ ! "$once" ]; then
		# Install Emacs build deps
		sudo apt-get -qq update
		sudo DEBIAN_FRONTEND=noninteractive apt-get -y -qq install $emacs_build_deps
		once="once"
	else
		echo Requirements already installed
	fi
}

# Install or update evm
if [ -d ~/.evm/.git ]; then
	git -C ~/.evm/ pull
else
	curl -fsSkL --retry 9 --retry-delay 9 https://raw.github.com/rejeep/evm/master/go | bash
fi
mkdir -p ~/.evms
evm config path ~/.evms

# Install one Emacs version, why not the latest release
evm use emacs-25.1 || { install_prereqs; evm install --use emacs-25.1  > emacs-25.1.log ; }
if [ -d ~/.cask/.git ]; then
	git -C ~/.cask/ pull
	#cask upgrade-cask
else
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

# InstalL the required Emacs versions from source
# vers="25.1 24.5 24.4 24.3 24.2"
vers="25.1"
for ver in 25.1 24.5 24.4 24.3 24.2; do
	if 	evm use emacs-$ver ; then
		echo emacs-$ver already installed
	else
		install_prereqs
		evm install emacs-$ver > emacs-$ver.log
	fi
	#cask install
done
# evm use emacs-24.4         || evm install emacs-24.4
# evm use emacs-24.3         || evm install emacs-24.3
# evm use emacs-24.2         || evm install emacs-24.2
# evm use emacs-24.1         || evm install emacs-24.1
# evm use emacs-git-snapshot || evm install emacs-git-snapshot
evm use emacs-25.1
