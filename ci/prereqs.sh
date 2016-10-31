#!/bin/sh

emacs_build_deps="libncurses5-dev texinfo liblockfile-dev librsvg2-dev \
 libgif-dev libtiff-dev libpng-dev libjpeg-dev libm17n-dev \
 libotf-dev libgpm-dev libdbus-1-dev autoconf automake autotools-dev \
 quilt  sharutils imagemagick libgnutls28-dev \
 libxml2-dev libselinux1-dev libmagick++-dev libgconf2-dev \
 libasound2-dev libacl1-dev zlib1g-dev libxpm-dev"
#  xaw3dg-dev libxaw7-dev

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

set -e
set -v

# sudo DEBIAN_FRONTEND=noninteractive apt-get install -yq libncurses5 liblockfile1 librsvg2-2 libgif4 libtiff5 libm17n-0 libotf0 libgpm2 libdbus-1-3 imagemagick libgnutls28 libxml2 libselinux1 libmagick\\+\\+6 libgconf-2-4 libasound2 libacl1 zlib1g libxpm4
# libpng12 libjpeg62-turbo
sudo DEBIAN_FRONTEND=noninteractive apt-get install -yqq libgnutls28

# Install or update evm
if [ -d ~/.evm/.git ]; then
	git -C ~/.evm/ pull
else
	curl -fsSkL --retry 9 --retry-delay 9 https://raw.github.com/rejeep/evm/master/go | bash
fi
mkdir -p ~/.evms
evm config path ~/.evms

# Install one Emacs version, why not the latest release
evm use emacs-25.1 || { install_prereqs; evm install --use emacs-25.1 >emacs-25.1.log 2>&1 ; }
if [ -d ~/.cask/.git ]; then
	#git -C ~/.cask/ pull
	cask upgrade-cask
else
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
fi

# InstalL the required Emacs versions from source
# vers="25.1 24.5 24.4 24.3 24.2"
# vers="25.1"
for ver in 25.1 24.5 24.4 24.3 24.2; do
	if 	evm use emacs-$ver ; then
		echo emacs-$ver already installed
	else
		install_prereqs
		echo evm install emacs-$ver
		evm install emacs-$ver >emacs-$ver.log 2>&1
	fi
	#cask install
done

# leave emacs-25.1 selected
echo evm use emacs-25.1
evm use emacs-25.1
