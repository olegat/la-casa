#!/bin/bash

set -ex
THIS_DIR=$(dirname $BASH_SOURCE[0])

build_url() {
  URL=${1}
  shift
  CONFIGURE_FLAGS=${@}
  NAME=$(basename ${URL})

  # Init build dir
  if [ -d /tmp/${NAME} ] ; then
    rm -fr /tmp/${NAME}
  fi
  mkdir /tmp/${NAME} && pushd /tmp/${NAME}

  # Download & extract archive
  wget ${URL}
  [[ "${NAME##*.}" = "gz" ]] && TAR_DECOMPRESSION="-z"
  [[ "${NAME##*.}" = "xz" ]] && TAR_DECOMPRESSION="-J"
  tar -x ${TAR_DECOMPRESSION} -f ${NAME}

  # Build & install
  cd $(dirname $(find . -mindepth 1 -maxdepth 2 -name configure))
  ./configure --prefix=/workspace/local ${CONFIGURE_FLAGS}
  make -j7 install

  # Clean build dir
  popd
  rm -fr /tmp/${NAME}
}

export PATH=/workspace/local/bin:$PATH
export LD_LIBRARY_PATH=/workspace/local/lib
export CFLAGS=-I/workspace/local/include
export CXXFLAGS=-I/workspace/local/include
export LDFLAGS=-L/workspace/local/lib
export PKG_CONFIG_PATH=/workspace/local/lib/pkgconfig

build_url https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz
build_url https://ftp.gnu.org/gnu/nettle/nettle-3.7.3.tar.gz
build_url https://www.gnupg.org/ftp/gcrypt/gnutls/v3.7/gnutls-3.7.2.tar.xz \
  --with-included-libtasn1 --with-included-unistring --without-p11-kit
build_url https://ftp.gnu.org/gnu/emacs/emacs-27.2.tar.gz
