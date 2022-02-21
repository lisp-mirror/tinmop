#!/bin/bash

set -e

if [[ ! -d /src/tinmop ]]; then
    echo 'Please mount source at /src/tinmop'
    exit 1
elif [[ ! -d /src/out ]]; then
    echo 'Please mount output dir'
    exit 1
fi

cd /src/tinmop
rm -f /src/out/$(dpkg-architecture -qDEB_HOST_ARCH)/tinmop
if [[ -e Makefile ]]; then make distclean; fi
./configure
bash quick_quicklisp.sh --do-not-prompt
make -j$(nproc)

# Do tests
make install
echo "gemini://medusae.space/index.gmi?about" | ./tinmop -e scripts/gemget.lisp

# Copy binary to out dir
mkdir -p /src/out/$(dpkg-architecture -qDEB_HOST_ARCH)
mv /src/tinmop/tinmop /src/out/$(dpkg-architecture -qDEB_HOST_ARCH)/

# Clean and exit
if [[ -e Makefile ]]; then make distclean; fi
exit 0
