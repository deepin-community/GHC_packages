#!/bin/bash

set -e

VERSION=$2

TMPDIR=$(mktemp -d -p ../ -t haskell-hadrian-repack.XXXXX)
echo Using temporary directory ${TMPDIR}

echo Unpack tarball
tar -xf ../haskell-hadrian_${VERSION}.orig.tar.xz -C ${TMPDIR}

echo Repack only the hadrian directory
tar -cJf ../haskell-hadrian_${VERSION}.orig.tar.xz -C ${TMPDIR}/ghc-${VERSION} hadrian

echo Remove temporary directory
rm -rf ${TMPDIR}
