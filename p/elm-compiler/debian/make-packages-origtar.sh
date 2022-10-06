#!/bin/bash

# Build everything from source, collect downloaded Elm packages into
# extra orig tarball, fetch unminified form of "marked" parser from
# Github.
#
# This script is intended to be run on upstream version upgrades.

set -eu

version=$( dpkg-parsechangelog -S Version | sed -E -e 's/^([^:]:)?([^-]*)(-.*)?$/\2/' )

rm -rf packages/ reactor/elm-stuff/
mkdir packages

dpkg-source --before-build .
fakeroot debian/rules clean build

rm -f packages/lock

markdown_js=$(find packages/ -name Markdown.js)
markdown_basedir=$(echo "$markdown_js" | sed -e 's/^\(.*[0-9]\/\).*/\1/')
marked_commit=$(< "$markdown_js" sed -ne '/\* commit/ {; s/.* commit *\(.*\)$/\1/; p; q; }')
tempdir=$(mktemp -d)

git init "$tempdir"
(
    cd "$tempdir"
    git fetch https://github.com/chjj/marked "$marked_commit"
    git checkout "$marked_commit"
)
cp "$tempdir/lib/marked.js" "$markdown_basedir/"
rm -rf "$tempdir"

tar \
    --numeric-owner --owner=0 --group=0 \
    --no-acls \
    --no-selinux \
    --no-xattrs \
    --mtime=@0 \
    --sort=name \
    -cJf ../elm-compiler_"$version".orig-packages.tar.xz packages/
