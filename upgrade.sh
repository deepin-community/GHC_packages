#!/bin/bash

set -e

if [ ! -d debian ]
then
  echo "No debian/ directory found!"
  echo "Run me in the packaging directory."
  exit 1
fi

if [ "$1" = "-D" ]
then
	dist="$2"
	shift
	shift
else
	dist=unstable
fi


cabal_name="$(cat debian/watch | grep http | cut -d/ -f5)"
package="$(grep-dctrl -n -s Source . < debian/control)"
old_version=`dpkg-parsechangelog -ldebian/changelog -c1 | grep-dctrl -n -s Version .`
old_version=`echo $old_version | cut -d- -f1` # this could be improved
if echo $old_version | fgrep -q : ; then
	old_version=`echo $old_version | cut -d: -f2-`
	epoch=`echo $old_version | cut -d: -f1`:
else
        epoch=''
fi

version=$(grep "^$cabal_name " ../../../package-plan/packages.txt|cut -d\  -f2)

if [ -z "$version" ]
then
	echo "could not detect version to upgrade to"
	exit 1
fi

debchange --changelog debian/changelog --newversion="$epoch$version-1" 'New upstream release'
origtargz -u

if fgrep -q 'DEB_ENABLE_TESTS = yes' debian/rules
then
  test=""
else
  test="--no-tests"
fi

cabal-debian --official --upgrade $test
find debian/ -name '*~' -delete

dch -D $dist -r ''

git commit . -q -m "$cabal_name: Upgrading from $old_version to $version"

echo "Upgraded $cabal_name to $version"
echo "Please check git show HEAD for sanity."
echo "Please check http://hdiff.luite.com/cgit/$cabal_name/diff/?id=$version&id2=$old_version for interesting changes."
if test -d debian/patches
then
	echo "Please refresh the patches"
fi



