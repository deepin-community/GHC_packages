#!/bin/bash

set -e

if [ -z "$1" -o "$1" = "--help" ]
then
  echo "Usage: $0 foo.changes"
  echo
  echo "Signs the .changes file and the corresponding .dsc file in a temporary"
  echo "location (to avoid touching the original files), uploads them to the archive"
  echo "using \"dput ssh-upload\" and tags them in the repository and pushes the tag."
  echo
  echo "Checks that the distribution is not UNRELEASED and that the tag does"
  echo "not exist already (by tagging first)."
  echo
  exit 1
fi

changes="$@"

root="$(realpath --relative-to=$PWD "$(git rev-parse --show-toplevel)")"
tmpdir=$root/uploads

if [ -e $tmpdir ]
then
	echo "$tmpdir exists, please remove"
	exit 1
fi
trap 'rm -rf "$tmpdir"' EXIT
mkdir $tmpdir

for c in $changes
do
	src="$(grep ^Source "$c"|grep-dctrl -s Source -n '' )"
	$root/tag.pl $root/p/$src
	dcmd cp -v "$c" "$tmpdir"
	debsign "$tmpdir"/"$(basename "$c")"
	dput ssh-upload "$tmpdir"/"$(basename "$c")"
done
git push --tags
