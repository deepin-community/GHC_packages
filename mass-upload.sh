#!/bin/bash

if [ -z "$1" -o "$1" = '--help' ]
then
	echo "Usage: $0 [-k keyid] file.changes file.changes..."
	echo
	echo "For each of the changes, will"
	echo " * sign it"
	echo " * dput it"
	echo " * and renames the file to file.changes-done"
	exit
fi

if [ "$1" = "-k" ];
then
	shift;
	debsignarg="-k $1";
	shift;
fi

set -e
status=0

while [ -n "$1" ]
do
	changes=$1
	shift

	#pkg="$(grep-dctrl '' -s Source -n "$changes")"
	pkg="$(grep ^Source $changes | cut -c9-)"

	debsign --re-sign $debsignarg "$changes"
	dput ssh-upload "$changes"
	mv "$changes" "$changes-done"

done

exit $status
