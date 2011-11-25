#!/bin/bash

# Program to find all copyright holders in the xmonad-contrib package.
#
# The Debian copyrights file can be updated by deleting the current list
# of per file copyrights and then running:
#
#     debian/find-copyrights.sh >> debian/copyright

find . -name \*.hs |
	while read f ; do
		if test `grep -c Copyright $f` -ge 1 ; then
			echo $f | sed "s|.*XMonad|XMonad|"
			grep Copyright $f
			grep License $f
			echo
			fi
		done
