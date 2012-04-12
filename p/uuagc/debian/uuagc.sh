#!/bin/bash

if [ -e debian/bootstrapping ]
then
	exec uuagc "$@"
else
	exit 0
fi
