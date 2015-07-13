#!/bin/sh

set -e

until [ -z "$1" ]
do
    case "$1" in
        *)
            DIRS="$DIRS $1"
            ;;
    esac
    shift
done

if [ -z "$DIRS" ]
then 
   cat <<__END__
Usage: $0 [dir ...]

Expects directories containing the contents of the debian/ directory of a
Debian source package (i.e. control, changelog, watch, ...). Uses the watch
file to retrieve the upstream tarball (unless it already exists in the local
directory), creates a debian.tar.xz tarball from the given directory (ignoring
the ususal suspects like _darcs and *~) and creates a corresponding .dsc file;
all without unpacking the upstream tarball.
__END__
   exit 1
fi

for DIR in $DIRS
do
    if [ ! -e $DIR/control -o ! -e $DIR/changelog ]
    then
      echo "Did not find $DIR/control or $DIR/changelog."
      echo "Is the repository in the debian/-only format?"
      exit 1
    fi

    VERSION=`dpkg-parsechangelog -l$DIR/changelog -c1 -SVersion`
    PACKAGE=`dpkg-parsechangelog -l$DIR/changelog -c1 -SSource`
    UPSTREAM=`echo $VERSION | cut -d- -f1` # this could be improved
    if echo $UPSTREAM | fgrep -q : ; then
    	UPSTREAM=`echo $UPSTREAM | cut -d: -f2-`
    	VERSION=`echo $VERSION | cut -d: -f2-`
    fi
    TARBALL_GZ=${PACKAGE}_$UPSTREAM.orig.tar.gz
    TARBALL_BZ2=${PACKAGE}_$UPSTREAM.orig.tar.bz2
    TARBALL_XZ=${PACKAGE}_$UPSTREAM.orig.tar.xz
    # see 375138 for why this doesn't work as well as it could. Fall back to apt-get source
    # as a last resort.
    [ ! -e $TARBALL_GZ -a ! -e $TARBALL_BZ2 -a ! -e $TARBALL_XZ ] && \
       ( uscan \
        --rename \
        --force-download \
        --package "$PACKAGE" \
        --download \
        --watchfile $DIR/watch \
        --copyright-file $DIR/copyright \
        --download-version "$UPSTREAM" \
        --upstream-version "$UPSTREAM" \
        --destdir . \
	--rename ||
        apt-get source "$PACKAGE" --tar-only )

    if [ ! -e $TARBALL_GZ -a ! -e $TARBALL_BZ2 -a ! -e $TARBALL_XZ ]
    then
      echo "Couldn't download tarball with uscan or apt-get source. See above for errors"
      exit 1
    fi

    TARBALL=""

    if [ -e $TARBALL_GZ ]
    then
    	TARBALL="$TARBALL_GZ"
    else
    	if [ -e $TARBALL_XZ ]
	then
	    TARBALL="$TARBALL_XZ"
	else
	    if [ -e $TARBALL_BZ2 ]
	    then
		TARBALL="$TARBALL_BZ2"
	    else
		echo "Unreachable code"
		exit 1
	    fi
	fi
    fi
    
    DEBIAN_TARBALL=${PACKAGE}_${VERSION}.debian.tar.xz
    # -I line taken from "man dpkg-source"
    tar --create \
	--xz \
	--transform s,^.,debian, \
	--force-local \
	--file $DEBIAN_TARBALL \
	--directory $DIR \
	"--exclude=*.a" "--exclude=*.la" "--exclude=*.o" "--exclude=*.so" "--exclude=.*.sw?" "--exclude=*/*~" "--exclude=,,*" "--exclude=.[#~]*" "--exclude=.arch-ids" "--exclude=.arch-inventory" "--exclude=.be" "--exclude=.bzr" "--exclude=.bzr.backup" "--exclude=.bzr.tags" "--exclude=.bzrignore" "--exclude=.cvsignore" "--exclude=.deps" "--exclude=.git" "--exclude=.gitattributes" "--exclude=.gitignore" "--exclude=.gitmodules" "--exclude=.hg" "--exclude=.hgignore" "--exclude=.hgsigs" "--exclude=.hgtags" "--exclude=.shelf" "--exclude=.svn" "--exclude=CVS" "--exclude=DEADJOE" "--exclude=RCS" "--exclude=_MTN" "--exclude=_darcs" "--exclude={arch}" \
	.
    dpkg-source \
	-c$DIR/control -l$DIR/changelog \
	--format="3.0 (custom)" --target-format="3.0 (quilt)" \
	-b / \
	$DEBIAN_TARBALL \
	$TARBALL

    DSC=${PACKAGE}_${VERSION}.dsc
    if [ -e $DSC ]
    then
	echo "Successfully created $DSC."
    else
	echo "Failed to create $DSC."
    fi
done
