#!/bin/bash

schroot=$1
shift

result=$1
shift

if [ -z "$schroot" -o -z "$result" -o "$schroot" = '--help' ]
then
	echo "Usage: $0 <chroot> <results/> dirs..."
	echo
	echo "Check each directory whether it is marked for release (distribution set) but"
	echo "not tagged with the latest version. Those packages will be built with sbuild"
	echo "inside the given chroot. The result will be stored in the second directory"
	echo "passed. Also, all packages in this directory are made available as a repository"
	echo "inside the chroot."
	echo "If the build is sucessful, it runs \"debcommit -r\" in the repository to create"
	echo "the correct tag."
	echo
	echo "Until http://bugs.debian.org/608840 is fixed you have to ensure that results/"
	echo "is set up as an apt source in the source schroot. This script will however"
	echo "keep Packages and Sources up-to-date. For this to work the schroot should have"
	echo "     Apt::Get::AllowUnauthenticated 1;"
	echo "set, e.g. in a file named /etc/apt/apt.conf.d/unauthenticated."
	exit
fi

set -e

# unlock sudo password
sudo true

tobuild=""
nottobuild=""
released=""

DEBIAN2DSC=debian2dsc
if ! which $DEBIAN2DSC >/dev/null
then
	pushd $(dirname $0) >/dev/null
	DEBIAN2DSC=$PWD/debian2dsc.sh
	popd >/dev/null
fi
if ! which $DEBIAN2DSC >/dev/null
then
	echo "Could not find debian2dsc ($DEBIAN2DSC)."
	exit 1
fi

ORDER_SOURCES=order-sources
if ! which $ORDER_SOURCES >/dev/null
then
	pushd $(dirname $0) >/dev/null
	ORDER_SOURCES=$PWD/order-sources.pl
	popd >/dev/null
fi
if ! which $ORDER_SOURCES >/dev/null
then
	echo "Could not find order-sources ($ORDER_SOURCES)."
	exit 1
fi

arch="$(schroot -c $schroot -- dpkg-architecture -qDEB_BUILD_ARCH)"
echo "The schroot $schroot builds $arch packages."

echo "Finding out what to build..."
while [ -n "$1" ]
do
	dir=$1
	shift

	if ! pushd "$dir" >/dev/null
	then
		echo "Failed to switch to \"$dir\""
		continue
	fi

	if [ ! -e debian/changelog ]
	then
		echo "No changelog file found, skipping $dir"
		popd >/dev/null
		continue
	fi

	set -e
	dist=$(dpkg-parsechangelog -ldebian/changelog -c1 -SDistribution)
	set +e
	if [ "$dist" = "UNRELEASED" ]
	then
		nottobuild="$nottobuild $dir"
	else
		package=$(dpkg-parsechangelog -ldebian/changelog -c1 -SVersion)
		version=$(dpkg-parsechangelog -ldebian/changelog -c1 -SSource)
		version_fixed="$(echo $version|tr ':~' '_')"
		if git tag -l "$package_v$version" | grep -q .
		then
			released="$released $dir"
		else
			tobuild="$tobuild $dir"
		fi
	fi
	popd >/dev/null
done

echo "Figuring out build order..."
if [ -n "$tobuild" ]
then
	tobuild="$($ORDER_SOURCES $tobuild)"
fi


echo "Creating source packages..."
tmpdir="$(mktemp -d --tmpdir=$result mass-build-XXXXX)"
tobuild2=""
for dir in $tobuild
do
	if ! pushd "$dir" >/dev/null
	then
		echo "Failed to switch to \"$dir\""
		continue
	fi
	repodir=$PWD
	popd >/dev/null

	if ! pushd "$result" >/dev/null
	then
		echo "Failed to switch to $result"
		exit
	fi

	PACKAGE=`dpkg-parsechangelog -l$repodir/debian/changelog -c1 -SSource`
	VERSION=`dpkg-parsechangelog -l$repodir/debian/changelog -c1 -SVersion`
        UPSTREAM=`echo $VERSION | cut -d- -f1` # this could be improved
        if echo $UPSTREAM | fgrep -q : ; then
        	UPSTREAM=`echo $UPSTREAM | cut -d: -f2-`
        	VERSION=`echo $VERSION | cut -d: -f2-`
        fi
	DISTRIBUTION=$(dpkg-parsechangelog -l$repodir/debian/changelog -c1 -SDistribution)
	DSC=${PACKAGE}_${VERSION}.dsc

	if ! $DEBIAN2DSC $repodir/debian && [ -e $DSC ]
	then
		echo "Failed to create $DSC for $dir."
		buildfailed="$buildfailed $dir"
		popd >/dev/null
		continue
	fi

	popd >/dev/null

	if ! pushd "$tmpdir" >/dev/null
	then
		echo "Failed to switch to $tmpdir"
		exit
	fi

	if ! ln -s "../$DSC" 
	then
		echo "Failed to link $DSC"
		buildfailed="$buildfailed $dir"
		popd >/dev/null
		continue
	fi

	nextround="$nextround $dir"
	popd >/dev/null
done

retry=yes

while [ "$retry" = yes ]
do

retry=no
thisround="$nextround"
nextround=""

echo "Updating Packages"
if ! pushd "$result" >/dev/null
then
	echo "Failed to switch to $result"
	exit
fi
dpkg-scanpackages . > Packages

echo "Upgrading schroot"
sudo sbuild-update --update --upgrade $schroot
popd >/dev/null


echo "Checking build-installability..."
if ! pushd "$tmpdir" >/dev/null
then
	echo "Failed to switch to $tmpdir"
	exit
fi

dpkg-scansources . > Sources
schroot -c $schroot -- bash -c "cat /var/lib/apt/lists/*Packages" > Packages
dose-builddebcheck --latest --explain --failures --deb-native-arch=$arch Packages Sources > ../edos-failures.log
installable="$(dose-builddebcheck --latest --successes --deb-native-arch=$arch Packages Sources|perl -ne 'print "$1\n" if /package: src:(.*)/')"
popd >/dev/null

#echo "The following $(echo $uninstallable|wc -w) packages are uninstallable:"
#echo $uninstallable

echo "Starting builds..."
for dir in $thisround
do
	echo "Building $dir..."
	if ! pushd "$dir" >/dev/null
	then
		echo "Failed to switch to \"$dir\""
		continue
	fi
	repodir=$PWD
	popd >/dev/null

	PACKAGE=`dpkg-parsechangelog -l$repodir/debian/changelog -c1 -SSource`

	if ! echo "$installable" | fgrep -xq "$PACKAGE"
	then
		echo "Package was found to be uninstallable, skipping.."
		nextround="$nextround $dir"
		continue
	fi

	VERSION=`dpkg-parsechangelog -l$repodir/debian/changelog -c1 -SVersion`
        UPSTREAM=`echo $VERSION | cut -d- -f1` # this could be improved
        if echo $UPSTREAM | fgrep -q : ; then
        	UPSTREAM=`echo $UPSTREAM | cut -d: -f2-`
        	VERSION=`echo $VERSION | cut -d: -f2-`
        fi
	DISTRIBUTION=$(dpkg-parsechangelog -l$repodir/debian/changelog -c1 -SDistribution)
	DSC=${PACKAGE}_${VERSION}.dsc


	pushd $result >/dev/null

	dpkg-scanpackages . > Packages
	sudo sbuild-update --update $schroot

	if [ ! -e $DSC ]
	then
		echo "File $DSC not found, should have been created earlier."
		buildfailed="$buildfailed $dir"
		popd >/dev/null
		continue
	fi
	

# Does not work, see http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=608840
#	cat > setup.sh <<__END__
##!/bin/bash
#echo deb file://$PWD ./ > /etc/apt/sources.list.d/mass-build.list'
#echo deb-src file://$PWD ./ > /etc/apt/sources.list.d/mass-build.list'
#__END__
#	chmod +x setup.sh

	if sbuild --dist "$DISTRIBUTION" \
		--chroot="$schroot" \
		--build-dep-resolver=aptitude \
		--arch=$arch \
		--run-lintian \
		--arch-all \
		--no-apt-update \
		--no-apt-upgrade \
		--source \
		"$DSC"
#		--chroot-setup-commands=$PWD/setup.sh \
	then
		#rm -f setup.sh
		buildok="$buildok $dir"
		popd >/dev/null
		pushd $dir >/dev/null
		debcommit -r
		popd >/dev/null
		echo "Building $dir was successful"
		retry=yes
	else
		#rm -f setup.sh
		buildfailed="$buildfailed $dir"
		popd >/dev/null
		pushd $dir >/dev/null
		if darcs diff|diffstat|tail -n1|fgrep -q '1 file changed, 2 insertions(+), 2 deletions(-)'
		then
			echo "Reverting release marker."
			darcs revert -a
		fi
		if darcs diff|diffstat|tail -n1|fgrep -q '1 file changed, 1 insertion(+), 1 deletion(-)'
		then
			echo "Reverting release marker."
			darcs revert -a
		fi
		popd >/dev/null
		echo "Building $dir failed"
	fi
done

done
rm -rf "$tmpdir"

echo "Summary:"
echo "Packages not to build:"
echo $nottobuild
echo "Packages already released:"
echo "($(echo $released |wc -w) packages)"
echo "Packages not yet buildable:"
echo $nextround
echo "Packages successfully build:"
echo $buildok
echo "Packages failed to build:"
echo $buildfailed
