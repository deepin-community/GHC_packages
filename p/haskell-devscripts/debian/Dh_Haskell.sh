cpu(){
  ghc -e 'putStr System.Info.arch'
}

os(){
  ghc -e 'putStr System.Info.os'
}

ghcjs_version(){
  ghcjs --numeric-ghcjs-version
}

ghcjs_ghc_version(){
  ghcjs --numeric-ghc-version
}

package_prefix(){
    echo $1 | sed -n -e 's|^\([^-]*\)-.*-[^-]*$|\1|p'
}

package_hc(){
    echo $1 | sed -n -e 's|^lib\([^-]*\)-.*-[^-]*$|\1|p'
}

package_ext(){
    case $1 in
	# I'm told the ghc build uses these scripts, hence these special cases
	ghc) echo "dev";;
	ghc-prof) echo "prof";;
	*) echo $1 | sed -n -e 's|^[^-]*-.*-\([^-]*\)$|\1|p';;
    esac
}

packages_hc(){
    DEB_DEFAULT_COMPILER=$1
    DEB_PACKAGES=$2
    hcs=`{ for i in ${DEB_PACKAGES}; do package_hc $i; done; } | sort -u`
    if [ `echo ${hcs} | wc -w` = 0 ]; then hcs=${DEB_DEFAULT_COMPILER}; fi
    if [ `echo ${hcs} | wc -w` != 1 ]; then echo "Multiple compilers not supported: ${hc}"; exit 1; fi
    echo ${hcs}
}

hc_libdir(){
    case $1 in
      ghc) echo "usr/lib/haskell-packages/ghc/lib";;
      ghcjs) echo "usr/lib/ghcjs/.cabal/lib";;
      *) echo "Don't know package_libdir for $1" >&2; exit 1;;
    esac
}

package_libdir(){
    hc_libdir `package_hc $1`
}

hc_pkgdir(){
    case $1 in
	ghc) echo "var/lib/ghc/package.conf.d";;
        ghcjs) echo "usr/lib/ghcjs/.ghcjs/`cpu`-`os`-`ghcjs_version`-`ghcjs_ghc_version`/ghcjs/package.conf.d";;
        *) echo "Don't know pkgdir for $1" >&2; exit 1;;
    esac
}

package_pkgdir(){
    hc_pkgdir `package_hc $1`
}

hc_prefix(){
    case $1 in
      ghc) echo "usr";;
      ghcjs) echo "usr/lib/ghcjs";;
      *) echo "Don't know prefix for compiler $1" >&2; exit 1;;
    esac
}

hc_haddock(){
    case $1 in
	ghc) echo "haddock";;
	ghcjs) echo "haddock-ghcjs";;
	*) echo "Don't know pkgdir for $1" >&2; exit 1;;
    esac
}

hc_docdir(){
    hc=$1
    pkgid=$2
    echo "usr/lib/${hc}-doc/haddock/${pkgid}/"
}

hc_htmldir(){
    hc=$1
    CABAL_PACKAGE=$2
    echo "usr/share/doc/lib${hc}-${CABAL_PACKAGE}-doc/html/"
}

hc_hoogle(){
    local hc
    hc=$1
    echo "/usr/lib/${hc}-doc/hoogle/"
}

strip_hash(){
	echo "$1" | sed 's/-................................$//'
}

sort_uniq(){
    {
	for i in "$@" ; do
	    echo $i
	done
    } | sort -u | tr "\n" " "
}

dependency(){
    local package
    local version
    local next_upstream_version
    package=$1
    version=`dpkg-query --showformat='${Version}' -W $package`
    next_upstream_version=`echo $version | sed  -e 's/-[^-]*$//' -e 's/$/+/'`
    echo "$package (>= $version), $package (<< $next_upstream_version)"
}

ghc_pkg_field(){
    hc=$1
    pkg=$2
    field=$3
    ${hc}-pkg --global field ${pkg} ${field} | head -n1
}

providing_package_for_ghc(){
    local package
    local dep
    local dir
    local dirs
    local lib
    local hc
    hc=$1
    dep=`strip_hash $2`
    dirs=`ghc_pkg_field $hc $dep library-dirs | grep -i ^library-dirs | cut -d':' -f 2`
    lib=`ghc_pkg_field $hc $dep hs-libraries | grep -i ^hs-libraries |  sed -e 's|hs-libraries: *\([^ ]*\).*|\1|' `
    for dir in $dirs ; do
	if [ -e "${dir}/lib${lib}.a" ] ; then
	    package=`dpkg-query -S ${dir}/lib${lib}.a | cut -d':' -f 1` || exit $?
	    continue
	fi
    done
    echo $package
}

providing_package_for_ghc_prof(){
    local package
    local dep
    local dir
    local dirs
    local lib
    local hc
    hc=$1
    dep=`strip_hash $2`
    dirs=`ghc_pkg_field $hc $dep library-dirs | grep -i ^library-dirs | cut -d':' -f 2`
    lib=`ghc_pkg_field $hc $dep hs-libraries | grep -i ^hs-libraries | sed -e 's|hs-libraries: *\([^ ]*\).*|\1|' `
    for dir in $dirs ; do
	if [ -e "${dir}/lib${lib}_p.a" ] ; then
	    package=`dpkg-query -S ${dir}/lib${lib}_p.a | cut -d':' -f 1` || exit $?
	    continue
	fi
    done
    echo $package
}

cabal_package_ids(){
    local config
    local package_ids
    until [ -z "$1" ]
    do
      config=$1
      package_ids="$package_ids `grep-dctrl -n -i -s Id "" $config`"
      shift
    done
    echo $package_ids
}

cabal_depends(){
    local config
    local dep
    local depends
    local final_depends
    until [ -z "$1" ]
    do
      config=$1
      depends="$depends `grep-dctrl -n -i -s Depends "" $config | tr "," " "`"
      shift
    done
    for dep in `sort_uniq $depends` ; do
	# The package is not mentioned in the ignored package list with the same version
	# or mentioned without any version in the ignored package list?
	if  echo " $ignores " | grep -qv " $dep " &&
	    echo " $ignores " | grep -qv " `echo $dep | sed s%-[0-9][.0-9a-zA-Z]*$%%` " ;
	then
	    final_depends="$final_depends $dep"
	fi
    done
    echo $final_depends
}

hashed_dependency(){
    local hc
    local type
    local pkgid
    local virpkg
    hc=$1
    type=$2
    pkgid=$3
    virtual_pkg=`package_id_to_virtual_package "${hc}" "$type" $pkgid`
    # As a transition measure, check if dpkg knows about this virtual package
    if dpkg-query -W $virtual_pkg >/dev/null 2>/dev/null;
    then
	 echo $virtual_pkg
    fi
}

depends_for_ghc(){
    local dep
    local packages
    local pkgid
    local hc
    hc=$1
    shift
    for pkgid in `cabal_depends $@` ; do
	dep=`hashed_dependency ${hc} dev $pkgid`
	if [ -z "$dep" ]
	then
	  pkg=`providing_package_for_ghc $hc $pkgid`
          if [ -n "$pkg" ]
          then
	      dep=`dependency $pkg`
	      packages="$packages, $dep"
	  else
	      echo "WARNING: No Debian package provides haskell package $pkgid." >&2
          fi
        else
	    packages="$packages, $dep"
	fi
    done

    echo $packages | sed -e 's/^,[ ]*//'
}

depends_for_ghc_prof(){
    local dep
    local packages
    local pkgid
    local hc
    hc=$1
    shift
    for pkgid in `cabal_depends $@` ; do
	dep=`hashed_dependency ${hc} prof $pkgid`
	if [ -z "$dep" ]
	then
	  pkg=`providing_package_for_ghc_prof $hc $pkgid`
          if [ -n "$pkg" ]
          then
	      dep=`dependency $pkg`
	      packages="$packages, $dep"
	  else
	      echo "WARNING: No Debian package provides haskell package $pkgid." >&2
          fi
        else
	    packages="$packages, $dep"
	fi
    done

    echo $packages | sed -e 's/^,[ ]*//'
}

provides_for_ghc(){
    local hc
    local dep
    local packages
    hc=$1
    shift
    for package_id in `cabal_package_ids $@` ; do
	packages="$packages, `package_id_to_virtual_package "${hc}" dev $package_id`"
    done
    echo $packages | sed -e 's/^,[ ]*//'
}

provides_for_ghc_prof(){
    local hc
    local dep
    local packages
    hc=$1
    shift
    for package_id in `cabal_package_ids $@` ; do
	packages="$packages, `package_id_to_virtual_package "${hc}" prof $package_id`"
    done
    echo $packages | sed -e 's/^,[ ]*//'
}

package_id_to_virtual_package(){
	local hc
	local type
	local pkgid
	hc="$1"
	type="$2"
	pkgid="$3"
	echo ${pkgid} | tr A-Z a-z | \
            grep '[a-z0-9]\+-[0-9\.]\+-................................' | \
		perl -pe 's/([a-z0-9-]+)-([0-9\.]+)-(.....).........................../lib'${hc}'-\1-'$type'-\2-\3/'
}

depends_for_hugs(){
    local version
    local upstream_version
    version=`dpkg-query --showformat='${Version}' -W hugs`
    upstream_version=`echo $version | sed -e 's/-[^-]*$//'`
    echo "hugs (>= $upstream_version)"
}

find_config_for_ghc(){
    local f
    local pkg
    pkg=$1
    pkgdir=`package_pkgdir ${pkg}`
    case "$pkg" in
	ghc-prof)
	    pkg=ghc
	    ;;
	*-prof)
	    pkg=`echo $pkg | sed -e 's/-prof$/-dev/'`
	    ;;
	*)
	    ;;
    esac
    for f in debian/$pkg/${pkgdir}/*.conf ; do
	if [ -f "$f" ] ; then
	    echo $f
	    echo " "
	fi
    done
}

clean_recipe(){
    DEB_SETUP_BIN_NAME=$1
    CABAL_PACKAGE=$2
    MAKEFILE=$3
    DEB_LINTIAN_OVERRIDES_FILE=$4
    [ ! -x "${DEB_SETUP_BIN_NAME}" ] || ${DEB_SETUP_BIN_NAME} clean
    rm -rf dist dist-ghc dist-ghcjs dist-hugs ${DEB_SETUP_BIN_NAME} Setup.hi Setup.ho Setup.o .*config*
    rm -f configure-ghc-stamp configure-ghcjs-stamp build-ghc-stamp build-ghcjs-stamp build-hugs-stamp build-haddock-stamp
    rm -rf debian/tmp-inst-ghc debian/tmp-inst-ghcjs
    rm -f debian/extra-depends-ghc debian/extra-depends-ghcjs
    rm -f debian/libghc-${CABAL_PACKAGE}-doc.links debian/libghcjs-${CABAL_PACKAGE}-doc.links
    if [ -f ${DEB_LINTIAN_OVERRIDES_FILE} ] ; then					\
      sed -i '/binary-or-shlib-defines-rpath/ d' ${DEB_LINTIAN_OVERRIDES_FILE} ;	\
      find ${DEB_LINTIAN_OVERRIDES_FILE} -empty -delete;				\
    fi

    rm -f ${MAKEFILE}
    rm -rf debian/dh_haskell_shlibdeps
}

make_setup_recipe(){
    DEB_SETUP_BIN_NAME=$1
    for setup in Setup.lhs Setup.hs; do if test -e $setup; then ghc --make $setup -o ${DEB_SETUP_BIN_NAME}; exit 0; fi; done
}

configure_recipe(){
    DEB_SETUP_BIN_NAME=$1
    CABAL_PACKAGE=$2
    CABAL_VERSION=$3
    ENABLE_PROFILING=$4
    NO_GHCI_FLAG=$5
    DEB_SETUP_GHC6_CONFIGURE_ARGS=$6
    DEB_SETUP_GHC_CONFIGURE_ARGS=$7
    OPTIMIZATION=$8
    TESTS=$9
    DEB_DEFAULT_COMPILER=$10
    DEB_PACKAGES=$11

    hc=`packages_hc "${DEB_DEFAULT_COMPILER}" "${DEB_PACKAGES}"`

    ENABLE_PROFILING=`{ for i in ${DEB_PACKAGES}; do package_ext $i | grep prof; done; } | sort -u | sed 's/prof/--enable-library-profiling/'`
    local GHC_OPTIONS
    for i in `dpkg-buildflags --get LDFLAGS`; do GHC_OPTIONS="$GHC_OPTIONS -optl$i"; done

    ${DEB_SETUP_BIN_NAME} configure "--${hc}" -v2 --package-db=/`hc_pkgdir ${hc}` \
        --prefix=/`hc_prefix ${hc}` --libdir=/`hc_libdir ${hc}` \
	--builddir=dist-${hc} \
       --ghc-options="${GHC_OPTIONS}" \
	--haddockdir=/`hc_docdir ${hc} ${CABAL_PACKAGE}-${CABAL_VERSION}` --datasubdir=${CABAL_PACKAGE}\
	--htmldir=/`hc_htmldir ${hc} ${CABAL_PACKAGE}` ${ENABLE_PROFILING} ${NO_GHCI_FLAG} \
	${DEB_SETUP_GHC6_CONFIGURE_ARGS} ${DEB_SETUP_GHC_CONFIGURE_ARGS} ${OPTIMIZATION} ${TESTS}
}

build_recipe(){
    DEB_SETUP_BIN_NAME=$1
    DEB_DEFAULT_COMPILER=$2
    DEB_PACKAGES=$3
    hc=`packages_hc "${DEB_DEFAULT_COMPILER}" "${DEB_PACKAGES}"`
    ${DEB_SETUP_BIN_NAME} build --builddir=dist-${hc}
}

check_recipe(){
    DEB_SETUP_BIN_NAME=$1
    DEB_DEFAULT_COMPILER=$2
    DEB_PACKAGES=$3
    hc=`packages_hc "${DEB_DEFAULT_COMPILER}" "${DEB_PACKAGES}"`
    ${DEB_SETUP_BIN_NAME} test --builddir=dist-${hc} --show-details=always
}

haddock_recipe(){
    DEB_SETUP_BIN_NAME=$1
    DEB_HADDOCK_OPTS=$2
    DEB_DEFAULT_COMPILER=$3
    DEB_PACKAGES=$4
    hc=`packages_hc "${DEB_DEFAULT_COMPILER}" "${DEB_PACKAGES}"`
    haddock=`hc_haddock ${hc}`
    [ ! -x /usr/bin/${haddock} ] || ${DEB_SETUP_BIN_NAME} haddock --builddir=dist-${hc} --with-haddock=/usr/bin/${haddock} --with-ghc=${hc} ${DEB_HADDOCK_OPTS} || \
	  echo "Haddock failed (no modules?), creating empty documentation package."
}

extra_depends_recipe(){
    DEB_SETUP_BIN_NAME=$1
    hc=$2
    pkg_config=`${DEB_SETUP_BIN_NAME} register --builddir=dist-${hc} --gen-pkg-config | tr -d ' \n' | sed -r 's,^.*:,,'`
    dh_haskell_extra_depends ${hc} $pkg_config
    rm $pkg_config
}

install_dev_recipe(){
    DEB_SETUP_BIN_NAME=$1
    CABAL_PACKAGE=$2
    CABAL_VERSION=$3
    HASKELL_HIDE_PACKAGES=$4
    DEB_GHC_EXTRA_PACKAGES=$5
    DEB_LINTIAN_OVERRIDES_FILE=$6
    PKG=$7

    hc=`package_hc ${PKG}`
    libdir=`package_libdir ${PKG}`
    pkgdir=`package_pkgdir ${PKG}`

    ( cd debian/tmp-inst-${hc} ; mkdir -p ${libdir} ; find ${libdir}/ \
	\( ! -name "*_p.a" ! -name "*.p_hi" ! -type d \) \
	-exec install -Dm 644 '{}' ../${PKG}/'{}' ';' )
    pkg_config=`${DEB_SETUP_BIN_NAME} register --builddir=dist-${hc} --gen-pkg-config | tr -d ' \n' | sed -r 's,^.*:,,'`
    if [ "${HASKELL_HIDE_PACKAGES}" ]; then sed -i 's/^exposed: True$/exposed: False/' $pkg_config; fi
    install -Dm 644 $pkg_config debian/${PKG}/${pkgdir}/$pkg_config
    rm -f $pkg_config
    if [ "z${DEB_GHC_EXTRA_PACKAGES}" != "z" ] ; then
       mkdir -p debian/$(notdir $@)/usr/lib/haskell-packages/extra-packages; \
	echo '${DEB_GHC_EXTRA_PACKAGES}' > debian/${PKG}/usr/lib/haskell-packages/extra-packages/${CABAL_PACKAGE}-${CABAL_VERSION}
    fi

    grep -s binary-or-shlib-defines-rpath ${DEB_LINTIAN_OVERRIDES_FILE} \
       || echo binary-or-shlib-defines-rpath >> ${DEB_LINTIAN_OVERRIDES_FILE}
    dh_haskell_provides -p${PKG}
    dh_haskell_depends -p${PKG}
    dh_haskell_shlibdeps -p${PKG}
}

install_prof_recipe(){
    PKG=$1
    libdir=`package_libdir ${PKG}`
    ( cd debian/tmp-inst-`package_hc ${PKG}` ; mkdir -p ${libdir} ; find ${libdir}/ \
        ! \( ! -name "*_p.a" ! -name "*.p_hi" \) \
        -exec install -Dm 644 '{}' ../${PKG}/'{}' ';' )
    dh_haskell_provides -p${PKG}
    dh_haskell_depends -p${PKG}
}

install_doc_recipe(){
    CABAL_PACKAGE=$1
    CABAL_VERSION=$2
    DEB_ENABLE_HOOGLE=$3
    PKG=$4
    hc=`package_hc ${PKG}`
    pkgid=${CABAL_PACKAGE}-${CABAL_VERSION}
    docdir=`hc_docdir ${hc} ${pkgid}`
    htmldir=`hc_htmldir ${hc} ${CABAL_PACKAGE}`
    hoogle=`hc_hoogle ${hc}`
    mkdir -p debian/${PKG}/${htmldir}
    ( cd debian/tmp-inst-${hc}/ ; find ./${htmldir} \
	! -name "*.haddock" ! -type d -exec install -Dm 644 '{}' \
	../${PKG}/'{}' ';' )
    mkdir -p debian/${PKG}/${docdir}
    [ 0 = `ls debian/tmp-inst-${hc}/${docdir}/ 2>/dev/null | wc -l` ] ||
	cp -r debian/tmp-inst-${hc}/${docdir}/*.haddock \
	    debian/${PKG}/${docdir}
    if [ "${DEB_ENABLE_HOOGLE}" = "yes" ]; then
        find debian/${PKG}/${htmldir} -name "*.txt" \
            -printf "%p ${hoogle}/${PKG}.txt\n" >> debian/lib${hc}-${CABAL_PACKAGE}-doc.links
        sed -i s,^debian/lib${hc}-${CABAL_PACKAGE}-doc,, debian/lib${hc}-${CABAL_PACKAGE}-doc.links
    fi
    dh_haskell_depends -p${PKG}
}

if ! [ `which grep-dctrl` > /dev/null ] ; then
    echo "grep-dctrl is missing" >&2
    exit 1
fi

args=
ignores=
files=
until [ -z "$1" ]
do
  case "$1" in
      -X*)
          pkg=${1##-X}
	  ignores="$ignores $pkg"
	  ;;

      --exclude=*)
	  pkg=${1##--exclude=}
	  ignores="$ignores $pkg"
	  ;;

      -*)
	  args="$args $1"
	  ;;
      *)
	  if [ -f $1 ] ; then
	      files="$files $1"
	  else
	      echo "Installed package description file $1 can not be found" >&2
	      exit 1
	  fi
	  ;;
  esac
  shift
done
