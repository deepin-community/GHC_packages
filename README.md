The Debian Haskell Group packages repository
============================================


Rationale
---------

As always, the Debian Haskell Group (DHG) does a few things different than
others. Most of our packages are trivial to maintain, so an individual
repository for the package would be overkill. On the other hand, it is
precisely the orchestration of hundreds of packages that are the distinguishing
feature of our work, hence the choice to put all our packaging into one
repository.


Also, our work is maintaining the debian/ directory, so we decided to track
only that, and _not_ upstream sources. Therefore, the directory structure for a
Haskell package foo is as follows:
    /p/                           -- root of all packaging
    /p/haskell-foo/               -- package name (Debian source package name)
    /p/haskell-foo/debian/        -- packaging directory
    /p/haskell-foo/debain/control -- the usual files
    ...

In particular, /p/haskell-foo/ contains no files besides debian/. So what is
the point of having the files under the debian/ subdirectory? This way, you can
extract the upstream sources in there and run your usual
uscan/debchange/quilt/sbuild/debrelease-commands as usual. The `.gitignore`
file is set up so that git will not bother you about the extracted upstream
sources.

As running uscan or dpkg-buildpackage in that directory will dump files into
the parent, we introduced the /p/ directory. Again, a `.gitignore` file is set
up so that git will not bother you about the tarballs, `deb`-files or
`changes`-files therein.

Tagging convention and workflow
-------------------------------

Packages are in one of three states:

 * `UNRELEASED` in `debian/changelog`:

   Changes (compared to the version in the archive) present, package not ready for upload.


 * `unstable` in `debian/changelog`, no corresponding git tag:

   Changes (compared to the version in the archive) present, package ready for upload.


 * `unstable` in `debian/changelog`, corresponding tag present

   Repository matches state in the archive, nothing to do.


Because git cannot tag individual directory, the tagging convention includes
the Debian source package name:

    <source package name>_v<full debian version>

In the full debian version, `:` and `~` are replaced by `_`.


Scripts in this repository
--------------------------

 * `./what-to-build.pl`.

   This script expects to be run with any number of directories as arguments,
   and defaults to `p/*/`. It prints the name of those directories that contain a
   package that is to be released (i.e. marked for upload, but not yet tagged).


Useful general tools
--------------------

 * `origtargz`:

   In order to download and extract upstream sources in one go, simply run
   `origtargz` inside the `haskell-foo/` directory. You can also run this command
   after you increased the version number in `debian/changelog`, and it will
   clean out `haskell-foo` before.

 * `git clean -d -x -n`:

   Save space by removing all extracted upstream files and other untracked
   files in `/p/`. Replace `-n` with `-f` after checking that everything is fine
