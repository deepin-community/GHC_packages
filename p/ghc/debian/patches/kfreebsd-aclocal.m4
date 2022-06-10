Description: Add kfreebsdgnu to GHC_CONVERT_OS in aclocal.m4
Author: Svante Signell <svante.signell@gmail.com>
Bug-Debian: https://bugs.debian.org/913140

Index: b/m4/fptools.m4
===================================================================
--- a/m4/fptools.m4
+++ b/m4/fptools.m4
@@ -2144,7 +2144,7 @@ AC_DEFUN([GHC_CONVERT_OS],[
         $3="openbsd"
         ;;
       # As far as I'm aware, none of these have relevant variants
-      freebsd|dragonfly|hpux|linuxaout|kfreebsdgnu|freebsd2|mingw32|darwin|nextstep2|nextstep3|sunos4|ultrix|haiku)
+      freebsd|dragonfly|hpux|linuxaout|freebsd2|mingw32|darwin|nextstep2|nextstep3|sunos4|ultrix|haiku)
         $3="$1"
         ;;
       msys)
@@ -2164,6 +2164,9 @@ AC_DEFUN([GHC_CONVERT_OS],[
                 #      i686-gentoo-freebsd8.2
         $3="freebsd"
         ;;
+      kfreebsd*)
+        $3="kfreebsdgnu"
+        ;;
       nto-qnx*)
         $3="nto-qnx"
         ;;
