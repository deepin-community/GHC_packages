Description: Add kfreebsdgnu to GHC_CONVERT_OS in aclocal.m4
Author: Svante Signell <svante.signell@gmail.com>
Bug-Debian: https://bugs.debian.org/913140

Index: ghc-9.2.5/m4/ghc_convert_os.m4
===================================================================
--- ghc-9.2.5.orig/m4/ghc_convert_os.m4
+++ ghc-9.2.5/m4/ghc_convert_os.m4
@@ -23,7 +23,7 @@
         $3="openbsd"
         ;;
       # As far as I'm aware, none of these have relevant variants
-      freebsd|dragonfly|hpux|linuxaout|kfreebsdgnu|freebsd2|mingw32|darwin|nextstep2|nextstep3|sunos4|ultrix|haiku)
+      freebsd|dragonfly|hpux|linuxaout|freebsd2|mingw32|darwin|nextstep2|nextstep3|sunos4|ultrix|haiku)
         $3="$1"
         ;;
       msys)
@@ -43,6 +43,9 @@
                 #      i686-gentoo-freebsd8.2
         $3="freebsd"
         ;;
+      kfreebsd*)
+        $3="kfreebsdgnu"
+        ;;
       nto-qnx*)
         $3="nto-qnx"
         ;;
