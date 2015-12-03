Index: haskell-lens/tests/doctests.hsc
===================================================================
--- haskell-lens.orig/tests/doctests.hsc	2015-08-15 14:18:10.501591463 +0200
+++ haskell-lens/tests/doctests.hsc	2015-08-15 14:18:10.501591463 +0200
@@ -56,9 +56,9 @@
 main :: IO ()
 main = withUnicode $ getSources >>= \sources -> doctest $
     "-isrc"
-  : "-idist/build/autogen"
+  : "-idist-ghc/build/autogen"
   : "-optP-include"
-  : "-optPdist/build/autogen/cabal_macros.h"
+  : "-optPdist-ghc/build/autogen/cabal_macros.h"
   : "-hide-all-packages"
 #ifdef TRUSTWORTHY
   : "-DTRUSTWORTHY=1"
