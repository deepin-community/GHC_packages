Index: lens-3.0.5/tests/doctests.hs
===================================================================
--- lens-3.0.5.orig/tests/doctests.hs	2012-10-29 20:42:42.226273057 +0000
+++ lens-3.0.5/tests/doctests.hs	2012-10-29 20:43:29.318137661 +0000
@@ -10,9 +10,9 @@
 main :: IO ()
 main = getSources >>= \sources -> doctest $
     "-isrc"
-  : "-idist/build/autogen"
+  : "-idist-ghc/build/autogen"
   : "-optP-include"
-  : "-optPdist/build/autogen/cabal_macros.h"
+  : "-optPdist-ghc/build/autogen/cabal_macros.h"
   : sources

 getSources :: IO [FilePath]
