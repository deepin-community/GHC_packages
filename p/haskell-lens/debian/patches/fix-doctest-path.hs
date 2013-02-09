--- a/tests/doctests.hsc
+++ b/tests/doctests.hsc
@@ -52,9 +52,9 @@
 main :: IO ()
 main = withUnicode $ getSources >>= \sources -> doctest $
     "-isrc"
-  : "-idist/build/autogen"
+  : "-idist-ghc/build/autogen"
   : "-optP-include"
-  : "-optPdist/build/autogen/cabal_macros.h"
+  : "-optPdist-ghc/build/autogen/cabal_macros.h"
   : "-hide-all-packages"
   : map ("-package="++) deps ++ sources

