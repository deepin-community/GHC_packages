Description: Include wrapper.c in build
Author: Joachim Breitner <nomeata@debian.org>
Forwarded: felipe.lessa@gmail.com

Index: haskell-hipmunk-5.2.0.4/Hipmunk.cabal
===================================================================
--- haskell-hipmunk-5.2.0.4.orig/Hipmunk.cabal	2011-10-26 17:14:49.000000000 +0200
+++ haskell-hipmunk-5.2.0.4/Hipmunk.cabal	2011-10-26 17:15:38.000000000 +0200
@@ -177,8 +177,10 @@
         Chipmunk-5.3.5/src/cpSpaceHash.c,
         Chipmunk-5.3.5/src/cpSpaceQuery.c,
         Chipmunk-5.3.5/src/cpSpaceStep.c,
-        Chipmunk-5.3.5/src/cpVect.c,
-        Physics/Hipmunk/wrapper.c
+        Chipmunk-5.3.5/src/cpVect.c
+
+  C-Sources:
+    Physics/Hipmunk/wrapper.c

   if flag(small_base)
     Build-Depends: base >= 3 && < 5,
