Index: lens-3.0.5/src/Control/Lens/Fold.hs
===================================================================
--- lens-3.0.5.orig/src/Control/Lens/Fold.hs	2012-10-26 08:11:27.000000000 +0000
+++ lens-3.0.5/src/Control/Lens/Fold.hs	2012-10-29 20:50:22.381110350 +0000
@@ -203,6 +203,7 @@
 -- [4,5,6]
 --
 -- >>> toListOf (droppingWhile (<=3) folded) [1,6,1]
+-- [6]

 droppingWhile :: (Gettable f, Applicative f)
               => (c -> Bool)
