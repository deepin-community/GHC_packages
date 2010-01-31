Index: hmake-3.14/src/hmake/Config.hs
===================================================================
--- hmake-3.14.orig/src/hmake/Config.hs	2007-11-23 15:26:05.000000000 +0100
+++ hmake-3.14/src/hmake/Config.hs	2010-01-31 20:06:41.000000000 +0100
@@ -272,14 +272,14 @@
       ok <- doesFileExist ghcpkg0
       let ghcpkg = if ok then ghcpkg0 else dirname fullpath++"/ghc-pkg"
    -- pkgs <- runAndReadStdout (ghcpkg++" --list-packages")
-      pkgs <- runAndReadStdout (ghcpkg++" -l")
+      pkgs <- runAndReadStdout (ghcpkg++" list")
       let pkgsOK = filter (\p-> any (`isPrefixOf` p)
                                     ["std","base","haskell98"])
                           (deComma pkgs)
       idirs <- mapM (\p-> runAndReadStdout
-                              (ghcpkg++" --show-package="
+                              (ghcpkg++" field "
                                ++deVersion (ghcsym>=604) p
-                               ++" --field=import_dirs"))
+                               ++" import_dirs"))
                     pkgsOK
       return config{ includePaths = pkgDirs libdir (nub idirs) }
  where
