Index: hmake-3.14/src/hmake/MkConfig.hs
===================================================================
--- hmake-3.14.orig/src/hmake/MkConfig.hs	2006-07-07 18:01:16.000000000 +0200
+++ hmake-3.14/src/hmake/MkConfig.hs	2010-01-31 20:06:55.000000000 +0100
@@ -258,14 +258,14 @@
           ok <- doesFileExist ghcpkg0
           let ghcpkg = if ok then ghcpkg0 else dirname fullpath++"/ghc-pkg"
        -- pkgs <- runAndReadStdout (ghcpkg++" --list-packages")
-          pkgs <- runAndReadStdout (ghcpkg++" -l")
+          pkgs <- runAndReadStdout (ghcpkg++" list")
           let pkgsOK = filter (\p-> any (`isPrefixOf` p)
                                         ["std","base","haskell98"])
                               (deComma pkgs)
           idirs <- mapM (\p-> runAndReadStdout
-                                  (ghcpkg++" --show-package="
+                                  (ghcpkg++" field "
                                    ++deVersion (ghcsym>=604) p
-                                   ++" --field=import_dirs"))
+                                   ++" import_dirs"))
                         pkgsOK
           return config{ includePaths = pkgDirs libdir (nub idirs) }
         else do ioError (userError ("Can't find ghc includes at "++incdir1))
