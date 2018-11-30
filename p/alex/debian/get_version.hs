
module Main (main) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec ( readGenericPackageDescription )
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version

main :: IO ()
main = do Right fp <- findPackageDesc "."
          pd <- readGenericPackageDescription normal fp
          putStr $ showVersion $ pkgVersion $ package $ packageDescription pd
