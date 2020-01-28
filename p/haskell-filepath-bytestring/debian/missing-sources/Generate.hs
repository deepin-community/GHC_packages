{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Generate(main) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.IO

equivtests :: [(Int, String)]
equivtests =
        [ (0, "isPathSeparator")
        , (0, "isSearchPathSeparator")
        , (0, "isExtSeparator")
        , (1, "splitExtension")
        , (1, "takeExtension")
        , (2, "replaceExtension")
        , (1, "dropExtension")
        , (2, "addExtension")
        , (1, "hasExtension")
        , (1, "splitExtensions")
        , (1, "dropExtensions")
        , (1, "takeExtensions")
        , (2, "replaceExtensions")
        , (2, "isExtensionOf")
        , (2, "stripExtension")
        , (1, "splitFileName")
        , (1, "takeFileName")
        , (2, "replaceFileName")
        , (1, "dropFileName")
        , (1, "takeBaseName")
        , (2, "replaceBaseName")
        , (1, "takeDirectory")
        , (2, "replaceDirectory")
        , (2, "combine")
        , (1, "splitPath")
        , (3, "joinPath")
        , (1, "splitDirectories")
        , (1, "splitDrive")
        , (2, "joinDrive")
        , (1, "takeDrive")
        , (1, "hasDrive")
        , (1, "dropDrive")
        , (1, "isDrive")
        , (1, "hasTrailingPathSeparator")
        , (1, "addTrailingPathSeparator")
        , (1, "dropTrailingPathSeparator")
        ]

main :: IO ()
main = do
    src <- readFile "System/FilePath/Internal.hs"
    let tests = map renderTest $ concatMap parseTest $ lines src
    writeFileBinaryChanged "tests/TestGen.hs" $ unlines $
        ["-- GENERATED CODE: See ../Generate.hs"
        ,"{-# LANGUAGE OverloadedStrings #-}"
        ,"module TestGen(tests) where"
        ,"import TestUtil"
        ,"import Data.Char"
        ,"import qualified System.FilePath.Windows.ByteString as W"
        ,"import qualified System.FilePath.Posix.ByteString as P"
        ,"import Data.ByteString (isPrefixOf, isSuffixOf, null, any)"
        ,"import Prelude hiding (isPrefixOf, isSuffixOf, null, any)"
        --,"{-# ANN module \"HLint: ignore\" #-}"
        ,"tests :: [(String, Property)]"
        ,"tests ="] ++
        ["    " ++ c ++ "(" ++ show t1 ++ ", " ++ t2 ++ ")" | (c,(t1,t2)) <- zip ("[":repeat ",") tests] ++
        ["    ]"]

    let mkequivs porw l = ["    " ++ c ++ "(" ++ show ("equiv " ++ porw ++ "." ++ t2) ++ ", equiv_" ++ show t1 ++ " Our" ++ porw ++ "." ++ t2 ++ " Their" ++ porw ++ "." ++ t2 ++ ")" | (c,(t1,t2)) <- l]
    writeFileBinaryChanged "tests/TestEquiv.hs" $ unlines $
        ["-- GENERATED CODE: See ../Generate.hs"
        ,"module TestEquiv(equivtests) where"
        ,"import TestUtil"
        ,"import System.FilePath.ByteString (RawFilePath)"
        ,"import qualified System.FilePath.Windows.ByteString as OurWindows"
        ,"import qualified System.FilePath.Windows as TheirWindows"
        ,"import qualified System.FilePath.Posix.ByteString as OurPosix"
        ,"import qualified System.FilePath.Posix as TheirPosix"
        ,"equivtests :: [(String, Property)]"
        ,"equivtests ="] ++
        mkequivs "Posix" (zip ("[":repeat ",") equivtests) ++
        mkequivs "Windows" (zip (repeat ",") equivtests) ++
        ["    ]"]

data PW = P | W deriving Show -- Posix or Windows
data Test = Test
    {testPlatform :: PW
    ,testVars :: [(String,String)]   -- generator constructor, variable
    ,testBody :: [String]
    }


parseTest :: String -> [Test]
parseTest (stripPrefix "-- > " -> Just x) = platform $ toLexemes x
    where
        platform ("untested":x) = []
        platform ("Windows":":":x) = [valid W x]
        platform ("Posix"  :":":x) = [valid P x]
        platform x                 = [valid P x, valid W x]

        valid p ("Valid":x) = free p a $ drop 1 b
            where (a,b) = break (== "=>") x
        valid p x = free p [] x

        free p val x = Test p [(ctor v, v) | v <- vars] x
            where vars = nub $ sort [v | v@[c] <- x, isAlpha c]
                  ctor v | v < "x" = ""
                         | v `elem` val = "QFilePathValid" ++ show p
                         | otherwise = "QFilePath"
parseTest _ = []


toLexemes :: String -> [String]
toLexemes x = case lex x of
    [("","")] -> []
    [(x,y)] -> x : toLexemes y
    y -> error $ "Generate.toLexemes, " ++ show x ++ " -> " ++ show y


fromLexemes :: [String] -> String
fromLexemes = unwords . f
    where
        f ("`":x:"`":xs) = ("`" ++ x ++ "`") : f xs
        f (x:y:xs) | x `elem` ["[","("] || y `elem` [",",")","]"] = f $ (x ++ y) : xs
        f (x:xs) = x : f xs
        f [] = []


renderTest :: Test -> (String, String)
renderTest Test{..} = (body, code)
    where
        code = "property $ " ++ if null testVars then body else "\\" ++ unwords vars ++ " -> " ++ convvars body
        vars = [if null ctor then v else "(" ++ ctor ++ " " ++ ('v':v) ++ ")" | (ctor,v) <- testVars]

        convvars = convvars' $ map snd $ filter (not . null . fst) testVars
        convvars' [] b = b
        convvars' (v:vs) b = convvars' vs $
                "let " ++ v ++ " = toRawFilePath v" ++ v ++ " in " ++ b

        body = fromLexemes $ map (qualify testPlatform) testBody


qualify :: PW -> String -> String
qualify pw str
    | str `elem` fpops || (all isAlpha str && length str > 1 && str `notElem` prelude) = show pw ++ "." ++ str
    | otherwise = str
    where
        prelude = ["elem","uncurry","snd","fst","not","null","if","then","else"
                  ,"True","False","Just","Nothing","fromJust","concat","isPrefixOf","isSuffixOf","any"
                  ,"fromIntegral","ord","mempty","mconcat"]
        fpops = ["</>","<.>","-<.>"]


---------------------------------------------------------------------
-- UTILITIES

writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary file x = withBinaryFile file WriteMode $ \h -> hPutStr h x

readFileBinary' :: FilePath -> IO String
readFileBinary' file = withBinaryFile file ReadMode $ \h -> do
    s <- hGetContents h
    evaluate $ length s
    return s

writeFileBinaryChanged :: FilePath -> String -> IO ()
writeFileBinaryChanged file x = do
    b <- doesFileExist file
    old <- if b then fmap Just $ readFileBinary' file else return Nothing
    when (Just x /= old) $
        writeFileBinary file x
