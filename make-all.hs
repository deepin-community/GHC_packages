import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative hiding (many)
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad
import System.Directory.Extra

import Development.Shake
import Development.Shake.FilePath

import Debian.Relation.Common
import Debian.Control.String
import Debian.Control.Policy

import Text.Parsec
import Text.Parsec.String

-- dpkg-parsechangelog is slow, so here is a quick hack
-- TODO: Ensure this is not run unnecessarily often
versionOfSource :: String -> Action String
versionOfSource s = do
    let f = "p" </> s </> "debian" </> "changelog"
    need [f]
    ret <- liftIO $ parseFromFile p f
    case ret of
        Left e -> fail (show e)
        Right s -> return s
  where
    p = do
        many $ noneOf "("
        char '('
        v <- many1 $ noneOf ")"
        char ')'
        return (removeEpoch v)

ensureVersion :: String -> String -> Action ()
ensureVersion s v = do
    v' <- versionOfSource s
    when (v /= v') $ do
        fail $ "Cannot build " ++ s ++ " version " ++ v ++ ", as we have " ++ v' ++ "."

removeEpoch :: String -> String
removeEpoch s | ':' `elem` s = tail $ dropWhile (/= ':') s
              | otherwise    = s


changesFileName s v = s ++ "_" ++ v ++ "_amd64.changes"
sourceFileName s v = s ++ "_" ++ v ++ ".dsc"

binaryPackagesOfSource :: String -> Action [String]
binaryPackagesOfSource s = do
    let controlFile = "p" </> s </> "debian" </> "control"
    need [controlFile]
    ret <- liftIO $ parseDebianControlFromFile controlFile
    case ret of
        Left e -> fail (show e)
        Right dc -> return $ map unBinPkgName $ debianBinaryPackageNames dc

-- TODO: Include Build-Depends-Indep
dependsOfDsc :: FilePath -> IO [String]
dependsOfDsc f = do
    ret <- parseControlFromFile f
    case ret of
        Left e -> fail (show e)
        Right (Control (p:_)) -> do
            case fieldValue "Build-Depends" (p:: Paragraph) of
                Nothing -> fail "no Build-Depends"
                Just depV -> return $ nub $ parseFlatRel depV


-- Parsing package relations with flattening
-- (this could be faster than flatRels . parseRels)
parseFlatRel :: String -> [String]
parseFlatRel = flatRels . parseRels
  where
    flatRels :: Relations -> [String]
    flatRels = map (\(Rel (BinPkgName n) _ _) -> n) . join

    parseRels :: String -> Relations
    parseRels s = case parseRelations s of
      Left pe ->  error $ "Failed to parse relations " ++ (show pe)
      Right rel -> rel


fixupScript :: [String] -> String
fixupScript [] = "#!/bin/bash"
fixupScript pkgs = unlines
    [ "#!/bin/bash"
    , "for f in /var/lib/apt/lists/*_Packages"
    , "do"
    , "grep-dctrl -v -F Package -X " ++ disj ++ " < \"$f\" > \"$f\".tmp"
    , "mv \"$f\".tmp \"$f\""
    , "done"
    ]
  where disj = intercalate " -o " pkgs

excludedSources = words "ghc haskell-devscripts uuagc haskell98-report"

main = shakeArgsWith shakeOptions [] $ \_ targets -> return $ Just $ do
    if null targets then want ["all"] else want targets

    "lab/cache/sources.txt" %> \out -> do
        sources <- getDirectoryDirs "p"
        let sources' = filter (`notElem` excludedSources) sources
        writeFileChanged out (unlines sources')

    "lab/cache/all-binaries.txt" %> \out -> do
        sources <- readFileLines $ "lab/cache/sources.txt"
        binaries <- concat <$> mapM readFileLines ["lab/cache/binaries/" ++ s ++ ".txt" | s <- sources]
        writeFileChanged out (unlines binaries)

    "lab/cache/built-by.txt" %> \out -> do
        sources <- readFileLines $ "lab/cache/sources.txt"
        builtBy <- liftM (sort . concat) $ forM sources $ \s -> do
            pkgs <- readFileLines $ "lab/cache/binaries/" ++ s ++ ".txt"
            return [(pkg,s) | pkg <- pkgs]
        writeFileChanged out $ unlines [ unwords [pkg,s] | (pkg,s) <- builtBy ]

    builtBy' <- newCache $ \() -> do
        builtBy <- readFileLines $ "lab/cache/built-by.txt"
        let map = M.fromList [ (p,s) | [p,s] <- words <$> builtBy ]
        return $ \b -> M.lookup b map
    let builtBy x = ($x) <$> builtBy' ()

    "lab/cache/all-logs.txt" %> \out -> do
        sources <- readFileLines $ "lab/cache/sources.txt"
        versioned <- forM sources $ \s -> do
            v <- versionOfSource s
            return (s,v)
        writeFileChanged out $ unlines $ map (uncurry changesFileName) versioned

    "lab/cache/binaries/*.txt" %> \out -> do
        let s = dropExtension $ takeFileName $ out
        pkgs <- binaryPackagesOfSource s
        writeFileChanged out (unlines pkgs)

    "all" ~> do
        logs <- readFileLines "lab/cache/all-logs.txt"
        need [ "lab" </>  l | l <- logs]

    -- Binary packages depend on the corresponding build log
    "lab/*.deb" %> \out -> do
        let filename = takeFileName out
        let [pkgname,version,_] = splitOn "_" filename
        sourceMB <- builtBy pkgname
        case sourceMB of
            Nothing -> fail $ "Binary " ++ show pkgname ++ " not built by us."
            Just source -> need ["lab" </> changesFileName source version]

    -- Build log depends on the corresponding source, and the dependencies
    "lab/*.changes" %> \out -> do
        let filename = takeFileName out
        let [source,version,_] = splitOn "_" filename
        ensureVersion source version
        let dsc = sourceFileName source version
        need ["lab" </> dsc]
        deps <- liftIO $ dependsOfDsc $ "lab" </> dsc
        -- TODO: avoid multiple calls to builtBy
        usedDeps <- filterM (\f -> isJust <$> builtBy f) deps
        depSources <- catMaybes <$> mapM builtBy deps
        depChanges <- forM depSources $ \s -> do
            v <- versionOfSource s
            return $ "lab" </> changesFileName s v
        need depChanges

        -- Actual package building

        -- Monkey patch dependencies out of the package lists
        withTempDir $ \tmpdir -> do
            let fixup = tmpdir </> "fixup.sh"
            liftIO $ writeFile fixup  $ fixupScript usedDeps
            unit $ cmd "chmod" "+x" fixup
            debs <- filter ((==".deb").takeExtension) <$> liftIO (listFiles "lab")
            unit $ cmd (Cwd "lab") (EchoStdout False) "sbuild" "-c" "haskell" "-A" "-j4" "--no-apt-update" "--dist" "unstable" ("--chroot-setup-commands="++fixup) dsc ["--extra-package=../"++d | d <- debs]


    -- Build log depends on the corresponding source, and the dependencies
    "lab/*.dsc" %> \out -> do
        let filename = dropExtension $ takeFileName out
        let [source,version] = splitOn "_" filename
        ensureVersion source version
        sourceFiles <- getDirectoryFiles ("p" </> source) ["debian//*"]
        need [ "p" </> source </> f | f <- sourceFiles]
        unit $ cmd (Cwd "lab") (EchoStdout False) "../debian2dsc.sh" (".." </> "p" </> source </> "debian")


