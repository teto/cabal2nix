{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cabal2nix
  ( main, cabal2nix, parseArgs
  , Options(..)
  )
  where

import Control.Lens
import Data.List
import Data.List.Split
import Data.Maybe ( fromMaybe, listToMaybe )
import qualified Distribution.Compat.ReadP as P
import Distribution.Compiler
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Haskell.FromCabal
import Distribution.Nixpkgs.Haskell.FromCabal.Flags
import Distribution.Package ( packageId )
import Distribution.PackageDescription hiding ( options )
import Distribution.PackageDescription.Parsec
import Distribution.Simple.Utils ( lowercase )
import Distribution.System
import Distribution.Text
import Distribution.Verbosity
import GHC.IO.Encoding ( setLocaleEncoding, utf8 )
import Language.Nix
import Options.Applicative
import Paths_cabal2nix ( version )
import System.Environment ( getArgs )
import Text.PrettyPrint.HughesPJClass ( prettyShow )

data Options = Options
  { optSha256 :: Maybe String
  , optFlags :: [String]
  , optCompiler :: CompilerId
  , optSystem :: Platform
  , optUrl :: FilePath
  }

options :: Parser Options
options = Options
          <$> optional (strOption $ long "sha256" <> metavar "HASH" <> help "sha256 hash of source tarball")
          <*> many (strOption $ short 'f' <> long "flag" <> help "Cabal flag (may be specified multiple times)")
          <*> option (readP parse) (long "compiler" <> help "compiler to use when evaluating the Cabal file" <> value buildCompilerId <> showDefaultWith display)
          <*> option (maybeReader parsePlatform) (long "system" <> help "host system (in either short Nix format or full LLVM style) to use when evaluating the Cabal file" <> value buildPlatform <> showDefaultWith display)
          <*> strArgument (metavar "CABAL-FILE")

readP :: P.ReadP a a -> ReadM a
readP p = eitherReader $ \s -> case [ r' | (r',"") <- P.readP_to_S p s ] of
                                    (r:_) -> Right r
                                    _     -> Left ("invalid value " ++ show s)

-- | Replicate the normalization performed by GHC_CONVERT_CPU in GHC's aclocal.m4
-- since the output of that is what Cabal parses.
ghcConvertArch :: String -> String
ghcConvertArch arch = case arch of
  "i486"  -> "i386"
  "i586"  -> "i386"
  "i686"  -> "i386"
  "amd64" -> "x86_64"
  _ -> fromMaybe arch $ listToMaybe
    [prefix | prefix <- archPrefixes, prefix `isPrefixOf` arch]
  where archPrefixes =
          [ "aarch64", "alpha", "arm", "hppa1_1", "hppa", "m68k", "mipseb"
          , "mipsel", "mips", "powerpc64le", "powerpc64", "powerpc", "s390x"
          , "sparc64", "sparc"
          ]

-- | Replicate the normalization performed by GHC_CONVERT_OS in GHC's aclocal.m4
-- since the output of that is what Cabal parses.
ghcConvertOS :: String -> String
ghcConvertOS os = case os of
  "watchos"       -> "ios"
  "tvos"          -> "ios"
  "linux-android" -> "linux-android"
  _ | "linux-" `isPrefixOf` os -> "linux"
  _ -> fromMaybe os $ listToMaybe
    [prefix | prefix <- osPrefixes, prefix `isPrefixOf` os]
  where osPrefixes =
          [ "gnu", "openbsd", "aix", "darwin", "solaris2", "freebsd", "nto-qnx"]

parseArch :: String -> Arch
parseArch = classifyArch Permissive . ghcConvertArch

parseOS :: String -> OS
parseOS = classifyOS Permissive . ghcConvertOS

parsePlatform :: String -> Maybe Platform
parsePlatform = parsePlatformParts . splitOn "-"

parsePlatformParts :: [String] -> Maybe Platform
parsePlatformParts = \case
  [arch, os]           -> Just $ Platform (parseArch arch) (parseOS os)
  (arch : _ : osParts) -> Just $ Platform (parseArch arch) $ parseOS $ intercalate "-" osParts
  _                    -> Nothing

pinfo :: ParserInfo Options
pinfo = info
        (   helper
        <*> infoOption ("cabal2nix " ++ display version) (long "version" <> help "Show version number")
        <*> options
        )
        (  fullDesc
        <> header "cabal2nix converts a Cabal file into build instructions for Nix."
        )

main :: IO ()
main = setLocaleEncoding utf8 >> getArgs >>= parseArgs >>= cabal2nix >>= putStrLn . prettyShow

parseArgs :: [String] -> IO Options
parseArgs = handleParseResult . execParserPure defaultPrefs pinfo

cabal2nix :: Options -> IO Derivation
cabal2nix opts = processPackage opts <$> readGenericPackageDescription silent (optUrl opts)

processPackage :: Options -> GenericPackageDescription -> Derivation
processPackage Options{..} gpd = deriv
  where
    flags :: FlagAssignment
    flags = configureCabalFlags (packageId gpd) `mappend` readFlagList optFlags

    deriv :: Derivation
    deriv = fromGenericPackageDescription (const True)
                (\i -> Just (binding # (i, path # [ident # "pkgs", i])))
                optSystem
                (unknownCompilerInfo optCompiler NoAbiTag)
                flags
                []
                gpd
            & sha256 .~ fromMaybe "" optSha256
            & extraFunctionArgs . contains "inherit stdenv" .~ True

-- Utils

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment . map tagWithValue
  where tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
        tagWithValue fname       = (mkFlagName (lowercase fname), True)
