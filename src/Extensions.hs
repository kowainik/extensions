{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Parse Haskell Language Extensions.
-}

module Extensions
       ( -- * Result data types
         ExtensionsResult
       , ExtensionsError (..)

         -- * Package modules
       , getPackageExtentions
       , getPackageExtentionsBySources

         -- * Single module
       , CabalAndModuleExtensions (..)
       , getModuleAndCabalExtentions
       , getModuleExtentions
       , getModuleAndCabalExtentionsBySource
       , getModuleExtentionsBySource
       ) where

import Control.Exception (catch)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Map.Merge.Strict (mapMissing, merge, zipWithMatched)
import Data.Map.Strict (Map)

import Extensions.Cabal (CabalException, parseCabalFileExtensions)
import Extensions.Parser (ParseError, parseFile, parseSourceWithPath)
import Extensions.Types (CabalAndModuleExtensions (..), Extensions (..), ParsedExtensions (..),
                         mergeAnyExtensions)

import qualified Data.Map.Strict as Map


-- | Type alias for the result of extensions analyse.
type ExtensionsResult = Either ExtensionsError Extensions

-- | Represents possible errors during the work of extensions analyser.
data ExtensionsError
    -- | Parse error during module extensions parsing.
    = ModuleParseError FilePath ParseError
    -- | Error during @.cabal@ file reading/parsing.
    | CabalError CabalException
    -- | File is in cabal file, but the source file is not provided where requested.
    | SourceNotFound FilePath
    -- | Source file is provided, but module is not in cabal file.
    | NotCabalModule FilePath
    | SafeHaskellConflict
    deriving stock (Show, Eq)

{- | By given path to @.cabal@ file, analyse extensions for each Haskell module
and return the corresponding 'Map'.

__Throws__:

* 'CabalException'
-}
getPackageExtentions
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> IO (Map FilePath ExtensionsResult)
getPackageExtentions cabalFile = do
    cabalMap <- parseCabalFileExtensions cabalFile
    Map.traverseWithKey perModuleParseMerge cabalMap
  where
    perModuleParseMerge :: FilePath -> ParsedExtensions -> IO ExtensionsResult
    perModuleParseMerge path cabalExts = do
        moduleRes <- parseFile path
        pure $ mergeCabalAndModule cabalExts path moduleRes

{- | By given path to @.cabal@ file and 'Map' of sources of all Haskell
modules, analyse extensions for each Haskell module and return the corresponding
'Map'.
-}
getPackageExtentionsBySources
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> Map FilePath ByteString  -- ^ Path to modules with corresponding sources.
    -> IO (Map FilePath ExtensionsResult)
getPackageExtentionsBySources cabalFile sourcesMap =
    parseCabalHandleException cabalFile <&> \case
        Left err -> Left err <$ sourcesMap
        Right cabalMap -> merge
            (mapMissing cabalNotSource) -- in cabal but not in sources
            (mapMissing sourceNotCabal) -- in sources but not in cabal
            (zipWithMatched cabalAndSource) -- in cabal and sources
            cabalMap
            sourcesMap
  where
    cabalNotSource :: FilePath -> ParsedExtensions -> ExtensionsResult
    cabalNotSource path _cabalExts = Left $ SourceNotFound path

    sourceNotCabal :: FilePath -> ByteString -> ExtensionsResult
    sourceNotCabal path _source = Left $ NotCabalModule path

    cabalAndSource
        :: FilePath
        -> ParsedExtensions
        -> ByteString
        -> ExtensionsResult
    cabalAndSource path cabalExts source =
        mergeCabalAndModule cabalExts path $ parseSourceWithPath path source

{- | By given path to @.cabal@ file and path to Haskell module of the
corresponding package, analyse and return extensions for the
given module separately from .cabal file and from the module itself.
-}
getModuleAndCabalExtentions
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> FilePath  -- ^ Path to Haskell module file.
    -> IO (Either ExtensionsError CabalAndModuleExtensions)
getModuleAndCabalExtentions cabalFile path =
    parseCabalHandleException cabalFile >>= \case
        Left err -> pure $ Left err
        Right cabalMap -> case Map.lookup path cabalMap of
            Nothing        -> pure $ Left $ NotCabalModule path
            Just cabalExts -> getCabalAndModuleExts path cabalExts <$> parseFile path

{- | By given path to @.cabal@ file and path to Haskell module of the
corresponding package, analyse and return summary set of extensions for the
given module.
-}
getModuleExtentions
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> FilePath  -- ^ Path to Haskell module file.
    -> IO ExtensionsResult
getModuleExtentions cabalFile path =
    parseCabalHandleException cabalFile >>= \case
        Left err -> pure $ Left err
        Right cabalMap -> case Map.lookup path cabalMap of
            Nothing -> pure $ Left $ NotCabalModule path
            Just cabalExts -> do
                moduleRes <- parseFile path
                pure $ mergeCabalAndModule cabalExts path moduleRes

{- | By given path to @.cabal@ file and path to Haskell module of the
corresponding package, analyse and return extensions extensions for the
given module separately from .cabal file and from the module itself.
-}
getModuleAndCabalExtentionsBySource
    :: FilePath  -- ^ Maybe path to @.cabal@ file.
    -> FilePath  -- ^ Path to the module's source (needed for matching with cabal file).
    -> ByteString  -- ^ Source of a Haskell module file.
    -> IO (Either ExtensionsError CabalAndModuleExtensions)
getModuleAndCabalExtentionsBySource cabalFile path source =
    parseCabalHandleException cabalFile <&> \case
        Left cabalError -> Left cabalError
        Right cabalMap -> case Map.lookup path cabalMap of
            Nothing        -> Left $ NotCabalModule path
            Just cabalExts -> getCabalAndModuleExts path cabalExts
                (parseSourceWithPath path source)

{- | By given path to @.cabal@ file and path to Haskell module of the
corresponding package, analyse and return combined set of extensions for the
given module.
-}
getModuleExtentionsBySource
    :: FilePath  -- ^ Maybe path to @.cabal@ file.
    -> FilePath  -- ^ Path to the module's source (needed for matching with cabal file).
    -> ByteString  -- ^ Source of a Haskell module file.
    -> IO ExtensionsResult
getModuleExtentionsBySource cabalFile path source =
    parseCabalHandleException cabalFile <&> \case
        Left cabalError -> Left cabalError
        Right cabalMap -> case Map.lookup path cabalMap of
            Nothing        -> Left $ NotCabalModule path
            Just cabalExts -> mergeCabalAndModule cabalExts path
                (parseSourceWithPath path source)

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

mergeCabalAndModule
    :: ParsedExtensions
    -> FilePath
    -> Either ParseError ParsedExtensions
    -> ExtensionsResult
mergeCabalAndModule cabalExts path moduleRes = case moduleRes of
    Right moduleExts -> case mergeAnyExtensions cabalExts moduleExts of
        Just res -> Right res
        Nothing  -> Left SafeHaskellConflict
    Left parseErr    -> Left $ ModuleParseError path parseErr

-- | 'parseCabalFileExtensions' with 'handleCabalException'.
parseCabalHandleException
    :: FilePath
    -> IO (Either ExtensionsError (Map FilePath ParsedExtensions))
parseCabalHandleException cabalFile = (Right <$> parseCabalFileExtensions cabalFile)
    `catch` handleCabalException

-- | Handle 'CabalException' and return corresponding 'CabalError'.
handleCabalException
    :: CabalException
    -> IO (Either ExtensionsError (Map FilePath ParsedExtensions))
handleCabalException = pure . Left . CabalError

getCabalAndModuleExts
    :: FilePath
    -> ParsedExtensions
    -> Either ParseError ParsedExtensions
    -> Either ExtensionsError CabalAndModuleExtensions
getCabalAndModuleExts path cabalExts moduleRes = case moduleRes of
    Left err -> Left $ ModuleParseError path err
    Right moduleExts -> Right $ CabalAndModuleExtensions
        { cabalExtensions  = cabalExts
        , moduleExtensions = moduleExts
        }
