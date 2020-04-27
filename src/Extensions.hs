{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Parse Haskell Language Extensions
-}

module Extensions
       ( -- * Result data types
         ExtensionsResult
       , ExtensionsError (..)

         -- * Package modules
       , getPackageExtentions
       , getPackageExtentionsBySources

         -- * Single module
       , getModuleExtentions
       , getModuleExtentionsBySource
       ) where

import Data.ByteString (ByteString)
import Data.Map.Merge.Strict (mapMissing, merge, zipWithMatched)
import Data.Map.Strict (Map)
import Data.Set (Set)

import Extensions.Cabal (parseCabalExtensions)
import Extensions.OnOff (OnOffExtension, mergeExtensions)
import Extensions.Parser (ParseError, parseFile, parseSourceWithPath)

import qualified Data.Map.Strict as Map


-- | Type alias for the result of extensions analyse.
type ExtensionsResult = Either ExtensionsError (Set OnOffExtension)

-- | Represents possible errors during the work of extensions analyser.
data ExtensionsError
    -- | Parse error during module extensions parsing.
    = ModuleParseError FilePath ParseError
    -- | Parse error during cabal extensions parsing.
    | CabalParseError
    | FileNotFound
    -- | File is in cabal file, but the source file is not provided where requested.
    | SourceNotFound FilePath
    -- | Source file is provided, but module is not in cabal file.
    | NotCabalModule FilePath
    deriving stock (Show, Eq)

{- | By given path to @.cabal@ file, analyse extensions for each Haskell module
and return the corresponding 'HashMap'
-}
getPackageExtentions
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> IO (Map FilePath ExtensionsResult)
getPackageExtentions cabalFile = do
    cabalMap <- parseCabalExtensions cabalFile
    Map.traverseWithKey perModuleParseMerge cabalMap
  where
    perModuleParseMerge :: FilePath -> [OnOffExtension] -> IO ExtensionsResult
    perModuleParseMerge path cabalExts = do
        -- TODO: catch and return SourceNotFound
        moduleRes <- parseFile path
        pure $ mergeCabalAndModule cabalExts path moduleRes

{- | By given path to @.cabal@ file and 'Hashmap' of sources of all Haskell
modules, analyse extensions for each Haskell module and return the corresponding
'HashMap'
-}
getPackageExtentionsBySources
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> Map FilePath ByteString  -- ^ Path to modules with corresponding sources.
    -> IO (Map FilePath ExtensionsResult)
getPackageExtentionsBySources cabalFile sourcesMap = do
    cabalMap <- parseCabalExtensions cabalFile
    pure $ merge
        (mapMissing cabalNotSource) -- in cabal but not in sources
        (mapMissing sourceNotCabal) -- in sources but not in cabal
        (zipWithMatched cabalAndSource) -- in cabal and sources
        cabalMap
        sourcesMap
  where
    cabalNotSource :: FilePath -> [OnOffExtension] -> ExtensionsResult
    cabalNotSource path _cabalExts = Left $ SourceNotFound path

    sourceNotCabal :: FilePath -> ByteString -> ExtensionsResult
    sourceNotCabal path _source = Left $ NotCabalModule path

    cabalAndSource
        :: FilePath
        -> [OnOffExtension]
        -> ByteString
        -> ExtensionsResult
    cabalAndSource path cabalExts source =
        mergeCabalAndModule cabalExts path $ parseSourceWithPath path source

{- | By given path to @.cabal@ file and path to Haskell module of the
corresponding package, analyse and return extensions for the given module.
-}
getModuleExtentions
    :: FilePath  -- ^ Path to @.cabal@ file.
    -> FilePath  -- ^ Path to Haskell module file.
    -> IO ExtensionsResult
getModuleExtentions cabalFile path = do
    cabalMap <- parseCabalExtensions cabalFile
    case Map.lookup path cabalMap of
        Nothing -> pure $ Left $ NotCabalModule path
        Just cabalExts -> do
            moduleRes <- parseFile path
            pure $ mergeCabalAndModule cabalExts path moduleRes

{- | By given path to @.cabal@ file and path to Haskell module of the
corresponding package, analyse and return extensions for the given module.
-}
getModuleExtentionsBySource
    :: FilePath  -- ^ Maybe path to @.cabal@ file.
    -> FilePath  -- ^ Path to the module's source (needed for matching with cabal file).
    -> ByteString  -- ^ Source of a Haskell module file.
    -> IO ExtensionsResult
getModuleExtentionsBySource cabalFile path source = do
    cabalMap <- parseCabalExtensions cabalFile
    pure $ case Map.lookup path cabalMap of
        Nothing        -> Left $ NotCabalModule path
        Just cabalExts -> mergeCabalAndModule cabalExts path
            (parseSourceWithPath path source)

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

mergeCabalAndModule
    :: [OnOffExtension]
    -> FilePath
    -> Either ParseError [OnOffExtension]
    -> ExtensionsResult
mergeCabalAndModule cabalExts path moduleRes = case moduleRes of
    Right moduleExts -> Right $ mergeExtensions $ cabalExts <> moduleExts
    Left parseErr    -> Left $ ModuleParseError path parseErr
