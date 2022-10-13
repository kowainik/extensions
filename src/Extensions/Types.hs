{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}

{- |
Copyright: (c) 2020-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and functions to work with different types of 'Extension's.

@extensions@ library supports the following types of extensions:

 +-------------------------------------------+----------------------------+
 | @Haskell2010@ Default Enabled Extensions  | 'On' of 'OnOffExtension's  |
 +-------------------------------------------+----------------------------+
 | @Haskell2010@ Default Disabled Extensions | 'Off' of 'OnOffExtension's |
 +-------------------------------------------+----------------------------+
 | @SafeHaskell@ Extensions                  | 'SafeHaskellExtension's    |
 +-------------------------------------------+----------------------------+

-}

module Extensions.Types
    ( Extensions (..)
    , ParsedExtensions (..)
    , CabalAndModuleExtensions (..)
    , ExtensionsResult

      -- * Errors
    , ExtensionsError (..)
    , CabalException (..)
    , ModuleParseError (..)

      -- ** Defaults / empty data types
    , emptyExtensions
    , emptyParsedExtensions

      -- * Enabled/Disabled Haskell2010 Extensions
    , OnOffExtension (..)
    , showOnOffExtension
    , readOnOffExtension
    , readExtension
    , mergeExtensions
    , mergeAnyExtensions
    , default2010Extensions

      -- * Safe Haskell Extensions
    , SafeHaskellExtension (..)
    ) where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Text (Text)
import GHC.LanguageExtensions.Type (Extension (..))
import Text.Read (readMaybe)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec


{- | Main returned data type that includes merged 'OnOffExtension's and possibly
one 'SafeHaskellExtension'.
-}
data Extensions = Extensions
    { extensionsAll  :: !(Set OnOffExtension)
    , extensionsSafe :: !(Maybe SafeHaskellExtension)
    } deriving stock (Show, Eq)

{- | Extensions that are collected in the result of parsing @.cabal@ file or
Haskell module (both 'OnOffExtension' and possibly one 'SafeHaskellExtension').

'OnOffExtension's are not necessary unique. They reflect exactly the extensions
found during parsing.
-}
data ParsedExtensions = ParsedExtensions
    { parsedExtensionsAll  :: ![OnOffExtension]
    , parsedExtensionsSafe :: !(Maybe SafeHaskellExtension)
    } deriving stock (Show, Eq)

-- | Stores extensions from @.cabal@ file and module separately.
data CabalAndModuleExtensions = CabalAndModuleExtensions
    { cabalExtensions  :: !ParsedExtensions
    , moduleExtensions :: !ParsedExtensions
    } deriving stock (Show, Eq)

-- | Type alias for the result of extensions analysis.
type ExtensionsResult = Either ExtensionsError Extensions

-- | Represents possible errors during the work of extensions analyser.
data ExtensionsError
    -- | Parse error during module extensions parsing.
    = ModuleParseError FilePath ModuleParseError
    -- | Error during @.cabal@ file reading/parsing.
    | CabalError CabalException
    -- | File is in cabal file, but the source file is not provided where requested.
    | SourceNotFound FilePath
    -- | Source file is provided, but module is not in cabal file.
    | NotCabalModule FilePath
    -- | Conflicting 'SafeHaskellExtension's in one scope.
    | SafeHaskellConflict (NonEmpty SafeHaskellExtension)
    deriving stock (Show, Eq)

{- | Exception that gets thrown when working with @.cabal@ files.
-}
data CabalException
    -- | The @.cabal@ file is not found.
    = CabalFileNotFound FilePath
    -- | Parsing errors in the @.cabal@ file.
    | CabalParseError Text
    -- | Conflicting 'SafeHaskellExtension's in one scope.
    | CabalSafeExtensionsConflict (NonEmpty SafeHaskellExtension)
    deriving stock (Show, Eq)
    deriving anyclass (Exception)

-- | Error while parsing Haskell source file.
data ModuleParseError
    -- | File parsing error.
    = ParsecError Parsec.ParseError
    -- | Uknown extensions were used in the module.
    | UnknownExtensions (NonEmpty String)
    -- | Conflicting 'SafeHaskellExtension's in one scope.
    | ModuleSafeHaskellConflict (NonEmpty SafeHaskellExtension)
    -- | Module file not found.
    | FileNotFound FilePath
    deriving stock (Show, Eq)


-- | Empty 'Extensions' with no specified 'SafeHaskellExtension'.
emptyExtensions :: Extensions
emptyExtensions = Extensions
    { extensionsAll = mempty
    , extensionsSafe = Nothing
    }

-- | Empty 'ParsedExtensions' with no specified 'SafeHaskellExtension'.
emptyParsedExtensions :: ParsedExtensions
emptyParsedExtensions = ParsedExtensions
    { parsedExtensionsAll = []
    , parsedExtensionsSafe = Nothing
    }

{- | Language extensions that are used by Safe Haskell to indicate safety of the
code.

To find out more, checkout the official documentation on @SafeHaskell@:

 * [Safe Language Pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe_haskell.html#safe-haskell)
-}
data SafeHaskellExtension
    = Unsafe
    | Trustworthy
    | Safe
    deriving stock (Show, Read, Eq, Ord, Enum, Bounded)

-- | Represents enabled/disabled extensions.
data OnOffExtension
    = On Extension
    | Off Extension
    deriving stock (Show, Eq, Ord)

-- | Display 'OnOffExtension' as GHC recognizes it.
showOnOffExtension :: OnOffExtension -> Text
showOnOffExtension = \case
    On ext  -> showExtension ext
    Off ext -> "No" <> showExtension ext
  where
    showExtension :: Extension -> Text
    showExtension = \case
        Cpp        -> "CPP"
#if !MIN_VERSION_ghc_boot_th(9,4,1)
        RecordPuns -> "NamedFieldPuns"
#endif
        ext        -> Text.pack $ show ext

{- | Parse 'OnOffExtension' from a string that specifies extension.
-}
readOnOffExtension :: String -> Maybe OnOffExtension
readOnOffExtension s =
    (On <$> readExtension s) <|> (Off <$> readOffExtension)
  where
    readOffExtension :: Maybe Extension
    readOffExtension = do
        ("No", ext) <- Just $ splitAt 2 s
        readExtension ext

{- | Parse 'Extension' from a string. 'Read' instance for 'Extension'
doesn't always work since some extensions are named differently.
-}
readExtension :: String -> Maybe Extension
readExtension = \case
    "GeneralisedNewtypeDeriving" -> Just GeneralizedNewtypeDeriving
#if !MIN_VERSION_ghc_boot_th(9,4,1)
    "NamedFieldPuns"             -> Just RecordPuns
    "RecordPuns"                 -> Nothing
#endif
    "Rank2Types"                 -> Just RankNTypes
    "CPP"                        -> Just Cpp
    "Cpp"                        -> Nothing
    s                            -> readMaybe s

{- | Take accumulated 'OnOffExtension's, and merge them into one 'Set',
excluding enabling of 'default2010Extensions'.

If the default extension is enabled manually it still won't count as it doesn't
affect real behaviour. However, disabling of default extension will be included
in the list.

So, basically, this set will only have 'On' extensions that are not defaults,
and 'Off' extensions of defaults.

'foldl\'' is used in order to process them in the right order: first all cabal
extensions and then extensions from the module in the order of appearance.
-}
mergeExtensions :: [OnOffExtension] -> Set OnOffExtension
mergeExtensions = foldl' handleExt Set.empty
  where
    handleExt :: Set OnOffExtension -> OnOffExtension -> Set OnOffExtension
    handleExt exts (On e)
        | e `elem` default2010Extensions = Set.delete (Off e) exts
        | otherwise                      = Set.insert (On e) exts
    handleExt exts (Off e)
        | e `elem` default2010Extensions = Set.insert (Off e) exts
        | otherwise                      = Set.delete (On e) exts

{- | Similar to 'mergeExtensions', but also merge 'SafeHaskellExtension's.
In case of conflicting 'SafeHaskellExtension' returns 'Left' with the pair of
conflicting extension constructors under 'SafeHaskellConflict' error.
-}
mergeAnyExtensions
    :: ParsedExtensions  -- ^ Cabal parsed extensions.
    -> ParsedExtensions  -- ^ Module parsed extensions.
    -> ExtensionsResult
mergeAnyExtensions (ParsedExtensions exts1 safe1) (ParsedExtensions exts2 safe2) = case (safe1, safe2) of
    (Nothing, safe) -> Right $ Extensions
        { extensionsAll = mergedExts
        , extensionsSafe = safe
        }
    (safe, Nothing) -> Right $ Extensions
        { extensionsAll = mergedExts
        , extensionsSafe = safe
        }
    (Just s1, Just s2) ->
        if safe1 == safe2
        then Right $ Extensions
            { extensionsAll = mergedExts
            , extensionsSafe = safe1
            }
        else Left $ SafeHaskellConflict $ s1 :| [s2]
  where
    mergedExts :: Set OnOffExtension
    mergedExts = mergeExtensions (exts1 <> exts2)

-- | Default enabled extensions for @Haskell2010@
default2010Extensions :: [Extension]
default2010Extensions =
    [ ImplicitPrelude
    , StarIsType
    , MonomorphismRestriction
    , DatatypeContexts
    , TraditionalRecordSyntax
    , EmptyDataDecls
    , ForeignFunctionInterface
    , PatternGuards
    , DoAndIfThenElse
    , RelaxedPolyRec
#if __GLASGOW_HASKELL__ >= 810
    , CUSKs
#endif
    ]

deriving stock instance Read Extension
#if __GLASGOW_HASKELL__ < 900
deriving stock instance Ord Extension
#endif
