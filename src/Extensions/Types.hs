{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Copyright: (c) 2020 Kowainik
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
import Data.Foldable (foldl')
import Data.Set (Set)
import Data.Text (Text)
import GHC.LanguageExtensions.Type (Extension (..))
import Text.Read (readMaybe)

import qualified Data.Set as Set
import qualified Data.Text as Text


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
    deriving stock (Show, Read, Eq)

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
        RecordPuns -> "NamedFieldPuns"
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
    "NamedFieldPuns" -> Just RecordPuns
    "RecordPuns" -> Nothing
    "Rank2Types" -> Just RankNTypes
    "CPP" -> Just Cpp
    "Cpp" -> Nothing
    s -> readMaybe s

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
In case of conflicting 'SafeHaskellExtension' returns 'Left' with the pair od
conflicting extension constructors.
-}
mergeAnyExtensions
    :: ParsedExtensions  -- ^ Cabal parsed extensions.
    -> ParsedExtensions  -- ^ Module parsed extensions.
    -> Either (SafeHaskellExtension, SafeHaskellExtension) Extensions
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
        else Left (s1, s2)
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
    ]

deriving stock instance Read Extension
deriving stock instance Ord  Extension
