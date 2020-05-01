{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and functions to work with enabled/disabled 'Extension's.
-}

module Extensions.OnOff
    ( OnOffExtension (..)
    , showOnOffExtension
    , readOnOffExtension
    , readExtension
    , mergeExtensions
    , default2010Extensions
    ) where

import Control.Applicative ((<|>))
import Data.Foldable (foldl')
import Data.Set (Set)
import Data.Text (Text)
import GHC.LanguageExtensions.Type (Extension (..))
import Text.Read (readMaybe)

import qualified Data.Set as Set
import qualified Data.Text as Text


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

{- | Parse 'OnOffextension' from a string that specifies extension.
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
