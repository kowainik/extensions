{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and functions to work with enabled/disabled 'Extension's.
-}

module Extensions.OnOff
    ( OnOffExtension (..)
    , mergeExtensions
    , default2010Extensions
    ) where

import Data.Foldable (foldl')
import Data.Set (Set)
import GHC.LanguageExtensions.Type (Extension (..))

import qualified Data.Set as Set


-- | Represents enabled/disabled extensions.
data OnOffExtension
    = On Extension
    | Off Extension
    deriving stock (Show, Eq, Ord)


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
