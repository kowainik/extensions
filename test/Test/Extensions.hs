module Test.Extensions
    ( getExtensionsSpec
    ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Extensions (ExtensionsResult, getPackageExtentions)
import Extensions.OnOff (OnOffExtension (..))
import Test.Extensions.Cabal (defaultExtensions)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


getExtensionsSpec :: Spec
getExtensionsSpec = describe "Get Extensions" $
    it "should get package extensions" $
        getPackageExtentions "extensions.cabal" >>= \res -> res `shouldBe` resultMap
  where
    resultMap :: Map FilePath ExtensionsResult
    resultMap = Map.fromList
        [ "src/Extensions.hs"              `to` Right exts
        , "src/Extensions/Cabal.hs"        `to` Right (Set.insert (On Cpp) exts)
        , "src/Extensions/OnOff.hs"        `to` Right exts
        , "src/Extensions/Parser.hs"       `to` Right exts
        , "test/Test/Extensions.hs"        `to` Right exts
        , "test/Test/Extensions/Cabal.hs"  `to` Right exts
        , "test/Test/Extensions/OnOff.hs"  `to` Right exts
        , "test/Test/Extensions/Parser.hs" `to` Right exts
        , "test/Spec.hs"                   `to` Right exts
        ]
      where
        to = (,)

        exts :: Set OnOffExtension
        exts = Set.fromList defaultExtensions
