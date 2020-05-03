module Test.Extensions.OnOff
    ( onOffSpec
    ) where

import GHC.LanguageExtensions.Type (Extension (..))
import Hedgehog (Gen, PropertyT, forAll, tripping)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog (hedgehog)

import Extensions.Types (OnOffExtension (..), mergeExtensions, readOnOffExtension,
                         showOnOffExtension)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen


onOffSpec :: Spec
onOffSpec = describe "OnOffExtension tests" $ do
    mergeSpec
    propertiesSpec

mergeSpec :: Spec
mergeSpec = describe "Merging on-off extensions" $ do
    it "should enable non default ext" $
        checkMerge [On LambdaCase] [On LambdaCase]
    it "should not add default ext" $
        checkMerge [On ImplicitPrelude] []
    it "should disable default ext" $
        checkMerge [Off ImplicitPrelude] [Off ImplicitPrelude]
    it "should not affect disabling non default ext" $
        checkMerge [Off LambdaCase] []
    it "should not affect enable+disable non default ext" $
        checkMerge [On LambdaCase, Off LambdaCase] []
    it "should enable when disable+enable non default ext" $
        checkMerge [Off LambdaCase, On LambdaCase] [On LambdaCase]
    it "should not affect disable+enable non default ext" $
        checkMerge [Off ImplicitPrelude, On ImplicitPrelude] []
    it "should enable when disable+enable non default ext" $
        checkMerge [On ImplicitPrelude, Off ImplicitPrelude] [Off ImplicitPrelude]
  where
    checkMerge :: [OnOffExtension] -> [OnOffExtension] -> Expectation
    checkMerge exts res = mergeExtensions exts `shouldBe` Set.fromList res

propertiesSpec :: Spec
propertiesSpec = describe "Property tests" $
    it "read . show ≡ Just" roundtripProperty

roundtripProperty :: PropertyT IO ()
roundtripProperty = hedgehog $ do
    onOffExtension <- forAll genOnOffExtension
    tripping
        onOffExtension
        showOnOffExtension
        (readOnOffExtension . Text.unpack)

genOnOffExtension :: Gen OnOffExtension
genOnOffExtension = Gen.choice
    [ On  <$> genExtension
    , Off <$> genExtension
    ]
  where
    genExtension :: Gen Extension
    genExtension = Gen.enumBounded
