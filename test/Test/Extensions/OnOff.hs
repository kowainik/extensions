module Test.Extensions.OnOff
    ( onOffSpec
    ) where

import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Extensions.OnOff (OnOffExtension (..), mergeExtensions)

import qualified Data.Set as Set


onOffSpec :: Spec
onOffSpec = describe "OnOffExtension tests" $ do
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
