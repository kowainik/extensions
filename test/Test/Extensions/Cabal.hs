module Test.Extensions.Cabal
    ( cabalSpec
    , defaultExtensions
    ) where

import Data.Map.Strict (Map)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

import Extensions.Cabal (CabalException (..), parseCabalExtensions, parseCabalFileExtensions)
import Extensions.OnOff (OnOffExtension (..))

import qualified Data.Map.Strict as Map


cabalSpec :: Spec
cabalSpec = describe "Cabal file Extensions Parser" $ do
    it "should throw 'CabalFileNotFound' on non-existing file" $
        parseCabalFileExtensions "xno-extensions.cabal" `shouldThrow`
            (== CabalFileNotFound "xno-extensions.cabal")
    it "should throw parse error" $
        parseCabalExtensions "stack.yaml" `shouldThrow`
            (== CabalParseError)
    it "should parse project Cabal file" $
        parseCabalFileExtensions "extensions.cabal" >>= \extMap ->
            extMap `shouldBe` expectedMap
  where
    expectedMap :: Map FilePath [OnOffExtension]
    expectedMap = Map.fromList
        [ "app/Cli.hs"                     `to` defaultExtensions
        , "app/Main.hs"                    `to` defaultExtensions
        , "src/Extensions.hs"              `to` defaultExtensions
        , "src/Extensions/Cabal.hs"        `to` defaultExtensions
        , "src/Extensions/OnOff.hs"        `to` defaultExtensions
        , "src/Extensions/Parser.hs"       `to` defaultExtensions
        , "test/Test/Extensions.hs"        `to` defaultExtensions
        , "test/Test/Extensions/Cabal.hs"  `to` defaultExtensions
        , "test/Test/Extensions/OnOff.hs"  `to` defaultExtensions
        , "test/Test/Extensions/Parser.hs" `to` defaultExtensions
        , "test/Spec.hs"                   `to` defaultExtensions
        ]
      where
        to = (,)

defaultExtensions :: [OnOffExtension]
defaultExtensions = map On
    [ ConstraintKinds
    , DeriveGeneric
    , DerivingStrategies
    , GeneralizedNewtypeDeriving
    , InstanceSigs
    , KindSignatures
    , LambdaCase
    , OverloadedStrings
    , RecordWildCards
    , ScopedTypeVariables
    , StandaloneDeriving
    , TupleSections
    , TypeApplications
    , ViewPatterns
    ]
