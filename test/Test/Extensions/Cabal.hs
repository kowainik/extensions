module Test.Extensions.Cabal
    ( cabalSpec
    ) where

import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Extensions.Cabal (parseCabalExtensions)
import Extensions.OnOff (OnOffExtension (..))

import qualified Data.HashMap.Strict as HM


cabalSpec :: Spec
cabalSpec = describe "Cabal file Extensions Parser" $ do
    extensionsMap <- runIO $ parseCabalExtensions "extensions.cabal"

    it "should parse project Cabal file" $
        extensionsMap `shouldBe` expectedMap
  where
    expectedMap :: HM.HashMap FilePath [OnOffExtension]
    expectedMap = HM.fromList
        [ "src/Extensions.hs"              `to` defaultExtensions
        , "src/Extensions/Cabal.hs"        `to` defaultExtensions
        , "src/Extensions/OnOff.hs"        `to` defaultExtensions
        , "src/Extensions/Parser.hs"       `to` defaultExtensions
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
