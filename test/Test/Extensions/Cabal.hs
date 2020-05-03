module Test.Extensions.Cabal
    ( cabalSpec
    , defaultExtensions
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

import Extensions.Cabal (CabalException (..), parseCabalExtensions, parseCabalFileExtensions)
import Extensions.Types (OnOffExtension (..), ParsedExtensions (..), emptyParsedExtensions)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


cabalSpec :: Spec
cabalSpec = describe "Cabal file Extensions Parser" $ do
    it "should throw 'CabalFileNotFound' on non-existing file" $
        parseCabalFileExtensions "xno-extensions.cabal" `shouldThrow`
            (== CabalFileNotFound "xno-extensions.cabal")
    it "should throw parse error" $
        parseCabalExtensions exampleCabal "stack.yaml" `shouldThrow`
            (== CabalParseError "example.cabal:0:0: \"name\" field missing")
    it "should parse minimal Cabal file" $
        parseCabalExtensions exampleCabal minimalCabal >>= \extMap ->
            extMap `shouldBe` mempty
    it "should parse Cabal file with a single module in a library" $
        parseCabalExtensions exampleCabal singleModuleCabal >>= \extMap ->
            extMap `shouldBe` singleModuleMap
    it "should parse Cabal file with multiple directories" $
        parseCabalExtensions exampleCabal multipleDirsCabal >>= \extMap ->
            extMap `shouldBe` singleModuleMap
    it "should parse Cabal file with default-extensions" $
        parseCabalExtensions exampleCabal singleExtensionCabal >>= \extMap ->
            extMap `shouldBe` singleExtensionMap
    it "should parse Cabal file with default-extensions inside common stanza" $
        parseCabalExtensions exampleCabal commonStanzaCabal >>= \extMap ->
            extMap `shouldBe` singleExtensionMap
    it "should parse extensions.cabal" $
        parseCabalFileExtensions "extensions.cabal" >>= \extMap ->
            extMap `shouldBe` expectedMap
  where
    exampleCabal :: FilePath
    exampleCabal = "example.cabal"

    singleModuleMap :: Map FilePath ParsedExtensions
    singleModuleMap = Map.singleton "src/Extensions.hs" emptyParsedExtensions

    singleExtensionMap :: Map FilePath ParsedExtensions
    singleExtensionMap = Map.singleton "src/Extensions.hs" emptyParsedExtensions
        { parsedExtensionsAll = [On TypeApplications]
        }

    -- Map for the project itself
    expectedMap :: Map FilePath ParsedExtensions
    expectedMap = Map.fromList
        [ "app/Cli.hs"                     `to` defaultExtensions
        , "app/Main.hs"                    `to` defaultExtensions
        , "src/Extensions.hs"              `to` defaultExtensions
        , "src/Extensions/Cabal.hs"        `to` defaultExtensions
        , "src/Extensions/Parser.hs"       `to` defaultExtensions
        , "src/Extensions/Types.hs"        `to` defaultExtensions
        , "test/Test/Extensions.hs"        `to` defaultExtensions
        , "test/Test/Extensions/Cabal.hs"  `to` defaultExtensions
        , "test/Test/Extensions/OnOff.hs"  `to` defaultExtensions
        , "test/Test/Extensions/Parser.hs" `to` defaultExtensions
        , "test/Spec.hs"                   `to` defaultExtensions
        ]
      where
        to = (,)

    -- Minimal cabal file with no modules and no extensions
    minimalCabal :: ByteString
    minimalCabal = Text.encodeUtf8 $ Text.unlines
        [ "cabal-version: 2.4"
        , "name: example"
        , "version: 0.0.0.0"
        , ""
        , "library"
        ]

    -- Cabal file with a single directory and single module
    singleModuleCabal :: ByteString
    singleModuleCabal = Text.encodeUtf8 $ Text.unlines
        [ "cabal-version: 2.4"
        , "name: example"
        , "version: 0.0.0.0"
        , ""
        , "library"
        , "  hs-source-dirs: src"
        , "  exposed-modules: Extensions"
        ]

    -- Cabal file with multiple directories
    multipleDirsCabal :: ByteString
    multipleDirsCabal = Text.encodeUtf8 $ Text.unlines
        [ "cabal-version: 2.4"
        , "name: example"
        , "version: 0.0.0.0"
        , ""
        , "library"
        , "  hs-source-dirs: foo, bar, baz, quux, src"
        , "  exposed-modules: Extensions"
        ]

    -- Cabal file with a single directory, single module and single extension
    singleExtensionCabal :: ByteString
    singleExtensionCabal = Text.encodeUtf8 $ Text.unlines
        [ "cabal-version: 2.4"
        , "name: example"
        , "version: 0.0.0.0"
        , ""
        , "library"
        , "  hs-source-dirs: src"
        , "  exposed-modules: Extensions"
        , "  default-extensions: TypeApplications"
        ]

    -- Cabal file with a common stanza
    commonStanzaCabal :: ByteString
    commonStanzaCabal = Text.encodeUtf8 $ Text.unlines
        [ "cabal-version: 2.4"
        , "name: example"
        , "version: 0.0.0.0"
        , ""
        , "common common-extensions"
        , "  default-extensions: TypeApplications"
        , ""
        , "library"
        , "  import: common-extensions"
        , "  hs-source-dirs: src"
        , "  exposed-modules: Extensions"
        ]

defaultExtensions :: ParsedExtensions
defaultExtensions = emptyParsedExtensions
    { parsedExtensionsAll = map On
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
    }
