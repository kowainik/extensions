module Test.Extensions
    ( getExtensionsSpec
    ) where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import Extensions (getModuleExtentions, getModuleExtentionsBySource, getPackageExtentions,
                   getPackageExtentionsBySources)
import Extensions.Types (Extensions (..), ExtensionsError (..), ExtensionsResult,
                         OnOffExtension (..), ParsedExtensions (..))
import Test.Extensions.Cabal (defaultExtensions)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


getExtensionsSpec :: Spec
getExtensionsSpec = describe "Get Extensions" $ do
    it "should getPackageExtensions" $
        getPackageExtentions cabal >>= \res -> res `shouldBe` resultMap
    it "should getPackageExtensionsBySource" $ do
        res <- sources >>= getPackageExtentionsBySources cabal
        res `shouldBe` resultMap
    it "should getPackageExtensionsBySource, return NonCabalModule" $ do
        srcs <- sources
        res <- getPackageExtentionsBySources cabal $
            Map.insert unknownModule "Hello" srcs
        res `shouldBe` Map.insert unknownModule (Left $ NotCabalModule unknownModule) resultMap
    it "should getPackageExtensionsBySource, return SourceNotFound" $ do
        srcs <- sources
        res <- getPackageExtentionsBySources cabal $
            Map.delete knownModule srcs
        res `shouldBe` Map.insert knownModule (Left $ SourceNotFound knownModule) resultMap
    it "should getModuleExtensions" $ do
        res <- getModuleExtentions cabal knownModule
        res `shouldBe` Right exts
    it "should getModuleExtensions, return NotCabalModule" $ do
        res <- getModuleExtentions cabal unknownModule
        res `shouldBe` Left (NotCabalModule unknownModule)
    it "should getModuleExtensionsBySource" $ do
        res <- knownSource >>= getModuleExtentionsBySource cabal knownModule
        res `shouldBe` Right exts
    it "should getModuleExtensions, return NotCabalModule" $ do
        res <- getModuleExtentionsBySource cabal unknownModule "Hello"
        res `shouldBe` Left (NotCabalModule unknownModule)

resultMap :: Map FilePath ExtensionsResult
resultMap = Map.fromList
    [ "app/Cli.hs"                     `to` Right
        (extsPlus $ Set.singleton $ On ApplicativeDo)
    , "app/Main.hs"                    `to` Right exts
    , "src/Extensions.hs"              `to` Right exts
    , "src/Extensions/Cabal.hs"        `to` Right
        (extsPlus $ Set.singleton $ On Cpp)
    , "src/Extensions/Module.hs"       `to` Right exts
    , "src/Extensions/Package.hs"      `to` Right exts
    , "src/Extensions/Types.hs"        `to` Right
        (extsPlus $ Set.fromList [On Cpp, On DeriveAnyClass])
    , "test/Test/Extensions.hs"        `to` Right exts
    , "test/Test/Extensions/Cabal.hs"  `to` Right exts
    , "test/Test/Extensions/Module.hs" `to` Right exts
    , "test/Test/Extensions/Types.hs"  `to` Right exts
    , "test/Spec.hs"                   `to` Right exts
    ]
  where
    to = (,)

exts :: Extensions
exts = Extensions
    { extensionsAll = Set.fromList $ parsedExtensionsAll defaultExtensions
    , extensionsSafe = Nothing
    }

extsPlus :: Set OnOffExtension -> Extensions
extsPlus s = exts
    { extensionsAll = extensionsAll exts <> s
    }

sources :: IO (Map FilePath ByteString)
sources = Map.traverseWithKey (\f _ -> BS.readFile f) resultMap

cabal :: FilePath
cabal = "extensions.cabal"

unknownModule :: FilePath
unknownModule = "src/Unknown.hs"

knownModule :: FilePath
knownModule = "src/Extensions.hs"

knownSource :: IO ByteString
knownSource = fromMaybe "" . Map.lookup knownModule <$> sources
