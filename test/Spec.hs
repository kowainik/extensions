module Main (main) where

import Test.Hspec (hspec)

import Test.Extensions (getExtensionsSpec)
import Test.Extensions.Cabal (cabalSpec)
import Test.Extensions.Module (moduleParserSpec)
import Test.Extensions.Types (onOffSpec)


main :: IO ()
main = hspec $ do
    onOffSpec
    cabalSpec
    moduleParserSpec
    getExtensionsSpec
