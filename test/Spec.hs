module Main (main) where

import Test.Hspec (hspec)

import Test.Extensions (getExtensionsSpec)
import Test.Extensions.Cabal (cabalSpec)
import Test.Extensions.OnOff (onOffSpec)
import Test.Extensions.Parser (parserSpec)


main :: IO ()
main = hspec $ do
    onOffSpec
    cabalSpec
    parserSpec
    getExtensionsSpec
