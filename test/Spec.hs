module Main (main) where

import Test.Hspec (hspec)

import Test.Extensions.Cabal (cabalSpec)
import Test.Extensions.Parser (parserSpec)


main :: IO ()
main = hspec $ do
    cabalSpec
    parserSpec
