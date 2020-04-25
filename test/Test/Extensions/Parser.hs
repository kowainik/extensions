module Test.Extensions.Parser
    ( parserSpec
    ) where

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)

import Extensions.Parser (ExtensionsParseResult, parseSourceFile)


parserSpec :: Spec
parserSpec = describe "Haskell file Extensions Parser" $ do
    itShouldParse "{-# LANGUAGE TypeApplications #-}" (Right [TypeApplications])
    itShouldParse "{-#language TypeApplications#-}" (Right [TypeApplications])
    itShouldParse "{-# LANGUAGE TypeApplications, LambdaCase#-}" (Right [TypeApplications, LambdaCase])
    itShouldParse "{-# LANGUAGE   TypeApplications  , LambdaCase   #-}" (Right [TypeApplications, LambdaCase])
    itShouldParse ( unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        ] )
        (Right [TypeApplications, LambdaCase])
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , " TypeApplications,"
        , " LambdaCase  "
        , "#-}"
        ])
        (Right [TypeApplications, LambdaCase])
    itShouldParse (unlines
        [ "{-# LANGUAGE   TypeApplications  , LambdaCase   #-}"
        , "{- hello -}"
        ])
        (Right [TypeApplications, LambdaCase])
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "{- hello -}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        (Right [TypeApplications, LambdaCase])
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "  "
        , "{- hello -}"
        , "{-# LANGUAGE LambdaCase #-}"
        , ""
        , "hello :: IO ()"
        ])
        (Right [TypeApplications, LambdaCase])

  where
    itShouldParse :: String -> ExtensionsParseResult -> SpecWith (Arg Expectation)
    itShouldParse input res = it
        ("should parse:\n" <> unlines (map ("    " <>) $ lines input)) $
            parseSourceFile (encodeUtf8 $ pack input) `shouldBe` res
