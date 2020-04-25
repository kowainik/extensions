module Test.Extensions.Parser
    ( parserSpec
    ) where

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)

import Extensions.Parser (parseSource)


parserSpec :: Spec
parserSpec = describe "Haskell file Extensions Parser" $ do
    itShouldParse "{-# LANGUAGE TypeApplications #-}" [TypeApplications]
    itShouldParse "{-#language TypeApplications#-}" [TypeApplications]
    itShouldParse "{-# LANGUAGE TypeApplications, LambdaCase#-}" [TypeApplications, LambdaCase]
    itShouldParse "{-# LANGUAGE   TypeApplications  , LambdaCase   #-}" [TypeApplications, LambdaCase]
    itShouldParse ( unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        ] )
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , " TypeApplications,"
        , " LambdaCase  "
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE   TypeApplications  , LambdaCase   #-}"
        , "{- hello -}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "{- hello -}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "  "
        , "{- hello -}"
        , "{-# LANGUAGE LambdaCase #-}"
        , ""
        , "hello :: IO ()"
        ])
        [TypeApplications, LambdaCase]

  where
    itShouldParse :: String -> [Extension] -> SpecWith (Arg Expectation)
    itShouldParse input res = it
        ("should parse:\n" <> unlines (map ("    " <>) $ lines input)) $
            parseSource (encodeUtf8 $ pack input) `shouldBe` Right res
