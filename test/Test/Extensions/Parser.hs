module Test.Extensions.Parser
    ( parserSpec
    ) where

import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)

import Extensions.OnOff (OnOffExtension (..))
import Extensions.Parser (parseSource)


parserSpec :: Spec
parserSpec = describe "Haskell file Extensions Parser" $ do
    itShouldParse "{-# LANGUAGE TypeApplications #-}" [TypeApplications]
    itShouldParse "{-# LaNgUaGe CPP #-}" [Cpp]
    itShouldParseOnOff "{-# LANGUAGE NoImplicitPrelude #-}" [Off ImplicitPrelude]
    itShouldParseOnOff
        "{-# LANGUAGE NondecreasingIndentation #-}"
        [On NondecreasingIndentation]
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
    itShouldParse (unlines
        [ "#if __GLASGOW_HASKELL__ < 810"
        , "{-# LANGUAGE TypeApplications #-}"
        , "#endif"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "#if __GLASGOW_HASKELL__ < 810"
        , "{-# LANGUAGE TypeApplications #-}"
        , "#else"
        , "{-# LANGUAGE LambdaCase #-}"
        , "#endif"
        , "{-# LANGUAGE DerivingStrategies #-}"
        ])
        [TypeApplications, LambdaCase, DerivingStrategies]
    itShouldParse (unlines
        [ "#if __GLASGOW_HASKELL__ < 810"
        , "{-# LANGUAGE TypeApplications #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        , "#endif"
        , "{-# LANGUAGE DerivingStrategies #-}"
        ])
        [TypeApplications, LambdaCase, DerivingStrategies]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "#if WHAT_IS THIS_SYNTAX o_O"
        , "TypeApplications,"
        , "#else"
        , "LambdaCase"
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "#if WHAT_IS THIS_SYNTAX o_O"
        , " TypeApplications,"
        , "#else"
        , " LambdaCase"
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "#if WHAT_IS THIS_SYNTAX o_O"
        , "TypeApplications,"
        , "#else"
        , "LambdaCase  "
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "#if WHAT_IS THIS_SYNTAX o_O"
        , "TypeApplications,"
        , "#else"
        , "LambdaCase"
        , "#endif"
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "#if WHAT_IS THIS_SYNTAX o_O"
        , "TypeApplications,"
        , "#else"
        , "LambdaCase  "
        , "#endif"
        , "#-}"
        ])
        [TypeApplications, LambdaCase]

  where
    itShouldParse :: String -> [Extension] -> SpecWith (Arg Expectation)
    itShouldParse s = itShouldParseOnOff s . map On

    itShouldParseOnOff :: String -> [OnOffExtension] -> SpecWith (Arg Expectation)
    itShouldParseOnOff input res = it
        ("should parse:\n" <> unlines (map ("    " <>) $ lines input)) $
            parseSource (encodeUtf8 $ pack input) `shouldBe` Right res
