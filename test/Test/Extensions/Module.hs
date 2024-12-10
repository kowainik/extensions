module Test.Extensions.Module
    ( moduleParserSpec
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)

import Extensions.Module (parseFile, parseSource)
import Extensions.Types (ModuleParseError (..), OnOffExtension (..), ParsedExtensions (..),
                         SafeHaskellExtension (..), emptyParsedExtensions)


moduleParserSpec :: Spec
moduleParserSpec = describe "Haskell file Extensions Parser" $ do
    failSpec
    onlyExtensionsSpec
    singleLineCommentsSpec
    multiLineCommentsSpec
    cppSpec
    optionsGhcSpec
    mixSpec

failSpec :: Spec
failSpec = describe "Expected test failures" $ do
    it "should throw FileNotFound" $
        parseFile "Java.hs" >>= \res ->
            res `shouldBe` Left (FileNotFound "Java.hs")
    itShouldFail
        "{-# LANGUAGE DependentTypes #-}"
        (UnknownExtensions $ "DependentTypes" :| [])
    itShouldFail (unlines
        [ "{-# LANGUAGE LambdaCase #-}"
        , "{-# LANGUAGE Cpp #-}"
        ])
        (UnknownExtensions $ "Cpp" :| [])
    itShouldFail (unlines
        [ "{-# LANGUAGE Safe #-}"
        , "{-# LANGUAGE Unsafe #-}"
        ])
        (ModuleSafeHaskellConflict $ Safe :| [Unsafe])
    itShouldFail
        "{-# LANGUAGE Safe, Trustworthy, Unsafe #-}"
        (ModuleSafeHaskellConflict $ Safe :| [Trustworthy, Unsafe])
    itShouldFail
        "{-# LANGUAGE NoSafe #-}"
        (UnknownExtensions $ "NoSafe" :| [])

onlyExtensionsSpec :: Spec
onlyExtensionsSpec = describe "Parsing only extensions without anything else" $ do
    itShouldParse "" []
    itShouldParse "{-# LANGUAGE TypeApplications #-}" [TypeApplications]
    itShouldParse "{-# LaNgUaGe CPP #-}" [Cpp]

    itShouldParseSafe "{-# LANGUAGE Safe #-}" Safe
    itShouldParseSafe "{-# LANGUAGE Trustworthy #-}" Trustworthy
    itShouldParseSafe "{-# LANGUAGE Unsafe #-}" Unsafe
    itShouldParseSafe (unlines
        [ "{-# LANGUAGE Safe #-}"
        , "{-# LANGUAGE Safe #-}"
        ])
        Safe

    itShouldParseOnOff
        "{-# LANGUAGE NoImplicitPrelude #-}"
        [Off ImplicitPrelude]
    itShouldParseOnOff
        "{-# LANGUAGE NondecreasingIndentation #-}"
        [On NondecreasingIndentation]
    itShouldParseOnOff
        "{-# LANGUAGE PolymorphicComponents #-}"
        [On RankNTypes]
    itShouldParse "{-#language TypeApplications#-}" [TypeApplications]
    itShouldParse "{-# LANGUAGE TypeApplications, LambdaCase#-}" [TypeApplications, LambdaCase]
    itShouldParse "{-# LANGUAGE   TypeApplications  , LambdaCase   #-}" [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
        , "{-# LANGUAGE GeneralisedNewtypeDeriving #-}"
        ])
        [GeneralizedNewtypeDeriving, GeneralizedNewtypeDeriving]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , " TypeApplications,"
        , " LambdaCase  "
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "    CPP"
        , "  , TypeApplications"
        , "  , LambdaCase"
        , "#-}"
        ])
        [Cpp, TypeApplications, LambdaCase]

singleLineCommentsSpec :: Spec
singleLineCommentsSpec = describe "Parsing extensions with single-line comments" $ do
    itShouldParse (unlines
        [ "-- hello"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE LambdaCase #-}"
        , "-- hello "
        ])
        [LambdaCase]
    itShouldParse
        "{-# LANGUAGE LambdaCase #-} -- this is extension for \\case"
        [LambdaCase]
    itShouldParse (unlines
        [ "-- For better syntax"
        , "{-# LANGUAGE LambdaCase #-}"
        , "-- For explicit type annotations"
        , "{-# LANGUAGE TypeApplications #-}"
        ])
        [LambdaCase, TypeApplications]
    itShouldParse (unlines
        [ "-- For better syntax"
        , "{-# LANGUAGE LambdaCase #-} -- this is extension for \\case  "
        , "-- For explicit type annotations"
        , "{-# LANGUAGE TypeApplications #-}  -- this is extension for @Int"
        ])
        [LambdaCase, TypeApplications]
    itShouldParse (unlines
        [ "    -- Comment with indentation"
        , "{-# LANGUAGE LambdaCase #-}"
        , "    -- Another comment with indentation"
        , "{-# LANGUAGE TypeApplications #-}"
        ])
        [LambdaCase, TypeApplications]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "-- hello"
        , "LambdaCase  "
        , "#-}"
        ])
        [LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "TypeApplications,"
        , "-- hello"
        , "LambdaCase  "
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "  TypeApplications,"
        , "  -- hello"
        , "  LambdaCase  "
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "  -- For explicit type annotations"
        , "  TypeApplications,"
        , "  -- For nicer syntax"
        , "  LambdaCase  "
        , "  -- That's all folks!"
        , "#-}"
        ])
        [TypeApplications, LambdaCase]

multiLineCommentsSpec :: Spec
multiLineCommentsSpec = describe "Parsing extensions with multi-line comments" $ do
    itShouldParse
        "{-# LANGUAGE {- WHAT IS -} LambdaCase {- THIS SYNTAX? o_O -} #-}"
        [LambdaCase]
    itShouldParse
        "{- Haskelll -} {-# {- extension -} LANGUAGE {- called -} LambdaCase {- and nothing else -} #-} {- matters -}"
        [LambdaCase]
    itShouldParse (unlines
        [ ""
        , "{- |"
        , "Hello"
        , "-}"
        , "{-# LANGUAGE CPP #-}"
        ])
        [Cpp]
    itShouldParse (unlines
        [ ""
        , "-- | Hey"
        , "-- Hello"
        , "--"
        , "module Abc"
        ])
        []
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
    itShouldParse
        "{-# LANGUAGE LambdaCase #-} {- hello -}"
        [LambdaCase]
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
        [ "{-# LANGUAGE TypeApplications #-}"
        , "{- This is"
        , "a very long"
        , "multiline"
        , "comment"
        , "-}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications #-} {- Long comment"
        , "explaining why this extension is required -}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]

cppSpec :: Spec
cppSpec = describe "Parsing extensions with CPP" $ do
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

optionsGhcSpec :: Spec
optionsGhcSpec = describe "Parsing LANGUAGE and OPTIONS_GHC pragmas" $ do
    itShouldParse
        "{-# OPTIONS_GHC -fno-warn-orphans #-}"
        []
    itShouldParse
        "{-# options_ghc -freverse-errors #-}"
        []
    itShouldParse
        "{-# OPTIONS_GHC -fno-warn-orphans -freverse-errors #-}"
        []
    itShouldParse (unlines
        [ "{-# OPTIONS_GHC"
        , " -fno-warn-orphans"
        , " -freverse-errors "
        , "#-}"
        ])
        []
    itShouldParse (unlines
        [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
        , "{-# OPTIONS_GHC -freverse-errors #-}"
        ])
        []
    itShouldParse (unlines
        [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
        , ""
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [LambdaCase]
    itShouldParse (unlines
        [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
        , ""
        , "{-# LANGUAGE TypeApplications #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "{-# OPTIONS_GHC -fno-warn-orphans #-}"
        , "{-# LANGUAGE TypeApplications #-}"
        , "{-# OPTIONS_GHC -freverse-errors #-}"
        , "{-# LANGUAGE LambdaCase #-}"
        ])
        [TypeApplications, LambdaCase]

mixSpec :: Spec
mixSpec = describe "Parsing combinations of different parts" $ do
    itShouldParse (unlines
        [ "{-# LANGUAGE TypeApplications, LambdaCase #-}"
        , "{-# language CPP #-}"
        , "{-# LANGUAGE"
        , "  DerivingVia,"
        , "  DerivingStrategies"
        , "#-}"
        ])
        [TypeApplications, LambdaCase, Cpp, DerivingVia, DerivingStrategies]
    itShouldParse (unlines
        [ "-- first language extension"
        , "{-# LANGUAGE TypeApplications #-} {- this is type applications -}"
        , "{- This is nice syntax for \\case instead of"
        , "   case mx of"
        , "       Nothing -> ..."
        , "       Just x  -> ... -}"
        , "{-# LANGUAGE LambdaCase {- CASE -} #-}"
        , "  -- That's all folks!"
        , ""
        , "main :: IO ()"
        , "main = putStrLn \"LANGUAGE\""
        ])
        [TypeApplications, LambdaCase]
    itShouldParse (unlines
        [ "-- this is super new extension"
        , "#if __GLASGOW_HASKELL__ < 810"
        , "{-# LANGUAGE TypeApplications #-}"
        , "-- for old GHC versions we can use another one"
        , "#else"
        , "-- or maybe not..."
        , "{-# LANGUAGE LambdaCase #-}"
        , "-- YES!"
        , "#endif"
        , "{- We can also always use strategies -}"
        , "{-# LANGUAGE DerivingStrategies #-}"
        ])
        [TypeApplications, LambdaCase, DerivingStrategies]
    itShouldParse (unlines
        [ "{-# LANGUAGE"
        , "-- this is really weird?"
        , "#if WHAT_IS THIS_SYNTAX o_O"
        , "  {- or is it?.. -}"
        , "  TypeApplications,"
        , "  {- YES! Nobody should write"
        , "     such language pragmas!"
        , "  -}"
        , "#else"
        , "  -- this is fine though"
        , "  -- i said, this is fine"
        , "  LambdaCase"
        , "#endif"
        , "-- innocent comment at the end"
        , "#-}"
        ])
        [TypeApplications, LambdaCase]
    itShouldParseExtensions (unlines
        [ "{-# LANGUAGE CPP #-}"
        , "#if __GLASGOW_HASKELL__ >= 702"
        , "{-# LANGUAGE Trustworthy #-}"
        , "#endif"
        , ""
        , "-- |"
        , "-- Module      : Data.ByteString.Base64"
        ])
        ParsedExtensions
            { parsedExtensionsAll  = [On Cpp]
            , parsedExtensionsSafe = Just Trustworthy
            }

itShouldParse :: String -> [Extension] -> SpecWith (Arg Expectation)
itShouldParse s = itShouldParseOnOff s . map On

itShouldParseOnOff :: String -> [OnOffExtension] -> SpecWith (Arg Expectation)
itShouldParseOnOff input res = itShouldParseExtensions input $ emptyParsedExtensions
   { parsedExtensionsAll = res
   }

itShouldParseSafe :: String -> SafeHaskellExtension -> SpecWith (Arg Expectation)
itShouldParseSafe input res = itShouldParseExtensions input $ emptyParsedExtensions
   { parsedExtensionsSafe = Just res
   }

itShouldParseExtensions :: String -> ParsedExtensions -> SpecWith (Arg Expectation)
itShouldParseExtensions input res = it ("should parse:\n" <> indent input) $
    parseSource (encodeUtf8 $ pack input) `shouldBe` Right res

itShouldFail :: String -> ModuleParseError -> SpecWith (Arg Expectation)
itShouldFail input err = it ("should not parse:\n" <> indent input) $
    parseSource (encodeUtf8 $ pack input) `shouldBe` Left err

indent :: String -> String
indent = unlines . map ("      " <>) . lines
