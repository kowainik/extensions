{- |
Copyright: (c) 2020-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Parser for Haskell Modules to get all Haskell Language Extensions used.
-}

module Extensions.Module
       ( parseFile
       , parseSource
       , parseSourceWithPath

         -- * Internal Parsers
       , extensionsP
       , singleExtensionsP
       , extensionP
       , languagePragmaP
       , optionsGhcP
       , pragmaP
       , commentP
       , cppP
       ) where

import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Functor ((<&>), void)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import System.Directory (doesFileExist)
import Text.Parsec (alphaNum, between, char, eof, many, many1, manyTill, noneOf, oneOf, parse,
                    sepBy1, skipMany, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (anyChar, endOfLine, letter, newline, space, spaces, string)
import Text.Read (readMaybe)

import Extensions.Types (ModuleParseError (..), OnOffExtension (..), ParsedExtensions (..),
                         SafeHaskellExtension (..), readOnOffExtension)

import qualified Data.ByteString as BS


-- | Internal data type for known and unknown extensions.
data ParsedExtension
    = KnownExtension OnOffExtension
    | SafeExtension SafeHaskellExtension
    | UnknownExtension String

handleParsedExtensions :: [ParsedExtension] -> Either ModuleParseError ParsedExtensions
handleParsedExtensions = handleResult . partitionEithers . map toEither
  where
    toEither :: ParsedExtension -> Either String (Either SafeHaskellExtension OnOffExtension)
    toEither (UnknownExtension ext) = Left ext
    toEither (KnownExtension ext)   = Right $ Right ext
    toEither (SafeExtension ext)    = Right $ Left ext

    -- Make sure that there is no conflicting 'SafeHaskellExtension's.
    handleResult
        :: ([String], [Either SafeHaskellExtension OnOffExtension])
        -> Either ModuleParseError ParsedExtensions
    handleResult (unknown, knownAndSafe) = case unknown of
        []   -> let (safe, known) = partitionEithers knownAndSafe in case nub safe of
            []   -> Right ParsedExtensions
                { parsedExtensionsAll = known
                , parsedExtensionsSafe = Nothing
                }
            [s]  -> Right ParsedExtensions
                { parsedExtensionsAll = known
                , parsedExtensionsSafe = Just s
                }
            s:ss -> Left $ ModuleSafeHaskellConflict $ s :| ss
        x:xs -> Left $ UnknownExtensions $ x :| xs

{- | By the given file path, reads the file and returns 'ParsedExtensions', if
parsing succeeds.
-}
parseFile :: FilePath -> IO (Either ModuleParseError ParsedExtensions)
parseFile file = doesFileExist file >>= \hasFile ->
    if hasFile
    then parseSourceWithPath file <$> BS.readFile file
    else pure $ Left $ FileNotFound file

{- | By the given file path and file source content, returns 'ParsedExtensions',
if parsing succeeds.

This function takes a path to a Haskell source file. The path is only used for
error message. Pass empty string or use 'parseSource', if you don't have a path
to a Haskell module.
-}
parseSourceWithPath :: FilePath -> ByteString -> Either ModuleParseError ParsedExtensions
parseSourceWithPath path src = case parse extensionsP path src of
    Left err         -> Left $ ParsecError err
    Right parsedExts -> handleParsedExtensions parsedExts

{- | By the given file source content, returns 'ParsedExtensions', if parsing
succeeds.
-}
parseSource :: ByteString -> Either ModuleParseError ParsedExtensions
parseSource = parseSourceWithPath "SourceName"

{- | The main parser of 'ParsedExtension'.

It parses language pragmas or comments until end of file or the first line with
the function/import/module name.
-}
extensionsP :: Parser [ParsedExtension]
extensionsP = concat <$>
    ( newLines *>
      manyTill
        (try singleExtensionsP <|> try optionsGhcP <|> [] <$ try commentP <|> try cppP)
        (eof <|> void (manyTill endOfLine letter))
    )

{- | Single LANGUAGE pragma parser.

@
 \{\-# LANGUAGE XXX
  , YYY ,
  ZZZ
 #\-\}
@
-}
singleExtensionsP :: Parser [ParsedExtension]
singleExtensionsP =
    languagePragmaP (commaSep (nonExtP *> extensionP <* nonExtP) <* spaces)

nonExtP :: Parser ()
nonExtP = skipMany (try cppP <|> try commentP)

{- | Parses all known and unknown 'OnOffExtension's or 'SafeHaskellExtension's.
-}
extensionP :: Parser ParsedExtension
extensionP = (spaces *> many1 alphaNum <* spaces) <&> \txt ->
    case readOnOffExtension txt of
        Just ext -> KnownExtension ext
        Nothing  -> case readMaybe @SafeHaskellExtension txt of
            Just ext -> SafeExtension ext
            Nothing  -> UnknownExtension txt

{- | Parser for standard language pragma keywords: @\{\-\# LANGUAGE XXX \#\-\}@
-}
languagePragmaP :: Parser a -> Parser a
languagePragmaP = pragmaP $ istringP "LANGUAGE"

{- | Parser for GHC options pragma keywords: @\{\-\# OPTIONS_GHC YYY \#\-\}@
-}
optionsGhcP :: Parser [a]
optionsGhcP = [] <$ optionsGhcPragmaP (many1 ghcOptionP)
  where
    ghcOptionP :: Parser String
    ghcOptionP = newLines *> many1 (alphaNum <|> char '-') <* newLines

optionsGhcPragmaP :: Parser a -> Parser a
optionsGhcPragmaP = pragmaP $ istringP "OPTIONS_GHC"

-- | Parser for case-insensitive strings.
istringP :: String -> Parser ()
istringP = traverse_ $ \c -> oneOf [toUpper c, toLower c]

{- | Parser for GHC pragmas with a given pragma word.
-}
pragmaP :: Parser () -> Parser a -> Parser a
pragmaP pragmaNameP p = between
    (string "{-#") (string "#-}" <* newLines)
    (newLines *> nonExtP *> pragmaNameP *> newLines *> p <* newLines)

-- | Comma separated parser. Newlines and spaces are allowed around comma.
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy1` try (newLines *> char ',' <* newLines)

{- | Haskell comment parser.
Supports both single-line comments:

  @
  -- I am a single comment
  @

and multi-line comments:

  @
  \{\- I
  AM
  MULTILINE
  \-\}
  @
-}
commentP :: Parser String
commentP = newLines *> (try singleLineCommentP <|> try multiLineCommentP) <* newLines
  where
    singleLineCommentP :: Parser String
    singleLineCommentP =
        string "--" *> manyTill anyChar (try (void endOfLine) <|> eof)

    multiLineCommentP :: Parser String
    multiLineCommentP =
        concat <$> (string "{-" *> manyTill (try multiLineCommentP <|> (: []) <$> anyChar) (try (string "-}")))

{- | CPP syntax parser.

  @
  #if \_\_GLASGOW_HASKELL\_\_ < 810
  -- Could be more Language pragmas that should be parsed
  #endif
  @
-}
cppP :: Parser [a]
cppP =
    [] <$ many newline
       <* try (char '#' <* noneOf "-")
       <* manyTill anyChar (try endOfLine)
       <* newLines

-- | Any combination of spaces and newlines.
newLines :: Parser ()
newLines = skipMany (space <|> endOfLine)
