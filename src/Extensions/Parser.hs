{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Parser for Haskell Modules to get all Haskell Language Extensions used.
-}

module Extensions.Parser
       ( ParseError (..)
       , parseFile
       , parseSource
       , parseSourceWithPath
       ) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Either (partitionEithers)
import Data.Foldable (asum, traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.LanguageExtensions.Type (Extension (..))
import Text.Parsec (alphaNum, between, char, eof, many, many1, manyTill, noneOf, oneOf, parse,
                    sepBy1, try, (<|>))
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (anyChar, endOfLine, letter, newline, space, spaces, string)
import Text.Read (readMaybe)

import Extensions.OnOff (OnOffExtension (..))

import qualified Data.ByteString as BS
import qualified Text.Parsec as Parsec


data ParseError
    -- | File parsing error.
    = ParsecError Parsec.ParseError
    -- | Uknown extensions were used in the module.
    | UnknownExtensions (NonEmpty String)
    deriving stock (Show, Eq)

-- | Internal data type for known and unknown extensions.
data ParsedExtension
    = KnownExtension OnOffExtension
    | UnknownExtension String

handleParsedExtensions :: [ParsedExtension] -> Either (NonEmpty String) [OnOffExtension]
handleParsedExtensions = handleResult . partitionEithers . map toEither
  where
    toEither :: ParsedExtension -> Either String OnOffExtension
    toEither (UnknownExtension ext) = Left ext
    toEither (KnownExtension ext)   = Right ext

    handleResult :: ([String], [OnOffExtension]) -> Either (NonEmpty String) [OnOffExtension]
    handleResult (unknown, known) = case unknown of
        []   -> Right known
        x:xs -> Left $ x :| xs

{- | By the given file path, reads the file and returns parsed list of
'Extension's, if parsing succeeds.
-}
parseFile :: FilePath -> IO (Either ParseError [OnOffExtension])
parseFile file = parseSourceWithPath file <$> BS.readFile file

{- | By the given file path and file source content, returns parsed list of
'Extension's, if parsing succeeds.
-}
parseSourceWithPath :: FilePath -> ByteString -> Either ParseError [OnOffExtension]
parseSourceWithPath path src = case parse extensionsP path src of
    Left err         -> Left $ ParsecError err
    Right parsedExts -> first UnknownExtensions $ handleParsedExtensions parsedExts

{- | By the given file source content, returns parsed list of
'Extension's, if parsing succeeds.
-}
parseSource :: ByteString -> Either ParseError [OnOffExtension]
parseSource = parseSourceWithPath "SourceName"

{- | The main parser of 'Extension's.

It parses language pragmas or comments until end of file or the first line with
the function/import/module name.
-}
extensionsP :: Parser [ParsedExtension]
extensionsP = concat <$> manyTill
    (try singleExtensionsP <|> try optionsGhcP <|> try commentP <|> try cppP)
    (eof <|> (() <$ manyTill endOfLine letter))

{- | Single LANGUAGE pragma parser.

@
 {-# LANGUAGE XXX
  , YYY ,
  ZZZ
 #-}
@
-}
singleExtensionsP :: Parser [ParsedExtension]
singleExtensionsP =
    languagePragmaP (commaSep (nonExtP *> extensionP <* nonExtP) <* spaces)
  where
    nonExtP :: Parser ()
    nonExtP = () <$ many (try cppP <|> try commentP)

-- | Parses all known and unknown 'Extension's.
extensionP :: Parser ParsedExtension
extensionP = (spaces *> many1 alphaNum <* spaces) >>= \txt ->
    pure $ case readOnOffExtension txt of
        Just ext -> KnownExtension ext
        Nothing  -> UnknownExtension txt

{- | Parser for standard language pragma keywords: @{-# LANGUAGE XXX #-}@
-}
languagePragmaP :: Parser a -> Parser a
languagePragmaP = pragmaP $ istringP "LANGUAGE"

{- | Parser for GHC options pragma keywords: @{-# OPTIONS_GHC YYY #-}@
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
    (newLines *> pragmaNameP *> newLines *> p <* newLines)

-- | Comma separated parser. Newlines and spaces are allowed around comma.
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy1` (try $ newLines *> char ',' <* newLines)

{- | Haskell comment parser.
-}
commentP :: Parser [a]
commentP = newLines *> (try singleLineCommentP <|> try multiLineCommentP) <* newLines
  where
    singleLineCommentP :: Parser [a]
    singleLineCommentP = [] <$
        (string "--" *> manyTill anyChar (try (() <$ endOfLine) <|> eof))

    multiLineCommentP :: Parser [a]
    multiLineCommentP = [] <$
        (string "{-" *> manyTill anyChar (try $ string "-}"))

cppP :: Parser [a]
cppP = [] <$ many newline <* try (char '#' <* noneOf "-") <* manyTill anyChar (try endOfLine)

-- | Any combination of spaces and newlines.
newLines :: Parser ()
newLines = () <$ many (space <|> endOfLine)

readOnOffExtension :: String -> Maybe OnOffExtension
readOnOffExtension s = asum
    [ On  <$> readOnExtension s
    , Off <$> readOffExtension
    ]
  where
    readOffExtension :: Maybe Extension
    readOffExtension = do
        ("No", ext) <- Just $ splitAt 2 s
        readOnExtension ext


{- | Parse 'Extension' from a string. 'Read' instance for 'Extension'
doesn't always work since some extensions are named differently.
-}
readOnExtension :: String -> Maybe Extension
readOnExtension = \case
    "GeneralisedNewtypeDeriving" -> Just GeneralizedNewtypeDeriving
    "NamedFieldPuns" -> Just RecordPuns
    "RecordPuns" -> Nothing
    "Rank2Types" -> Just RankNTypes
    "CPP" -> Just Cpp
    "Cpp" -> Nothing
    s -> readMaybe s
