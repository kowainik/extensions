{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Parser for Haskell Modules to get all Haskell Language Extensions used.
-}

module Extensions.Parser
       ( parseFile
       , parseSource
       , parseSourceWithPath
       ) where

import Data.ByteString (ByteString)
import GHC.LanguageExtensions.Type (Extension (..))
import Text.Parsec (ParseError, alphaNum, between, char, eof, many, many1, manyTill, noneOf, oneOf,
                    optional, parse, sepBy1, try, unexpected, (<|>))
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (anyChar, endOfLine, letter, newline, space, spaces, string)
import Text.Read (readMaybe)

import qualified Data.ByteString as BS


{- | By the given file path, reads the file and returns parsed list of
'Extension's, if parsing succeeds.
-}
parseFile :: FilePath -> IO (Either ParseError [Extension])
parseFile file = parseSource <$> BS.readFile file

{- | By the given file path and file source content, returns parsed list of
'Extension's, if parsing succeeds.
-}
parseSourceWithPath :: FilePath -> ByteString -> Either ParseError [Extension]
parseSourceWithPath = parse extensionsP

{- | By the given file source content, returns parsed list of
'Extension's, if parsing succeeds.
-}
parseSource :: ByteString -> Either ParseError [Extension]
parseSource = parseSourceWithPath "SourceName"

{- | The main parser of 'Extension's.

It parses language pragmas or comments until end of file or the first line with
the function/import/module name.
-}
extensionsP :: Parser [Extension]
extensionsP = concat <$> manyTill
    (try singleExtensionsP <|> try commentP <|> try cppP)
    (eof <|> (() <$ manyTill endOfLine letter))

{- | Single LANGUAGE pragma parser.

@
 {-# LANGUAGE XXX
  , YYY ,
  ZZZ
 #-}
@
-}
singleExtensionsP :: Parser [Extension]
singleExtensionsP = pragma (commaSep (tryCpp *> extensionP <* tryCpp) <* spaces)
  where
    tryCpp :: Parser ()
    tryCpp = try (optional cppP)

-- | Parses all known 'Extension's.
extensionP :: Parser Extension
extensionP = (spaces *> many1 alphaNum <* spaces) >>= \txt ->
    case readMaybe @Extension txt of
        Just ext -> pure ext
        Nothing  -> unexpected $ "Unknown Extension: " <> txt

{- | Parser for standard pragma keywords: @{-# LANGUAGE XXX #-}@
-}
pragma :: Parser a -> Parser a
pragma p = between (string "{-#") (string "#-}" <* newLines)
    (language *> p <* newLines)
  where
    -- case insensitive word @LANGUAGE@.
    language :: Parser ()
    language = newLines
        *> oneOf "lL"
        *> oneOf "aA"
        *> oneOf "nN"
        *> oneOf "gG"
        *> oneOf "uU"
        *> oneOf "aA"
        *> oneOf "gG"
        *> oneOf "eE"
        *> newLines

-- | Comma separated parser. Newlines and spaces are allowed around comma.
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy1` (try $ newLines *> char ',' <* newLines)

{- | Haskell comment parser.
-}
commentP :: Parser [a]
commentP = [] <$ (spaces *> string "{-" *> manyTill anyChar (try $ string "-}"))
    <* newLines

cppP :: Parser [a]
cppP = [] <$ many newline <* try (char '#' <* noneOf "-") <* manyTill anyChar (try endOfLine)

-- | Any combination of spaces and newlines.
newLines :: Parser ()
newLines = () <$ many (space <|> endOfLine)

deriving stock instance Read Extension
