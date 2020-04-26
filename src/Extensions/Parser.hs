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
import Data.Foldable (asum)
import GHC.LanguageExtensions.Type (Extension (..))
import Text.Parsec (ParseError, alphaNum, between, char, eof, many, many1, manyTill, noneOf, oneOf,
                    optional, parse, sepBy1, try, unexpected, (<|>))
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (anyChar, endOfLine, letter, newline, space, spaces, string)
import Text.Read (readMaybe)

import Extensions.OnOff (OnOffExtension (..))

import qualified Data.ByteString as BS


{- | By the given file path, reads the file and returns parsed list of
'Extension's, if parsing succeeds.
-}
parseFile :: FilePath -> IO (Either ParseError [OnOffExtension])
parseFile file = parseSource <$> BS.readFile file

{- | By the given file path and file source content, returns parsed list of
'Extension's, if parsing succeeds.
-}
parseSourceWithPath :: FilePath -> ByteString -> Either ParseError [OnOffExtension]
parseSourceWithPath = parse extensionsP

{- | By the given file source content, returns parsed list of
'Extension's, if parsing succeeds.
-}
parseSource :: ByteString -> Either ParseError [OnOffExtension]
parseSource = parseSourceWithPath "SourceName"

{- | The main parser of 'Extension's.

It parses language pragmas or comments until end of file or the first line with
the function/import/module name.
-}
extensionsP :: Parser [OnOffExtension]
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
singleExtensionsP :: Parser [OnOffExtension]
singleExtensionsP = pragma (commaSep (tryCpp *> onOffExtensionP <* tryCpp) <* spaces)
  where
    tryCpp :: Parser ()
    tryCpp = try (optional cppP)

-- | Parses all known 'Extension's.
onOffExtensionP :: Parser OnOffExtension
onOffExtensionP = (spaces *> many1 alphaNum <* spaces) >>= \txt ->
    case readOnOffExtension txt of
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
