{-# LANGUAGE ApplicativeDo #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

CLI options for the @extensions@ exectuable.
-}

module Cli
    ( ExtensionsArgs (..)
    , runExtensionsCli
    ) where

import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long,
                            metavar, optional, progDesc, strOption, switch)


-- | Options used for the main @extensions@ executable.
data ExtensionsArgs = ExtensionsArgs
    { extensionsArgsModuleFilePath :: !(Maybe FilePath)  -- ^ Path to module
    , extensionsArgsCabalFilePath  :: !(Maybe FilePath)  -- ^ Path to cabal file
    , extensionsArgsNoCabal        :: !Bool  -- ^ Do not fail if no @.cabal@ file
    , extensionsArgsNoModules      :: !Bool  -- ^ Do not print per-module info
    }

-- | Run main parser of the @extensions@ command line tool.
runExtensionsCli :: IO ExtensionsArgs
runExtensionsCli = execParser extensionsCliParser

extensionsCliParser :: ParserInfo ExtensionsArgs
extensionsCliParser = info (helper <*> extensionsP) $
    fullDesc <> progDesc "Display Haskell Language Extensions"

-- | @extensions@ command parser.
extensionsP :: Parser ExtensionsArgs
extensionsP = do
    extensionsArgsModuleFilePath <- moduleFilePathP
    extensionsArgsCabalFilePath  <- cabalFilePathP
    extensionsArgsNoCabal        <- noCabalP
    extensionsArgsNoModules      <- noModulesP
    pure ExtensionsArgs{..}

filePathP :: String -> String -> Parser (Maybe FilePath)
filePathP opt desc = optional $ strOption $ mconcat
    [ long opt
    , metavar "FILE_PATH"
    , help ("Relative path to the " <> desc)
    ]
    
moduleFilePathP :: Parser (Maybe FilePath)
moduleFilePathP = filePathP "module-file-path" "Haskell module"

cabalFilePathP :: Parser (Maybe FilePath)
cabalFilePathP = filePathP "cabal-file-path" ".cabal file"

noCabalP :: Parser Bool
noCabalP = switch $ mconcat
    [ long "no-cabal"
    , help "Do not fail when no .cabal file found"
    ]

noModulesP :: Parser Bool
noModulesP = switch $ mconcat
    [ long "no-modules"
    , help "Do not pring per-module info when printing extension from the .cabal file"
    ]
