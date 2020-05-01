{-# LANGUAGE ApplicativeDo #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

CLI options for the @extensions@ exectuable.
-}

module Cli
    ( ExtensionsArgs (..)
    , Toggle (..)
    , runExtensionsCli
    ) where

import Options.Applicative (Parser, ParserInfo, execParser, flag, fullDesc, help, helper, info,
                            long, metavar, optional, progDesc, strOption)


-- | Options used for the main @extensions@ executable.
data ExtensionsArgs = ExtensionsArgs
    { -- | Path to cabal file
      extensionsArgsCabalFilePath  :: !(Maybe FilePath)
      -- | Path to module
    , extensionsArgsModuleFilePath :: !(Maybe FilePath)
      -- | When 'Disabled', do not check extensions in @.cabal@ file
    , extensionsArgsCabalToggle    :: !Toggle
      -- | When 'Disabled', print only @.cabal@ file's @default-extensions@
    , extensionsArgsModulesToggle  :: !Toggle
    }

data Toggle
    = Disabled
    | Enabled

-- | Run main parser of the @extensions@ command line tool.
runExtensionsCli :: IO ExtensionsArgs
runExtensionsCli = execParser extensionsCliParser

extensionsCliParser :: ParserInfo ExtensionsArgs
extensionsCliParser = info (helper <*> extensionsP) $
    fullDesc <> progDesc "Display Haskell Language Extensions"

-- | @extensions@ command parser.
extensionsP :: Parser ExtensionsArgs
extensionsP = do
    extensionsArgsCabalFilePath  <- filePathP "cabal" ".cabal file"
    extensionsArgsModuleFilePath <- filePathP "module" "Haskell module"
    extensionsArgsCabalToggle    <- noCabalP
    extensionsArgsModulesToggle  <- noModulesP
    pure ExtensionsArgs{..}

filePathP :: String -> String -> Parser (Maybe FilePath)
filePathP opt desc = optional $ strOption $ mconcat
    [ long $ opt <> "-file-path"
    , metavar "FILE_PATH"
    , help $ "Relative path to the " <> desc
    ]

noCabalP :: Parser Toggle
noCabalP = flag Enabled Disabled $ mconcat
    [ long "no-cabal"
    , help "Do not check extensions in the .cabal file"
    ]

noModulesP :: Parser Toggle
noModulesP = flag Enabled Disabled $ mconcat
    [ long "no-modules"
    , help "Print only .cabal file's 'default-extensions'"
    ]
