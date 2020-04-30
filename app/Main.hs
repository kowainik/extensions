module Main (main) where

import Colourista (errorMessage, infoMessage, warningMessage)
import Data.Foldable (forM_)
import Data.Text (Text)
import GHC.Exts (sortWith)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (</>))

import Cli (ExtensionsArgs (..), runExtensionsCli)
import Extensions (ExtensionsResult, getModuleExtentions, getPackageExtentions)
import Extensions.Cabal (parseCabalFileExtensions)
import Extensions.OnOff (OnOffExtension (..), mergeExtensions)
import Extensions.Parser (parseFile)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO


main :: IO ()
main = do
    ExtensionsArgs{..} <- runExtensionsCli

    let printExtensions :: FilePath -> IO ()
        printExtensions = printWithCabalExtensions
            extensionsArgsNoModules
            extensionsArgsModuleFilePath

    case extensionsArgsCabalFilePath of
        Just cabalPath -> doesFileExist cabalPath >>= \hasCabal ->
            if hasCabal
            then printExtensions cabalPath
            else do
                errorMessage $ ".cabal file does not exist: " <> Text.pack cabalPath
                exitFailure
        Nothing ->
            if extensionsArgsNoCabal
            then case extensionsArgsModuleFilePath of
                Nothing -> do
                    warningMessage "No cabal file or module specified"
                    infoMessage "Use --cabal-file-path or --module-file-path options"
                    exitFailure
                Just modulePath -> parseFile modulePath >>= \case
                    Left err -> print err
                    Right exts -> do
                        infoMessage $ "Extensions defined only in " <> Text.pack modulePath
                        printOnOffExtensions exts
            else findCabalFile >>= printExtensions

-- | Find a @.cabal@ file in the current directory.
findCabalFile :: IO FilePath
findCabalFile = do
    dirPath <- getCurrentDirectory
    dirContent <- listDirectory dirPath
    let cabalFiles = filter (\p -> takeExtension p == ".cabal") dirContent
    let usageHint = infoMessage
            "Use the --cabal-file-path option to specify path to .cabal file"
    case cabalFiles of
        [] -> do
            errorMessage ".cabal file not found in the current directory"
            usageHint
            exitFailure
        [cabalFile] ->
            pure $ dirPath </> cabalFile
        l -> do
            errorMessage $ "Multiple .cabal files found: "
                <> Text.intercalate ", " (map Text.pack l)
            usageHint
            exitFailure

-- TODO: should this be library function?
showOnOffExtension :: OnOffExtension -> Text
showOnOffExtension = \case
    On ext  -> Text.pack $ show ext
    Off ext -> "No" <> Text.pack (show ext)

printOnOffExtensions :: Foldable f => f OnOffExtension -> IO ()
printOnOffExtensions = mapM_ (TextIO.putStrLn . showOnOffExtension)

printExtensionsResult :: ExtensionsResult -> IO ()
printExtensionsResult = either print printOnOffExtensions

printWithCabalExtensions :: Bool -> Maybe FilePath -> FilePath -> IO ()
printWithCabalExtensions noModules mModulePath cabalPath = case mModulePath of
    Nothing ->
        if noModules
        then do
            infoMessage $ "All language extensions defined in " <> Text.pack cabalPath
            printAllCabalExtensions
        else do
            infoMessage "Extensions for each module combined with 'default-extensions'"
            printAllModulesExtensions
    Just modulePath -> do
        -- ignore noModules variable here
        infoMessage $ "Cabal extensions and extensions in module " <> Text.pack modulePath
        extsResult <- getModuleExtentions cabalPath modulePath
        printExtensionsResult extsResult
  where
    printAllCabalExtensions :: IO ()
    printAllCabalExtensions = do
        cabalExts <- parseCabalFileExtensions cabalPath
        let exts = mergeExtensions $ concatMap snd $ Map.toList cabalExts
        printOnOffExtensions exts

    printAllModulesExtensions :: IO ()
    printAllModulesExtensions = do
        cabalExts <- getPackageExtentions cabalPath
        let modules = sortWith fst $ Map.toList cabalExts
        forM_ modules $ \(path, extsRes) -> do
            putStrLn path
            case extsRes of
                Left err   -> putStrLn $ "  " ++ show err
                Right exts ->
                    mapM_ (TextIO.putStrLn . ("    " <>) . showOnOffExtension) exts
