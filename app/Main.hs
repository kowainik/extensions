module Main (main) where

import Colourista (errorMessage, infoMessage)
import Control.Exception (catch)
import Data.Foldable (forM_, for_)
import Data.List (nub)
import Data.Text (Text)
import GHC.Exts (sortWith)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (</>))

import Cli (ExtensionsArgs (..), Toggle (..), runExtensionsCli)
import Extensions (ExtensionsResult, getModuleExtentions, getPackageExtentions)
import Extensions.Cabal (CabalException, parseCabalFileExtensions)
import Extensions.Parser (parseFile)
import Extensions.Types (Extensions (..), OnOffExtension (..), ParsedExtensions (..),
                         SafeHaskellExtension, showOnOffExtension)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO


main :: IO ()
main = do
    ExtensionsArgs{..} <- runExtensionsCli
    run extensionsArgsCabalFilePath
        extensionsArgsModuleFilePath
        extensionsArgsCabalToggle
        extensionsArgsModulesToggle


run :: Maybe FilePath -> Maybe FilePath -> Toggle -> Toggle -> IO ()
run mCabalPath mModulePath cabal modules = case (mCabalPath, mModulePath, cabal, modules) of
    -- no options specified
    (Nothing, Nothing, Enabled, Enabled) ->
        findCabalFile >>= printAllModulesExtensions

    -- --no-modules
    (Nothing, Nothing, Enabled, Disabled) ->
        findCabalFile >>= printOnlyCabalExtensions

    -- --no-cabal
    (Nothing, Nothing, Disabled, _modules) -> exitAfter $ do
        errorMessage "No cabal file or module specified"
        infoMessage "Use the --module-file-path option to print extensions for a module"

    -- --cabal-file-path
    (Just cabalPath, Nothing, Enabled, Enabled) ->
        withPath cabalPath (printAllModulesExtensions cabalPath)

    -- --cabal-file-path --no-modules
    (Just cabalPath, Nothing, Enabled, Disabled) ->
        withPath cabalPath (printOnlyCabalExtensions cabalPath)

    -- --cabal-file-path --no-cabal
    (Just _cabalPath, _modulePath, Disabled, _modules) -> exitAfter $
        errorMessage "Both --cabal-file-path and --no-cabal are specified"

    -- --module-file-path --no-modules
    (_cabalPath, Just _modulePath, _cabal, Disabled) -> exitAfter $
        errorMessage "Both --module-file-path and --no-modules are specified"

    -- --module-file-path
    (Nothing, Just modulePath, Enabled, Enabled) -> withPath modulePath $ do
        cabalPath  <- findCabalFile
        extsResult <- getModuleExtentions cabalPath modulePath
        infoMessage $ "Cabal extensions and extensions in module " <> Text.pack modulePath
        printExtensionsResult extsResult

    -- --module-file-path --no-cabal
    (Nothing, Just modulePath, Disabled, Enabled) -> withPath modulePath $
        parseFile modulePath >>= \case
            Left err ->
                errorMessage $ "Error parsing module: " <> tshow err
            Right ParsedExtensions{..} -> do
                infoMessage $ "Extensions defined only in " <> Text.pack modulePath
                printOnOffExtensions parsedExtensionsAll
                case parsedExtensionsSafe of
                    Just safe -> print safe
                    Nothing   -> pure ()

    -- --cabal-file-path --module-file-path
    (Just cabalPath, Just modulePath, Enabled, Enabled) ->
        withPath cabalPath $ withPath modulePath $ do
            extsResult <- getModuleExtentions cabalPath modulePath
            infoMessage $ "Cabal extensions and extensions in module " <> Text.pack modulePath
            printExtensionsResult extsResult
  where
    -- check whether the file exist under the path and run action only if exists
    withPath :: FilePath -> IO a -> IO a
    withPath path action = doesFileExist path >>= \pathExists ->
        if pathExists
        then action
        else exitAfter $ errorMessage $ "File does not exist: " <> Text.pack path

exitAfter :: IO a -> IO b
exitAfter action = action >> exitFailure

-- | Find a @.cabal@ file in the current directory.
findCabalFile :: IO FilePath
findCabalFile = do
    dirPath <- getCurrentDirectory
    dirContent <- listDirectory dirPath
    let cabalFiles = filter (\p -> takeExtension p == ".cabal") dirContent
    let usageHint = infoMessage
            "Use the --cabal-file-path option to specify path to .cabal file"
    case cabalFiles of
        [] -> exitAfter $ do
            errorMessage ".cabal file not found in the current directory"
            usageHint
        [cabalFile] ->
            pure $ dirPath </> cabalFile
        l -> exitAfter $ do
            errorMessage $ "Multiple .cabal files found: "
                <> Text.intercalate ", " (map Text.pack l)
            usageHint

printAllModulesExtensions :: FilePath -> IO ()
printAllModulesExtensions cabalPath = do
    cabalExts <- handleCabalException $ getPackageExtentions cabalPath
    infoMessage "Extensions for each module combined with 'default-extensions'"

    let modules = sortWith fst $ Map.toList cabalExts
    forM_ modules $ \(path, extsRes) -> do
        putStrLn path
        case extsRes of
            Left err   -> errorMessage $ "  Error: " <> tshow err
            Right Extensions{..} -> do
                mapM_ (TextIO.putStrLn . ("    " <>) . showOnOffExtension) extensionsAll
                case extensionsSafe of
                    Just safe -> putStrLn $ "    " <> show safe
                    Nothing   -> pure ()

printOnlyCabalExtensions :: FilePath -> IO ()
printOnlyCabalExtensions cabalPath = do
    cabalExts <- handleCabalException $ parseCabalFileExtensions cabalPath
    let (g, x) = concatAll $ Map.elems cabalExts
    let (exts, mSafe) = ( nub g , nub x)

    infoMessage $ "All language extensions defined in " <> Text.pack cabalPath
    printOnOffExtensions exts
    for_ mSafe $ \case
        Just safe -> print safe
        Nothing   -> pure ()
  where
    concatAll :: [ParsedExtensions] -> ([OnOffExtension], [Maybe SafeHaskellExtension])
    concatAll = foldMap (\ParsedExtensions{..} -> (parsedExtensionsAll, [parsedExtensionsSafe]))

handleCabalException :: IO a -> IO a
handleCabalException action = action `catch` \(exc :: CabalException) -> exitAfter $
    errorMessage $ "Error processing .cabal file: " <> tshow exc

printOnOffExtensions :: Foldable f => f OnOffExtension -> IO ()
printOnOffExtensions = mapM_ (TextIO.putStrLn . showOnOffExtension)

printExtensionsResult :: ExtensionsResult -> IO ()
printExtensionsResult = \case
    Left err -> errorMessage $ "Error in finding extensions: " <> tshow err
    Right Extensions{..} -> do
        printOnOffExtensions extensionsAll
        case extensionsSafe of
            Just safe -> print safe
            Nothing   -> pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show
