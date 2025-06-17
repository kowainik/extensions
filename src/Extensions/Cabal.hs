{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020-2022 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to extract extensions from the @.cabal@ files.
-}

module Extensions.Cabal
    ( parseCabalFileExtensions
    , parseCabalExtensions
    , extractCabalExtensions

      -- * Bridge between Cabal and GHC extensions
    , cabalToGhcExtension
    , toGhcExtension
    , toSafeExtensions
    ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec.Error (PError, showPError)
import Distribution.Types.Benchmark (Benchmark (..))
import Distribution.Types.BenchmarkInterface (BenchmarkInterface (..))
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Executable (Executable (..))
import Distribution.Types.ForeignLib (ForeignLib (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.TestSuite (TestSuite (..))
import Distribution.Types.TestSuiteInterface (TestSuiteInterface (..))
import Distribution.Utils.Path (getSymbolicPath)
import GHC.LanguageExtensions.Type (Extension (..))
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

import Extensions.Types (CabalException (..), OnOffExtension (..), ParsedExtensions (..),
                         SafeHaskellExtension (..))

import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Language.Haskell.Extension as Cabal


{- | Parse default extensions from a @.cabal@ file under given
'FilePath'.

__Throws__:

* 'CabalException'
-}
parseCabalFileExtensions :: FilePath -> IO (Map FilePath ParsedExtensions)
parseCabalFileExtensions cabalPath = doesFileExist cabalPath >>= \hasCabalFile ->
    if hasCabalFile
    then ByteString.readFile cabalPath >>= parseCabalExtensions cabalPath
    else throwIO $ CabalFileNotFound cabalPath

{- | Parse default extensions from a @.cabal@ file content. This
function takes a path to a @.cabal@ file. The path is only used for error
message. Pass empty string, if you don't have a path to @.cabal@ file.

__Throws__:

* 'CabalException'
-}
parseCabalExtensions :: FilePath -> ByteString -> IO (Map FilePath ParsedExtensions)
parseCabalExtensions path cabal = do
    let (_warnings, res) = runParseResult $ parseGenericPackageDescription cabal
    case res of
        Left (_version, errors) ->
            throwIO $ CabalParseError $ prettyCabalErrors errors
        Right pkgDesc -> extractCabalExtensions pkgDesc
  where
    prettyCabalErrors :: Foldable f => f PError -> Text
    prettyCabalErrors = Text.intercalate "\n" . map errorToText . toList

    errorToText :: PError -> Text
    errorToText = Text.pack . showPError path

{- | Extract Haskell Language extensions from a Cabal package
description.
-}
extractCabalExtensions :: GenericPackageDescription -> IO (Map FilePath ParsedExtensions)
extractCabalExtensions GenericPackageDescription{..} = mconcat
    [ foldMap    libraryToExtensions condLibrary
    , foldSndMap libraryToExtensions condSubLibraries
    , foldSndMap foreignToExtensions condForeignLibs
    , foldSndMap exeToExtensions     condExecutables
    , foldSndMap testToExtensions    condTestSuites
    , foldSndMap benchToExtensions   condBenchmarks
    ]
  where
    foldSndMap :: Monoid m => (a -> m) -> [(b, a)] -> m
    foldSndMap f = foldMap (f . snd)

    libraryToExtensions :: CondTree var deps Library -> IO (Map FilePath ParsedExtensions)
    libraryToExtensions = condTreeToExtensions
        (map toModulePath . exposedModules)
        libBuildInfo

    foreignToExtensions :: CondTree var deps ForeignLib -> IO (Map FilePath ParsedExtensions)
    foreignToExtensions = condTreeToExtensions (const []) foreignLibBuildInfo

    exeToExtensions :: CondTree var deps Executable -> IO (Map FilePath ParsedExtensions)
    exeToExtensions = condTreeToExtensions (\Executable{..} -> [modulePath]) buildInfo

    testToExtensions :: CondTree var deps TestSuite -> IO (Map FilePath ParsedExtensions)
    testToExtensions = condTreeToExtensions testMainPath testBuildInfo
      where
        testMainPath :: TestSuite -> [FilePath]
        testMainPath TestSuite{..} = case testInterface of
            TestSuiteExeV10 _ path -> [path]
            TestSuiteLibV09 _ m    -> [toModulePath m]
            TestSuiteUnsupported _ -> []

    benchToExtensions :: CondTree var deps Benchmark -> IO (Map FilePath ParsedExtensions)
    benchToExtensions = condTreeToExtensions benchMainPath benchmarkBuildInfo
      where
        benchMainPath :: Benchmark -> [FilePath]
        benchMainPath Benchmark{..} = case benchmarkInterface of
            BenchmarkExeV10 _ path -> [path]
            BenchmarkUnsupported _ -> []

condTreeToExtensions
    :: (comp -> [FilePath])
    -- ^ Get all modules as file paths from a component, not listed in 'BuildInfo'
    -> (comp -> BuildInfo)
    -- ^ Extract 'BuildInfo' from component
    -> CondTree var deps comp
    -- ^ Cabal stanza
    -> IO (Map FilePath ParsedExtensions)
condTreeToExtensions extractModules extractBuildInfo condTree = do
    let comp = condTreeData condTree
    let buildInfo = extractBuildInfo comp
    let srcDirs = getSymbolicPath <$> hsSourceDirs buildInfo :: [FilePath]
    let modules = extractModules comp ++
            map toModulePath (otherModules buildInfo ++ autogenModules buildInfo)
    let (safeExts, parsedExtensionsAll) = partitionEithers $ mapMaybe cabalToGhcExtension $ defaultExtensions buildInfo
    parsedExtensionsSafe <- case nub safeExts of
        []   -> pure Nothing
        [x]  -> pure $ Just x
        x:xs -> throwIO $ CabalSafeExtensionsConflict $ x :| xs

    modulesToExtensions ParsedExtensions{..} srcDirs modules

modulesToExtensions
    :: ParsedExtensions
    -- ^ List of default extensions in the stanza
    -> [FilePath]
    -- ^ hs-src-dirs
    -> [FilePath]
    -- ^ All modules in the stanza
    -> IO (Map FilePath ParsedExtensions)
modulesToExtensions extensions srcDirs = case srcDirs of
    [] -> findTopLevel
    _  -> findInDirs []
  where
    mapFromPaths :: [FilePath] -> Map FilePath ParsedExtensions
    mapFromPaths = Map.fromList . map (, extensions)

    findInDirs :: [FilePath] -> [FilePath] -> IO (Map FilePath ParsedExtensions)
    findInDirs res [] = pure $ mapFromPaths res
    findInDirs res (m:ms) = findDir m srcDirs >>= \case
        Nothing         -> findInDirs res ms
        Just modulePath -> findInDirs (modulePath : res) ms

    findTopLevel :: [FilePath] -> IO (Map FilePath ParsedExtensions)
    findTopLevel modules = do
        mPaths <- traverse (withDir Nothing) modules
        pure $ mapFromPaths $ catMaybes mPaths

    -- Find directory where path exists and return full path
    findDir :: FilePath -> [FilePath] -> IO (Maybe FilePath)
    findDir modulePath = \case
        [] -> pure Nothing
        dir:dirs -> withDir (Just dir) modulePath >>= \case
            Nothing   -> findDir modulePath dirs
            Just path -> pure $ Just path

    -- returns path if it exists inside optional dir
    withDir :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
    withDir mDir path = do
        let fullPath = maybe path (\dir -> dir </> path) mDir
        doesFileExist fullPath >>= \isFile ->
            if isFile
            then pure $ Just fullPath
            else pure Nothing

toModulePath :: ModuleName -> FilePath
toModulePath moduleName = toFilePath moduleName <.> "hs"

-- | Convert 'Cabal.Extension' to 'OnOffExtension' or 'SafeHaskellExtension'.
cabalToGhcExtension :: Cabal.Extension -> Maybe (Either SafeHaskellExtension OnOffExtension)
cabalToGhcExtension = \case
    Cabal.EnableExtension  extension -> case (toGhcExtension extension, toSafeExtensions extension) of
        (Nothing, safe) -> Left <$> safe
        (ghc, _)        -> Right . On <$> ghc
    Cabal.DisableExtension extension -> Right . Off <$> toGhcExtension extension
    Cabal.UnknownExtension _ -> Nothing

-- | Convert 'Cabal.KnownExtension' to 'OnOffExtension'.
toGhcExtension :: Cabal.KnownExtension -> Maybe Extension
toGhcExtension = \case
    Cabal.OverlappingInstances              -> Just OverlappingInstances
    Cabal.UndecidableInstances              -> Just UndecidableInstances
    Cabal.IncoherentInstances               -> Just IncoherentInstances
    Cabal.DoRec                             -> Just RecursiveDo
    Cabal.RecursiveDo                       -> Just RecursiveDo
    Cabal.ParallelListComp                  -> Just ParallelListComp
    Cabal.MultiParamTypeClasses             -> Just MultiParamTypeClasses
    Cabal.MonomorphismRestriction           -> Just MonomorphismRestriction
    Cabal.FunctionalDependencies            -> Just FunctionalDependencies
    Cabal.Rank2Types                        -> Just RankNTypes
    Cabal.RankNTypes                        -> Just RankNTypes
    Cabal.PolymorphicComponents             -> Just RankNTypes
    Cabal.ExistentialQuantification         -> Just ExistentialQuantification
    Cabal.ScopedTypeVariables               -> Just ScopedTypeVariables
    Cabal.PatternSignatures                 -> Just ScopedTypeVariables
    Cabal.ImplicitParams                    -> Just ImplicitParams
    Cabal.FlexibleContexts                  -> Just FlexibleContexts
    Cabal.FlexibleInstances                 -> Just FlexibleInstances
    Cabal.EmptyDataDecls                    -> Just EmptyDataDecls
    Cabal.CPP                               -> Just Cpp
    Cabal.KindSignatures                    -> Just KindSignatures
    Cabal.BangPatterns                      -> Just BangPatterns
    Cabal.TypeSynonymInstances              -> Just TypeSynonymInstances
    Cabal.TemplateHaskell                   -> Just TemplateHaskell
    Cabal.ForeignFunctionInterface          -> Just ForeignFunctionInterface
    Cabal.Arrows                            -> Just Arrows
    Cabal.ImplicitPrelude                   -> Just ImplicitPrelude
    Cabal.PatternGuards                     -> Just PatternGuards
    Cabal.GeneralizedNewtypeDeriving        -> Just GeneralizedNewtypeDeriving
    Cabal.GeneralisedNewtypeDeriving        -> Just GeneralizedNewtypeDeriving
    Cabal.MagicHash                         -> Just MagicHash
    Cabal.TypeFamilies                      -> Just TypeFamilies
    Cabal.StandaloneDeriving                -> Just StandaloneDeriving
    Cabal.UnicodeSyntax                     -> Just UnicodeSyntax
    Cabal.UnliftedFFITypes                  -> Just UnliftedFFITypes
    Cabal.InterruptibleFFI                  -> Just InterruptibleFFI
    Cabal.CApiFFI                           -> Just CApiFFI
    Cabal.LiberalTypeSynonyms               -> Just LiberalTypeSynonyms
    Cabal.TypeOperators                     -> Just TypeOperators
    Cabal.RecordWildCards                   -> Just RecordWildCards
#if MIN_VERSION_ghc_boot_th(9,4,1)
    Cabal.RecordPuns                        -> Just NamedFieldPuns
    Cabal.NamedFieldPuns                    -> Just NamedFieldPuns
#else
    Cabal.RecordPuns                        -> Just RecordPuns
    Cabal.NamedFieldPuns                    -> Just RecordPuns
#endif
    Cabal.DisambiguateRecordFields          -> Just DisambiguateRecordFields
    Cabal.TraditionalRecordSyntax           -> Just TraditionalRecordSyntax
    Cabal.OverloadedStrings                 -> Just OverloadedStrings
    Cabal.GADTs                             -> Just GADTs
    Cabal.GADTSyntax                        -> Just GADTSyntax
    Cabal.RelaxedPolyRec                    -> Just RelaxedPolyRec
    Cabal.ExtendedDefaultRules              -> Just ExtendedDefaultRules
    Cabal.UnboxedTuples                     -> Just UnboxedTuples
    Cabal.DeriveDataTypeable                -> Just DeriveDataTypeable
    Cabal.AutoDeriveTypeable                -> Just DeriveDataTypeable
    Cabal.DeriveGeneric                     -> Just DeriveGeneric
    Cabal.DefaultSignatures                 -> Just DefaultSignatures
    Cabal.InstanceSigs                      -> Just InstanceSigs
    Cabal.ConstrainedClassMethods           -> Just ConstrainedClassMethods
    Cabal.PackageImports                    -> Just PackageImports
    Cabal.ImpredicativeTypes                -> Just ImpredicativeTypes
    Cabal.PostfixOperators                  -> Just PostfixOperators
    Cabal.QuasiQuotes                       -> Just QuasiQuotes
    Cabal.TransformListComp                 -> Just TransformListComp
    Cabal.MonadComprehensions               -> Just MonadComprehensions
    Cabal.ViewPatterns                      -> Just ViewPatterns
    Cabal.TupleSections                     -> Just TupleSections
    Cabal.GHCForeignImportPrim              -> Just GHCForeignImportPrim
    Cabal.NPlusKPatterns                    -> Just NPlusKPatterns
    Cabal.DoAndIfThenElse                   -> Just DoAndIfThenElse
    Cabal.MultiWayIf                        -> Just MultiWayIf
    Cabal.LambdaCase                        -> Just LambdaCase
    Cabal.RebindableSyntax                  -> Just RebindableSyntax
    Cabal.ExplicitForAll                    -> Just ExplicitForAll
    Cabal.DatatypeContexts                  -> Just DatatypeContexts
    Cabal.MonoLocalBinds                    -> Just MonoLocalBinds
    Cabal.DeriveFunctor                     -> Just DeriveFunctor
    Cabal.DeriveTraversable                 -> Just DeriveTraversable
    Cabal.DeriveFoldable                    -> Just DeriveFoldable
    Cabal.NondecreasingIndentation          -> Just NondecreasingIndentation
    Cabal.ConstraintKinds                   -> Just ConstraintKinds
    Cabal.PolyKinds                         -> Just PolyKinds
    Cabal.DataKinds                         -> Just DataKinds
    Cabal.ParallelArrays                    -> Just ParallelArrays
    Cabal.RoleAnnotations                   -> Just RoleAnnotations
    Cabal.OverloadedLists                   -> Just OverloadedLists
    Cabal.EmptyCase                         -> Just EmptyCase
    Cabal.NegativeLiterals                  -> Just NegativeLiterals
    Cabal.BinaryLiterals                    -> Just BinaryLiterals
    Cabal.NumDecimals                       -> Just NumDecimals
    Cabal.NullaryTypeClasses                -> Just NullaryTypeClasses
    Cabal.ExplicitNamespaces                -> Just ExplicitNamespaces
    Cabal.AllowAmbiguousTypes               -> Just AllowAmbiguousTypes
    Cabal.JavaScriptFFI                     -> Just JavaScriptFFI
    Cabal.PatternSynonyms                   -> Just PatternSynonyms
    Cabal.PartialTypeSignatures             -> Just PartialTypeSignatures
    Cabal.NamedWildCards                    -> Just NamedWildCards
    Cabal.DeriveAnyClass                    -> Just DeriveAnyClass
    Cabal.DeriveLift                        -> Just DeriveLift
    Cabal.StaticPointers                    -> Just StaticPointers
    Cabal.StrictData                        -> Just StrictData
    Cabal.Strict                            -> Just Strict
    Cabal.ApplicativeDo                     -> Just ApplicativeDo
    Cabal.DuplicateRecordFields             -> Just DuplicateRecordFields
    Cabal.TypeApplications                  -> Just TypeApplications
    Cabal.TypeInType                        -> Just TypeInType
    Cabal.UndecidableSuperClasses           -> Just UndecidableSuperClasses
#if MIN_VERSION_ghc_boot_th(9,2,1)
    Cabal.MonadFailDesugaring               -> Nothing
#else
    Cabal.MonadFailDesugaring               -> Just MonadFailDesugaring
#endif
    Cabal.TemplateHaskellQuotes             -> Just TemplateHaskellQuotes
    Cabal.OverloadedLabels                  -> Just OverloadedLabels
    Cabal.TypeFamilyDependencies            -> Just TypeFamilyDependencies
    Cabal.DerivingStrategies                -> Just DerivingStrategies
    Cabal.DerivingVia                       -> Just DerivingVia
    Cabal.UnboxedSums                       -> Just UnboxedSums
    Cabal.HexFloatLiterals                  -> Just HexFloatLiterals
    Cabal.BlockArguments                    -> Just BlockArguments
    Cabal.NumericUnderscores                -> Just NumericUnderscores
    Cabal.QuantifiedConstraints             -> Just QuantifiedConstraints
    Cabal.StarIsType                        -> Just StarIsType
    Cabal.EmptyDataDeriving                 -> Just EmptyDataDeriving
#if __GLASGOW_HASKELL__ >= 810
    Cabal.CUSKs                             -> Just CUSKs
    Cabal.ImportQualifiedPost               -> Just ImportQualifiedPost
    Cabal.StandaloneKindSignatures          -> Just StandaloneKindSignatures
    Cabal.UnliftedNewtypes                  -> Just UnliftedNewtypes
#else
    Cabal.CUSKs                             -> Nothing
    Cabal.ImportQualifiedPost               -> Nothing
    Cabal.StandaloneKindSignatures          -> Nothing
    Cabal.UnliftedNewtypes                  -> Nothing
#endif
#if __GLASGOW_HASKELL__ >= 900
    Cabal.LexicalNegation                   -> Just LexicalNegation
    Cabal.QualifiedDo                       -> Just QualifiedDo
    Cabal.LinearTypes                       -> Just LinearTypes
#else
    Cabal.LexicalNegation                   -> Nothing
    Cabal.QualifiedDo                       -> Nothing
    Cabal.LinearTypes                       -> Nothing
#endif
#if __GLASGOW_HASKELL__ >= 902
    Cabal.FieldSelectors                    -> Just FieldSelectors
    Cabal.OverloadedRecordDot               -> Just OverloadedRecordDot
    Cabal.UnliftedDatatypes                 -> Just UnliftedDatatypes
#else
    Cabal.FieldSelectors                    -> Nothing
    Cabal.OverloadedRecordDot               -> Nothing
    Cabal.UnliftedDatatypes                 -> Nothing
#endif
#if __GLASGOW_HASKELL__ >= 904
    Cabal.OverloadedRecordUpdate            -> Just OverloadedRecordUpdate
    Cabal.AlternativeLayoutRule             -> Just AlternativeLayoutRule
    Cabal.AlternativeLayoutRuleTransitional -> Just AlternativeLayoutRuleTransitional
    Cabal.RelaxedLayout                     -> Just RelaxedLayout
#else
    Cabal.OverloadedRecordUpdate            -> Nothing
    Cabal.AlternativeLayoutRule             -> Nothing
    Cabal.AlternativeLayoutRuleTransitional -> Nothing
    Cabal.RelaxedLayout                     -> Nothing
#endif
#if MIN_VERSION_Cabal_syntax(3,10,0)
# if __GLASGOW_HASKELL__ >= 906
    Cabal.DeepSubsumption                   -> Just DeepSubsumption
    Cabal.TypeData                          -> Just TypeData
# else
    Cabal.DeepSubsumption                   -> Nothing
    Cabal.TypeData                          -> Nothing
# endif
#endif
#if MIN_VERSION_Cabal_syntax(3,12,0)
# if __GLASGOW_HASKELL__ >= 908
    Cabal.TypeAbstractions                  -> Just TypeAbstractions
# else
    Cabal.TypeAbstractions                  -> Nothing
# endif
# if __GLASGOW_HASKELL__ >= 910
    Cabal.RequiredTypeArguments             -> Just RequiredTypeArguments
    Cabal.ExtendedLiterals                  -> Just ExtendedLiterals
    Cabal.ListTuplePuns                     -> Just ListTuplePuns
# else
    Cabal.RequiredTypeArguments             -> Nothing
    Cabal.ExtendedLiterals                  -> Nothing
    Cabal.ListTuplePuns                     -> Nothing
# endif
#endif
#if MIN_VERSION_Cabal_syntax(3,14,0)
# if __GLASGOW_HASKELL__ >= 912
    -- This branch cannot be satisfied yet but we're including it so
    -- we don't forget to enable these when they become available.
    Cabal.NamedDefaults                     -> Just NamedDefaults
    Cabal.MultilineStrings                  -> Just MultilineStrings
    Cabal.OrPatterns                        -> Just OrPatterns
# else
    Cabal.NamedDefaults                     -> Nothing
    Cabal.MultilineStrings                  -> Nothing
    Cabal.OrPatterns                        -> Nothing
# endif
#endif
    -- GHC extensions, parsed by both Cabal and GHC, but don't have an Extension constructor
    Cabal.Safe                              -> Nothing
    Cabal.Trustworthy                       -> Nothing
    Cabal.Unsafe                            -> Nothing
    -- non-GHC extensions
    Cabal.Generics                          -> Nothing
    Cabal.ExtensibleRecords                 -> Nothing
    Cabal.RestrictedTypeSynonyms            -> Nothing
    Cabal.HereDocuments                     -> Nothing
    Cabal.MonoPatBinds                      -> Nothing
    Cabal.XmlSyntax                         -> Nothing
    Cabal.RegularPatterns                   -> Nothing
    Cabal.SafeImports                       -> Nothing
    Cabal.NewQualifiedOperators             -> Nothing

-- | Convert 'Cabal.KnownExtension' to 'SafeHaskellExtension'.
toSafeExtensions :: Cabal.KnownExtension -> Maybe SafeHaskellExtension
toSafeExtensions = \case
    Cabal.Safe        -> Just Safe
    Cabal.Trustworthy -> Just Trustworthy
    Cabal.Unsafe      -> Just Unsafe
    _                 -> Nothing
