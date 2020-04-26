{-# LANGUAGE CPP #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Functions to extract extensions from the @.cabal@ files.
-}

module Extensions.Cabal
    ( parseCabalExtensions
    , extractCabalExtensions
    ) where

import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe)
import Distribution.ModuleName (ModuleName (..), toFilePath)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
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
import Distribution.Verbosity (normal)
import GHC.LanguageExtensions.Type (Extension (..))
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>))

import qualified Data.HashMap.Strict as HM
import qualified Language.Haskell.Extension as Cabal



{- | Parse default extensions from a @.cabal@ file under given
'FilePath'.
-}
parseCabalExtensions :: FilePath -> IO (HashMap FilePath [Extension])
parseCabalExtensions cabalPath = do
    pkgDesc <- readGenericPackageDescription normal cabalPath
    extractCabalExtensions pkgDesc

{- | Extract Haskell Language extensions from a Cabal package
description.
-}
extractCabalExtensions :: GenericPackageDescription -> IO (HashMap FilePath [Extension])
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
    foldSndMap f = foldMap f . map snd


    libraryToExtensions :: CondTree var deps Library -> IO (HashMap FilePath [Extension])
    libraryToExtensions = condTreeToExtensions
        (map toModulePath . exposedModules)
        libBuildInfo

    foreignToExtensions :: CondTree var deps ForeignLib -> IO (HashMap FilePath [Extension])
    foreignToExtensions = condTreeToExtensions (const []) foreignLibBuildInfo

    exeToExtensions :: CondTree var deps Executable -> IO (HashMap FilePath [Extension])
    exeToExtensions = condTreeToExtensions (\Executable{..} -> [modulePath]) buildInfo

    testToExtensions :: CondTree var deps TestSuite -> IO (HashMap FilePath [Extension])
    testToExtensions = condTreeToExtensions testMainPath testBuildInfo
      where
        testMainPath :: TestSuite -> [FilePath]
        testMainPath TestSuite{..} = case testInterface of
            TestSuiteExeV10 _ path -> [path]
            TestSuiteLibV09 _ m    -> [toModulePath m]
            TestSuiteUnsupported _ -> []

    benchToExtensions :: CondTree var deps Benchmark -> IO (HashMap FilePath [Extension])
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
    -> IO (HashMap FilePath [Extension])
condTreeToExtensions extractModules extractBuildInfo condTree = do
    let comp = condTreeData condTree
    let buildInfo = extractBuildInfo comp
    let extensions = mapMaybe cabalToGhcExtension $ defaultExtensions buildInfo
    let srcDirs = hsSourceDirs buildInfo
    let modules = extractModules comp ++
            map toModulePath (otherModules buildInfo ++ autogenModules buildInfo)
    modulesToExtensions extensions srcDirs modules

modulesToExtensions
    :: [Extension]
    -- ^ List of default extensions in the stanza
    -> [FilePath]
    -- ^ hs-src-dirs
    -> [FilePath]
    -- ^ All modules in the stanza
    -> IO (HashMap FilePath [Extension])
modulesToExtensions extensions srcDirs = go []
  where
    go :: [FilePath] -> [FilePath] -> IO (HashMap FilePath [Extension])
    go files [] = pure $ HM.fromList $ map (, extensions) files
    go files (m:ms) = findDir m srcDirs >>= \case
        Nothing         -> go files ms
        Just modulePath -> go (modulePath : files) ms

    findDir :: FilePath -> [FilePath] -> IO (Maybe FilePath)
    findDir modulePath = \case
        [] -> pure Nothing
        dir:dirs -> do
            let path = dir </> modulePath
            isFile <- doesFileExist path
            if isFile
                then pure $ Just path
                else findDir modulePath dirs

toModulePath :: ModuleName -> FilePath
toModulePath moduleName = toFilePath moduleName <.> "hs"

cabalToGhcExtension :: Cabal.Extension -> Maybe Extension
cabalToGhcExtension = \case
    Cabal.EnableExtension  extension -> toGhcExtension extension
    Cabal.DisableExtension extension -> toGhcExtension extension
    Cabal.UnknownExtension _ -> Nothing

toGhcExtension :: Cabal.KnownExtension -> Maybe Extension
toGhcExtension = \case
    Cabal.OverlappingInstances       -> Just OverlappingInstances
    Cabal.UndecidableInstances       -> Just UndecidableInstances
    Cabal.IncoherentInstances        -> Just IncoherentInstances
    Cabal.DoRec                      -> Just RecursiveDo
    Cabal.RecursiveDo                -> Just RecursiveDo
    Cabal.ParallelListComp           -> Just ParallelListComp
    Cabal.MultiParamTypeClasses      -> Just MultiParamTypeClasses
    Cabal.MonomorphismRestriction    -> Just MonomorphismRestriction
    Cabal.FunctionalDependencies     -> Just FunctionalDependencies
    Cabal.Rank2Types                 -> Just RankNTypes
    Cabal.RankNTypes                 -> Just RankNTypes
    Cabal.PolymorphicComponents      -> Just RankNTypes
    Cabal.ExistentialQuantification  -> Just ExistentialQuantification
    Cabal.ScopedTypeVariables        -> Just ScopedTypeVariables
    Cabal.PatternSignatures          -> Just ScopedTypeVariables
    Cabal.ImplicitParams             -> Just ImplicitParams
    Cabal.FlexibleContexts           -> Just FlexibleContexts
    Cabal.FlexibleInstances          -> Just FlexibleInstances
    Cabal.EmptyDataDecls             -> Just EmptyDataDecls
    Cabal.CPP                        -> Just Cpp
    Cabal.KindSignatures             -> Just KindSignatures
    Cabal.BangPatterns               -> Just BangPatterns
    Cabal.TypeSynonymInstances       -> Just TypeSynonymInstances
    Cabal.TemplateHaskell            -> Just TemplateHaskell
    Cabal.ForeignFunctionInterface   -> Just ForeignFunctionInterface
    Cabal.Arrows                     -> Just Arrows
    Cabal.ImplicitPrelude            -> Just ImplicitPrelude
    Cabal.PatternGuards              -> Just PatternGuards
    Cabal.GeneralizedNewtypeDeriving -> Just GeneralizedNewtypeDeriving
    Cabal.GeneralisedNewtypeDeriving -> Just GeneralizedNewtypeDeriving
    Cabal.MagicHash                  -> Just MagicHash
    Cabal.TypeFamilies               -> Just TypeFamilies
    Cabal.StandaloneDeriving         -> Just StandaloneDeriving
    Cabal.UnicodeSyntax              -> Just UnicodeSyntax
    Cabal.UnliftedFFITypes           -> Just UnliftedFFITypes
    Cabal.InterruptibleFFI           -> Just InterruptibleFFI
    Cabal.CApiFFI                    -> Just CApiFFI
    Cabal.LiberalTypeSynonyms        -> Just LiberalTypeSynonyms
    Cabal.TypeOperators              -> Just TypeOperators
    Cabal.RecordWildCards            -> Just RecordWildCards
    Cabal.RecordPuns                 -> Just NamedWildCards
    Cabal.NamedFieldPuns             -> Just NamedWildCards
    Cabal.DisambiguateRecordFields   -> Just DisambiguateRecordFields
    Cabal.TraditionalRecordSyntax    -> Just TraditionalRecordSyntax
    Cabal.OverloadedStrings          -> Just OverloadedStrings
    Cabal.GADTs                      -> Just GADTs
    Cabal.GADTSyntax                 -> Just GADTSyntax
    Cabal.RelaxedPolyRec             -> Just RelaxedPolyRec
    Cabal.ExtendedDefaultRules       -> Just ExtendedDefaultRules
    Cabal.UnboxedTuples              -> Just UnboxedTuples
    Cabal.DeriveDataTypeable         -> Just DeriveDataTypeable
    Cabal.AutoDeriveTypeable         -> Just DeriveDataTypeable
    Cabal.DeriveGeneric              -> Just DeriveGeneric
    Cabal.DefaultSignatures          -> Just DefaultSignatures
    Cabal.InstanceSigs               -> Just InstanceSigs
    Cabal.ConstrainedClassMethods    -> Just ConstrainedClassMethods
    Cabal.PackageImports             -> Just PackageImports
    Cabal.ImpredicativeTypes         -> Just ImpredicativeTypes
    Cabal.PostfixOperators           -> Just PostfixOperators
    Cabal.QuasiQuotes                -> Just QuasiQuotes
    Cabal.TransformListComp          -> Just TransformListComp
    Cabal.MonadComprehensions        -> Just MonadComprehensions
    Cabal.ViewPatterns               -> Just ViewPatterns
    Cabal.TupleSections              -> Just TupleSections
    Cabal.GHCForeignImportPrim       -> Just GHCForeignImportPrim
    Cabal.NPlusKPatterns             -> Just NPlusKPatterns
    Cabal.DoAndIfThenElse            -> Just DoAndIfThenElse
    Cabal.MultiWayIf                 -> Just MultiWayIf
    Cabal.LambdaCase                 -> Just LambdaCase
    Cabal.RebindableSyntax           -> Just RebindableSyntax
    Cabal.ExplicitForAll             -> Just ExplicitForAll
    Cabal.DatatypeContexts           -> Just DatatypeContexts
    Cabal.MonoLocalBinds             -> Just MonoLocalBinds
    Cabal.DeriveFunctor              -> Just DeriveFunctor
    Cabal.DeriveTraversable          -> Just DeriveTraversable
    Cabal.DeriveFoldable             -> Just DeriveFoldable
    Cabal.NondecreasingIndentation   -> Just NondecreasingIndentation
    Cabal.ConstraintKinds            -> Just ConstraintKinds
    Cabal.PolyKinds                  -> Just PolyKinds
    Cabal.DataKinds                  -> Just DataKinds
    Cabal.ParallelArrays             -> Just ParallelArrays
    Cabal.RoleAnnotations            -> Just RoleAnnotations
    Cabal.OverloadedLists            -> Just OverloadedLists
    Cabal.EmptyCase                  -> Just EmptyCase
    Cabal.NegativeLiterals           -> Just NegativeLiterals
    Cabal.BinaryLiterals             -> Just BinaryLiterals
    Cabal.NumDecimals                -> Just NumDecimals
    Cabal.NullaryTypeClasses         -> Just NullaryTypeClasses
    Cabal.ExplicitNamespaces         -> Just ExplicitNamespaces
    Cabal.AllowAmbiguousTypes        -> Just AllowAmbiguousTypes
    Cabal.JavaScriptFFI              -> Just JavaScriptFFI
    Cabal.PatternSynonyms            -> Just PatternSynonyms
    Cabal.PartialTypeSignatures      -> Just PartialTypeSignatures
    Cabal.NamedWildCards             -> Just NamedWildCards
    Cabal.DeriveAnyClass             -> Just DeriveAnyClass
    Cabal.DeriveLift                 -> Just DeriveLift
    Cabal.StaticPointers             -> Just StaticPointers
    Cabal.StrictData                 -> Just StrictData
    Cabal.Strict                     -> Just Strict
    Cabal.ApplicativeDo              -> Just ApplicativeDo
    Cabal.DuplicateRecordFields      -> Just DuplicateRecordFields
    Cabal.TypeApplications           -> Just TypeApplications
    Cabal.TypeInType                 -> Just TypeInType
    Cabal.UndecidableSuperClasses    -> Just UndecidableSuperClasses
    Cabal.MonadFailDesugaring        -> Just MonadFailDesugaring
    Cabal.TemplateHaskellQuotes      -> Just TemplateHaskellQuotes
    Cabal.OverloadedLabels           -> Just OverloadedLabels
    Cabal.TypeFamilyDependencies     -> Just TypeFamilyDependencies
    Cabal.DerivingStrategies         -> Just DerivingStrategies
    Cabal.DerivingVia                -> Just DerivingVia
    Cabal.UnboxedSums                -> Just UnboxedSums
    Cabal.HexFloatLiterals           -> Just HexFloatLiterals
    Cabal.BlockArguments             -> Just BlockArguments
    Cabal.NumericUnderscores         -> Just NumericUnderscores
    Cabal.QuantifiedConstraints      -> Just QuantifiedConstraints
    Cabal.StarIsType                 -> Just StarIsType
    Cabal.EmptyDataDeriving          -> Just EmptyDataDeriving
#if __GLASGOW_HASKELL__ >= 810
    Cabal.CUSKs                    -> Just CUSKs
    Cabal.ImportQualifiedPost      -> Just ImportQualifiedPost
    Cabal.StandaloneKindSignatures -> Just StandaloneKindSignatures
    Cabal.UnliftedNewtypes         -> Just UnliftedNewtypes
#endif
    -- non-GHC extensions
    Cabal.Generics               -> Nothing
    Cabal.ExtensibleRecords      -> Nothing
    Cabal.RestrictedTypeSynonyms -> Nothing
    Cabal.HereDocuments          -> Nothing
    Cabal.MonoPatBinds           -> Nothing
    Cabal.XmlSyntax              -> Nothing
    Cabal.RegularPatterns        -> Nothing
    Cabal.SafeImports            -> Nothing
    Cabal.Safe                   -> Nothing
    Cabal.Trustworthy            -> Nothing
    Cabal.Unsafe                 -> Nothing
    Cabal.NewQualifiedOperators  -> Nothing
