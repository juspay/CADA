{-# LANGUAGE BangPatterns,TypeApplications, DeriveGeneric, OverloadedStrings, TupleSections,ImportQualifiedPost,PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Diff where

import qualified GHC.Data.EnumSet as EnumSet
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes,mapMaybe,fromMaybe,isNothing)
import Text.Read (readMaybe)
import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Extra (splitOn)
import GHC.Generics (Generic)
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Process
import Text.Regex.Posix
import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Paths (libdir)
import GHC
import qualified GHC.Driver.Session as GHC
import GHC.Utils.Outputable hiding ((<>))
import GHC.Driver.Flags
import GHC.Driver.Session
import GHC.LanguageExtensions.Type hiding (Extension)
import System.Environment( getArgs )
import GHC.Types.Name
import GHC.Core.TyCo.Rep
import GHC.Driver.Env
import GHC.Tc.Types
import GHC.Unit.Module.ModSummary
import GHC.Utils.Outputable (showSDocUnsafe,ppr,SDoc)
import GHC.Data.Bag (bagToList)
import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Core.Opt.Monad
import GHC.Core
import GHC.Unit.Module.ModGuts
import GHC.Types.Name.Reader
import GHC.Types.Id
import GHC.Data.FastString
import Text.Regex.Posix
import qualified GHC.LanguageExtensions as LangExt
import Data.Generics.Uniplate.Data ()
import Control.Reference ((^.), (!~), biplateRef,(^?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.FilePath ((</>), takeDirectory, takeExtension,takeBaseName)
import Control.Monad (filterM, forM)
import Control.Applicative ((<|>))
import Debug.Trace
import Control.Concurrent.Async (mapConcurrently)

-- Cabal imports for parsing .cabal files
import Distribution.PackageDescription hiding (Extension)
import Distribution.PackageDescription.Parsec
import Distribution.Types.GenericPackageDescription
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.CondTree
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Language.Haskell.Extension (Extension(..), KnownExtension)
import qualified Language.Haskell.Extension as Ext
import Distribution.Verbosity (normal)
import Distribution.Utils.Path (getSymbolicPath)
import qualified GHC.LanguageExtensions as LangExt
import qualified Language.Haskell.Extension as Cabal
import qualified Control.Monad.Catch as MC
import GHC.Driver.Pipeline.Monad
import GHC.Parser.Header (getOptionsFromFile)
import GHC.Driver.Session (parseDynamicFilePragma)

-- Data type to represent source locations
data SourceLocation = SourceLocation {
    startLine :: Int,
    startCol :: Int,
    endLine :: Int,
    endCol :: Int,
    fileName :: String
} deriving (Show, Generic)

instance ToJSON SourceLocation

-- Data type to track function changes
data FunctionModified = FunctionModified {
    deleted :: [String],
    modified :: [String],
    added :: [String],
    modName :: String
} deriving (Show, Generic)

instance ToJSON FunctionModified

-- Data type to track granular changes within functions
data CalledFunctionChanges = CalledFunctionChanges {
    added_functions :: [String],
    removed_functions :: [String],
    added_literals :: [(String, String)],
    removed_literals :: [(String, String)],
    old_function_src_loc :: SourceLocation,
    new_function_src_loc :: SourceLocation
} deriving (Show, Generic)

instance ToJSON CalledFunctionChanges

-- Data type to capture all declaration types
data DetailedChanges = DetailedChanges {
    moduleName :: String,
    -- Functions
    addedFunctions :: [(String, String)],      -- (name, code)
    modifiedFunctions :: [(String, String, String)],  -- (name, old code, new code)
    deletedFunctions :: [(String, String)],    -- (name, code)
    -- Types
    addedTypes :: [(String, String)],          -- (name, code)
    modifiedTypes :: [(String, String, String)],      -- (name, old code, new code)
    deletedTypes :: [(String, String)],        -- (name, code)
    -- Instances
    addedInstances :: [(String, String)],      -- (name, code)
    modifiedInstances :: [(String, String, String)],  -- (name, old code, new code)
    deletedInstances :: [(String, String)]     -- (name, code)
} deriving (Show, Generic)

instance ToJSON DetailedChanges

-- Helper functions for Git operations

-- Clone the repository if it doesn't exist already
cloneRepo :: String -> FilePath -> IO ()
cloneRepo repoUrl localPath = do
    exists <- doesPathExist localPath
    if not exists
        then callCommand $ "git clone " <> repoUrl <> " " <> localPath
        else putStrLn "Repository already cloned."

-- Get files changed between two commits
getChangedFiles :: String -> String -> FilePath -> IO [FilePath]
getChangedFiles branchName newCommit localPath = do
    setCurrentDirectory localPath
    readProcess "git" ["checkout", branchName] ""
    commit <- readProcess "git" ["rev-parse", branchName] ""
    result <- readProcess "git" ["diff", "--name-only", (T.unpack $ T.stripEnd (T.pack commit)), newCommit] ""
    pure $ lines result

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
    contents <- listDirectory dir
    let paths = map (dir </>) contents

    -- Find subdirectories for recursion
    dirs <- filterM doesDirectoryExist paths

    -- Find .cabal files in current directory (check both extension AND that it's a file)
    cabalFiles <- filterM (\p -> do
        isFile <- doesFileExist p
        return (isFile && takeExtension p == ".cabal")
        ) paths

    -- Recursively search subdirectories
    nested <- concat <$> mapM findCabalFiles dirs

    return (cabalFiles ++ nested)

generateProjectRoots :: [FilePath] -> [(FilePath, String)]
generateProjectRoots cabalPaths =
    map (\path -> (dropPrefix (takeDirectory path) ++ "/", takeBaseName path)) cabalPaths
  where
    dropPrefix :: FilePath -> FilePath
    dropPrefix p = case stripPrefix "./" p of
        Just rest -> rest
        Nothing -> p

-- Data structure to hold parsed Cabal configuration
data CabalConfig = CabalConfig {
    cabalSourceDirs :: [FilePath],
    cabalExtensions :: [Extension],
    cabalDependencies :: [String],
    cabalPackageName :: String,
    cabalBasePath :: FilePath,
    cabalDefaultLanguage :: Maybe Cabal.Language
} deriving (Show)

-- Parse a single .cabal file and extract configuration for ALL components
parseCabalFile :: FilePath -> IO [CabalConfig]
parseCabalFile cabalPath = do
    exists <- doesFileExist cabalPath
    if not exists
        then pure []
        else do
            result <- readGenericPackageDescription normal cabalPath
            let basePath = takeDirectory cabalPath
                pkgDesc = packageDescription result
                pkgNameStr = unPackageName . pkgName . package $ pkgDesc
                
                -- Extract from main library component if it exists
                mainLibConfigs = case condLibrary result of
                    Nothing -> []
                    Just condTree -> 
                        let lib = condTreeData condTree
                            libBI = libBuildInfo lib
                            rawSrcDirs = hsSourceDirs libBI
                            srcDirs = map ((basePath </>) . getSymbolicPath) rawSrcDirs
                            finalSrcDirs = if null srcDirs then [basePath] else srcDirs
                            exts = defaultExtensions libBI
                            deps = map (unPackageName . depPkgName) $ targetBuildDepends libBI
                            defaultLang = defaultLanguage libBI
                        in [CabalConfig {
                            cabalSourceDirs = finalSrcDirs,
                            cabalExtensions = exts,
                            cabalDependencies = deps,
                            cabalPackageName = pkgNameStr,
                            cabalBasePath = basePath,
                            cabalDefaultLanguage = defaultLang
                        }]
                
                -- Extract from ALL named sub-libraries
                subLibConfigs = map (\(libName, condTree) ->
                        let lib = condTreeData condTree
                            libBI = libBuildInfo lib
                            rawSrcDirs = hsSourceDirs libBI
                            srcDirs = map ((basePath </>) . getSymbolicPath) rawSrcDirs
                            finalSrcDirs = if null srcDirs then [basePath] else srcDirs
                            exts = defaultExtensions libBI
                            deps = map (unPackageName . depPkgName) $ targetBuildDepends libBI
                            defaultLang = defaultLanguage libBI
                        in CabalConfig {
                            cabalSourceDirs = finalSrcDirs,
                            cabalExtensions = exts,
                            cabalDependencies = deps,
                            cabalPackageName = unUnqualComponentName libName,
                            cabalBasePath = basePath,
                            cabalDefaultLanguage = defaultLang
                        }) (condSubLibraries result)
                
                -- Combine main library and sub-libraries
                libConfigs = mainLibConfigs ++ subLibConfigs
                
                -- Extract from ALL executables
                exeConfigs = map (\(exeName, condTree) ->
                        let exe = condTreeData condTree
                            exeBI = buildInfo exe
                            srcDirs = map ((basePath </>) . getSymbolicPath) $ hsSourceDirs exeBI
                            exts = defaultExtensions exeBI
                            deps = map (unPackageName . depPkgName) $ targetBuildDepends exeBI
                            defaultLang = defaultLanguage exeBI
                        in CabalConfig {
                            cabalSourceDirs = if null srcDirs then [basePath] else srcDirs,
                            cabalExtensions = exts,
                            cabalDependencies = deps,
                            cabalPackageName = pkgNameStr ++ "-" ++ unUnqualComponentName exeName,
                            cabalBasePath = basePath,
                            cabalDefaultLanguage = defaultLang
                        }) (condExecutables result)
                
                -- Extract from ALL test suites
                testConfigs = map (\(testName, condTree) ->
                        let test = condTreeData condTree
                            testBI = testBuildInfo test
                            srcDirs = map ((basePath </>) . getSymbolicPath) $ hsSourceDirs testBI
                            exts = defaultExtensions testBI
                            deps = map (unPackageName . depPkgName) $ targetBuildDepends testBI
                            defaultLang = defaultLanguage testBI
                        in CabalConfig {
                            cabalSourceDirs = if null srcDirs then [basePath] else srcDirs,
                            cabalExtensions = exts,
                            cabalDependencies = deps,
                            cabalPackageName = pkgNameStr ++ "-test-" ++ unUnqualComponentName testName,
                            cabalBasePath = basePath,
                            cabalDefaultLanguage = defaultLang
                        }) (condTestSuites result)
                
                -- Extract from ALL benchmarks
                benchConfigs = map (\(benchName, condTree) ->
                        let bench = condTreeData condTree
                            benchBI = benchmarkBuildInfo bench
                            srcDirs = map ((basePath </>) . getSymbolicPath) $ hsSourceDirs benchBI
                            exts = defaultExtensions benchBI
                            deps = map (unPackageName . depPkgName) $ targetBuildDepends benchBI
                            defaultLang = defaultLanguage benchBI
                        in CabalConfig {
                            cabalSourceDirs = if null srcDirs then [basePath] else srcDirs,
                            cabalExtensions = exts,
                            cabalDependencies = deps,
                            cabalPackageName = pkgNameStr ++ "-bench-" ++ unUnqualComponentName benchName,
                            cabalBasePath = basePath,
                            cabalDefaultLanguage = defaultLang
                        }) (condBenchmarks result)
            
            let allConfigs = libConfigs ++ exeConfigs ++ testConfigs ++ benchConfigs
            -- Debug: print what we parsed
            mapM_ (\cfg -> putStrLn $ "  Parsed component: " ++ cabalPackageName cfg ++ 
                                     " with src dirs: " ++ show (cabalSourceDirs cfg)) allConfigs
            -- Return all configs (library + all executables)
            pure allConfigs

-- Parse all .cabal files in the repository
parseAllCabalFiles :: [FilePath] -> IO [CabalConfig]
parseAllCabalFiles cabalPaths = do
    configs <- mapM parseCabalFile cabalPaths
    pure $ concat configs

-- | Convert a Cabal Extension to a GHC Language Extension
cabalExtToGhcExt :: Cabal.Extension -> (Maybe LangExt.Extension,Bool)
cabalExtToGhcExt (Cabal.EnableExtension ext)  = (cabalKnownExtToGhcExt ext , True)
cabalExtToGhcExt (Cabal.DisableExtension ext)   = (cabalKnownExtToGhcExt  ext, False) -- DisableExtension doesn't map directly
cabalExtToGhcExt (Cabal.UnknownExtension _)   = (Nothing,False)  -- Unknown extensions can't be mapped

-- | Map Cabal's KnownExtension to GHC's Extension
cabalKnownExtToGhcExt :: Cabal.KnownExtension -> Maybe LangExt.Extension
cabalKnownExtToGhcExt ext = case ext of
  Cabal.OverlappingInstances          -> Just LangExt.OverlappingInstances
  Cabal.UndecidableInstances          -> Just LangExt.UndecidableInstances
  Cabal.IncoherentInstances           -> Just LangExt.IncoherentInstances
  Cabal.DoRec                         -> Just LangExt.RecursiveDo
  Cabal.RecursiveDo                   -> Just LangExt.RecursiveDo
  Cabal.ParallelListComp              -> Just LangExt.ParallelListComp
  Cabal.MultiParamTypeClasses         -> Just LangExt.MultiParamTypeClasses
  Cabal.MonomorphismRestriction       -> Just LangExt.MonomorphismRestriction
  Cabal.FunctionalDependencies        -> Just LangExt.FunctionalDependencies
  Cabal.Rank2Types                    -> Just LangExt.RankNTypes
  Cabal.RankNTypes                    -> Just LangExt.RankNTypes
  Cabal.PolymorphicComponents         -> Just LangExt.RankNTypes
  Cabal.ExistentialQuantification     -> Just LangExt.ExistentialQuantification
  Cabal.ScopedTypeVariables           -> Just LangExt.ScopedTypeVariables
  Cabal.PatternSignatures             -> Just LangExt.ScopedTypeVariables
  Cabal.ImplicitParams                -> Just LangExt.ImplicitParams
  Cabal.FlexibleContexts              -> Just LangExt.FlexibleContexts
  Cabal.FlexibleInstances             -> Just LangExt.FlexibleInstances
  Cabal.EmptyDataDecls                -> Just LangExt.EmptyDataDecls
  Cabal.CPP                           -> Just LangExt.Cpp
  Cabal.KindSignatures                -> Just LangExt.KindSignatures
  Cabal.BangPatterns                  -> Just LangExt.BangPatterns
  Cabal.TypeSynonymInstances          -> Just LangExt.TypeSynonymInstances
  Cabal.TemplateHaskell               -> Just LangExt.TemplateHaskell
  Cabal.ForeignFunctionInterface      -> Just LangExt.ForeignFunctionInterface
  Cabal.Arrows                        -> Just LangExt.Arrows
  Cabal.Generics                      -> Nothing  -- Deprecated, no direct mapping
  Cabal.ImplicitPrelude               -> Just LangExt.ImplicitPrelude
  Cabal.NamedFieldPuns                -> Just LangExt.RecordPuns
  Cabal.PatternGuards                 -> Just LangExt.PatternGuards
  Cabal.GeneralizedNewtypeDeriving    -> Just LangExt.GeneralizedNewtypeDeriving
  Cabal.GeneralisedNewtypeDeriving    -> Just LangExt.GeneralizedNewtypeDeriving
  Cabal.ExtensibleRecords             -> Nothing  -- Hugs-specific
  Cabal.RestrictedTypeSynonyms        -> Nothing  -- Hugs-specific
  Cabal.HereDocuments                 -> Nothing  -- Hugs-specific
  Cabal.MagicHash                     -> Just LangExt.MagicHash
  Cabal.TypeFamilies                  -> Just LangExt.TypeFamilies
  Cabal.StandaloneDeriving            -> Just LangExt.StandaloneDeriving
  Cabal.UnicodeSyntax                 -> Just LangExt.UnicodeSyntax
  Cabal.UnliftedFFITypes              -> Just LangExt.UnliftedFFITypes
  Cabal.InterruptibleFFI              -> Just LangExt.InterruptibleFFI
  Cabal.CApiFFI                       -> Just LangExt.CApiFFI
  Cabal.LiberalTypeSynonyms           -> Just LangExt.LiberalTypeSynonyms
  Cabal.TypeOperators                 -> Just LangExt.TypeOperators
  Cabal.RecordWildCards               -> Just LangExt.RecordWildCards
  Cabal.RecordPuns                    -> Just LangExt.RecordPuns
  Cabal.DisambiguateRecordFields      -> Just LangExt.DisambiguateRecordFields
  Cabal.TraditionalRecordSyntax       -> Just LangExt.TraditionalRecordSyntax
  Cabal.OverloadedStrings             -> Just LangExt.OverloadedStrings
  Cabal.GADTs                         -> Just LangExt.GADTs
  Cabal.GADTSyntax                    -> Just LangExt.GADTSyntax
  Cabal.MonoPatBinds                  -> Nothing  -- Deprecated
  Cabal.RelaxedPolyRec                -> Just LangExt.RelaxedPolyRec
  Cabal.ExtendedDefaultRules          -> Just LangExt.ExtendedDefaultRules
  Cabal.UnboxedTuples                 -> Just LangExt.UnboxedTuples
  Cabal.DeriveDataTypeable            -> Just LangExt.DeriveDataTypeable
  Cabal.DeriveGeneric                 -> Just LangExt.DeriveGeneric
  Cabal.DefaultSignatures             -> Just LangExt.DefaultSignatures
  Cabal.InstanceSigs                  -> Just LangExt.InstanceSigs
  Cabal.ConstrainedClassMethods       -> Just LangExt.ConstrainedClassMethods
  Cabal.PackageImports                -> Just LangExt.PackageImports
  Cabal.ImpredicativeTypes            -> Just LangExt.ImpredicativeTypes
  Cabal.NewQualifiedOperators         -> Nothing  -- Deprecated
  Cabal.PostfixOperators              -> Just LangExt.PostfixOperators
  Cabal.QuasiQuotes                   -> Just LangExt.QuasiQuotes
  Cabal.TransformListComp             -> Just LangExt.TransformListComp
  Cabal.MonadComprehensions           -> Just LangExt.MonadComprehensions
  Cabal.ViewPatterns                  -> Just LangExt.ViewPatterns
  Cabal.XmlSyntax                     -> Nothing  -- HSP-specific
  Cabal.RegularPatterns               -> Nothing  -- Not in GHC
  Cabal.TupleSections                 -> Just LangExt.TupleSections
  Cabal.GHCForeignImportPrim          -> Just LangExt.GHCForeignImportPrim
  Cabal.NPlusKPatterns                -> Just LangExt.NPlusKPatterns
  Cabal.DoAndIfThenElse               -> Just LangExt.DoAndIfThenElse
  Cabal.MultiWayIf                    -> Just LangExt.MultiWayIf
  Cabal.LambdaCase                    -> Just LangExt.LambdaCase
  Cabal.RebindableSyntax              -> Just LangExt.RebindableSyntax
  Cabal.ExplicitForAll                -> Just LangExt.ExplicitForAll
  Cabal.DatatypeContexts              -> Just LangExt.DatatypeContexts
  Cabal.MonoLocalBinds                -> Just LangExt.MonoLocalBinds
  Cabal.DeriveFunctor                 -> Just LangExt.DeriveFunctor
  Cabal.DeriveTraversable             -> Just LangExt.DeriveTraversable
  Cabal.DeriveFoldable                -> Just LangExt.DeriveFoldable
  Cabal.NondecreasingIndentation      -> Just LangExt.NondecreasingIndentation
  Cabal.SafeImports                   -> Nothing  -- Part of Safe Haskell, handled differently
  Cabal.Safe                          -> Nothing  -- Safe Haskell mode, not an extension flag
  Cabal.Trustworthy                   -> Nothing  -- Safe Haskell mode, not an extension flag
  Cabal.Unsafe                        -> Nothing  -- Safe Haskell mode, not an extension flag
  Cabal.ConstraintKinds               -> Just LangExt.ConstraintKinds
  Cabal.PolyKinds                     -> Just LangExt.PolyKinds
  Cabal.DataKinds                     -> Just LangExt.DataKinds
  Cabal.ParallelArrays                -> Just LangExt.ParallelArrays
  Cabal.RoleAnnotations               -> Just LangExt.RoleAnnotations
  Cabal.OverloadedLists               -> Just LangExt.OverloadedLists
  Cabal.EmptyCase                     -> Just LangExt.EmptyCase
  Cabal.AutoDeriveTypeable            -> Nothing  -- Deprecated, automatic in modern GHC
  Cabal.NegativeLiterals              -> Just LangExt.NegativeLiterals
  Cabal.BinaryLiterals                -> Just LangExt.BinaryLiterals
  Cabal.NumDecimals                   -> Just LangExt.NumDecimals
  Cabal.NullaryTypeClasses            -> Nothing  -- Deprecated, use MultiParamTypeClasses
  Cabal.ExplicitNamespaces            -> Just LangExt.ExplicitNamespaces
  Cabal.AllowAmbiguousTypes           -> Just LangExt.AllowAmbiguousTypes
  Cabal.JavaScriptFFI                 -> Just LangExt.JavaScriptFFI
  Cabal.PatternSynonyms               -> Just LangExt.PatternSynonyms
  Cabal.PartialTypeSignatures         -> Just LangExt.PartialTypeSignatures
  Cabal.NamedWildCards                -> Just LangExt.NamedWildCards
  Cabal.DeriveAnyClass                -> Just LangExt.DeriveAnyClass
  Cabal.DeriveLift                    -> Just LangExt.DeriveLift
  Cabal.StaticPointers                -> Just LangExt.StaticPointers
  Cabal.StrictData                    -> Just LangExt.StrictData
  Cabal.Strict                        -> Just LangExt.Strict
  Cabal.ApplicativeDo                 -> Just LangExt.ApplicativeDo
  Cabal.DuplicateRecordFields         -> Just LangExt.DuplicateRecordFields
  Cabal.TypeApplications              -> Just LangExt.TypeApplications
  Cabal.TypeInType                    -> Just LangExt.TypeInType
  Cabal.UndecidableSuperClasses       -> Just LangExt.UndecidableSuperClasses
  Cabal.MonadFailDesugaring           -> Nothing  -- Transitional, no longer needed
  Cabal.TemplateHaskellQuotes         -> Just LangExt.TemplateHaskellQuotes
  Cabal.OverloadedLabels              -> Just LangExt.OverloadedLabels
  Cabal.TypeFamilyDependencies        -> Just LangExt.TypeFamilyDependencies
  Cabal.DerivingStrategies            -> Just LangExt.DerivingStrategies
  Cabal.DerivingVia                   -> Just LangExt.DerivingVia
  Cabal.UnboxedSums                   -> Just LangExt.UnboxedSums
  Cabal.HexFloatLiterals              -> Just LangExt.HexFloatLiterals
  Cabal.BlockArguments                -> Just LangExt.BlockArguments
  Cabal.NumericUnderscores            -> Just LangExt.NumericUnderscores
  Cabal.QuantifiedConstraints         -> Just LangExt.QuantifiedConstraints
  Cabal.StarIsType                    -> Just LangExt.StarIsType
  Cabal.EmptyDataDeriving             -> Just LangExt.EmptyDataDeriving
  Cabal.CUSKs                         -> Just LangExt.CUSKs
  Cabal.ImportQualifiedPost           -> Just LangExt.ImportQualifiedPost
  Cabal.StandaloneKindSignatures      -> Just LangExt.StandaloneKindSignatures
  Cabal.UnliftedNewtypes              -> Just LangExt.UnliftedNewtypes
  Cabal.LexicalNegation               -> Just LangExt.LexicalNegation
  Cabal.QualifiedDo                   -> Just LangExt.QualifiedDo
  Cabal.LinearTypes                   -> Just LangExt.LinearTypes
  Cabal.FieldSelectors                -> Just LangExt.FieldSelectors
  Cabal.OverloadedRecordDot           -> Just LangExt.OverloadedRecordDot
  Cabal.UnliftedDatatypes             -> Just LangExt.UnliftedDatatypes


-- Find which cabal config a file belongs to based on file path
findCabalForFile :: FilePath -> [CabalConfig] -> Maybe CabalConfig
findCabalForFile filePath cabalConfigs =
    -- Normalize the file path by removing leading "./" if present
    let normalizedFilePath = case stripPrefix "./" filePath of
                                Just p -> p
                                Nothing -> filePath
        
        matchesCabal cfg = 
            let basePath = case stripPrefix "./" (cabalBasePath cfg) of
                            Just p -> p
                            Nothing -> cabalBasePath cfg
                -- Check multiple matching strategies:
                -- 1. basePath is a prefix of the file path
                -- 2. Any source directory is a prefix of the file path  
                -- 3. The file path contains the basePath somewhere in it
            in basePath `isPrefixOf` normalizedFilePath ||
               any (\srcDir -> 
                    let normalizedSrcDir = case stripPrefix "./" srcDir of
                                            Just p -> p
                                            Nothing -> srcDir
                    in normalizedSrcDir `isPrefixOf` normalizedFilePath
                   ) (cabalSourceDirs cfg) ||
               -- Also check if basePath is contained in the file path (for nested projects)
               ("/" ++ basePath ++ "/") `isInfixOf` ("/" ++ normalizedFilePath)
               
    in find matchesCabal cabalConfigs

-- | Apply all implied extension flags based on currently enabled extensions
applyImpliedExtensions :: DynFlags -> DynFlags
applyImpliedExtensions dflags =
    let enabledExts = EnumSet.toList $ extensionFlags dflags
        newDflags = foldl' applyImplicationsForExt dflags enabledExts
    in newDflags
  where
    -- Apply implications for a single extension
    applyImplicationsForExt :: DynFlags -> LangExt.Extension -> DynFlags
    applyImplicationsForExt df ext =
        let implications = getImplications ext
        in foldl' applyImplication df implications

    -- Apply a single implication
    applyImplication :: DynFlags -> (GHC.Driver.Session.TurnOnFlag, LangExt.Extension) -> DynFlags
    applyImplication df (turnOnFlag, impliedExt) =
        case turnOnFlag of
            True  -> xopt_set df impliedExt
            False -> xopt_unset df impliedExt

-- | Get all extensions implied by a given extension
getImplications :: LangExt.Extension -> [(GHC.Driver.Session.TurnOnFlag, LangExt.Extension)]
getImplications ext =
    [(onOff, impliedExt) | (triggerExt, onOff, impliedExt) <- GHC.Driver.Session.impliedXFlags, triggerExt == ext]

-- | Apply all implied extensions (GHC handles transitive implications)
applyImpliedExtensionsComplete :: DynFlags -> DynFlags
applyImpliedExtensionsComplete = applyImpliedExtensions

-- | Convert Cabal.Language to GHC's Language
cabalLangToGhcLang :: Cabal.Language -> Language
cabalLangToGhcLang Cabal.Haskell98 = Haskell98
cabalLangToGhcLang Cabal.Haskell2010 = Haskell2010
cabalLangToGhcLang Cabal.GHC2021 = GHC2021
cabalLangToGhcLang _ = GHC2021  -- Fallback for unknown languages

initGhcFlagsWithCabal :: String -> CabalConfig -> [FilePath] -> Ghc DynFlags
initGhcFlagsWithCabal actualFilePath cabalConfig extraDirs = do
    let defaultLang = maybe GHC2021 cabalLangToGhcLang (cabalDefaultLanguage cabalConfig)
        enabledCabalExts = ((map (\x -> (Just x,True)) $ languageExtensions (Just defaultLang))) <> (map cabalExtToGhcExt $ (cabalExtensions) cabalConfig)
    dflags''' <- (\x -> foldl' (\acc (mExt,onOrOff) -> 
                                  case mExt of 
                                    Just ext -> if onOrOff then xopt_set acc ext else xopt_unset acc ext
                                    Nothing -> acc) x enabledCabalExts) <$> getSessionDynFlags
    src_opts <- liftIO $ getOptionsFromFile dflags''' actualFilePath
    (dflags', leftovers, warns) <- parseDynamicFilePragma dflags''' src_opts
    _ <- setSessionDynFlags $ applyImpliedExtensionsComplete $ (\x -> 
                                      x `gopt_set` Opt_KeepRawTokenStream
                                        `gopt_set` Opt_NoHsMain
                                        `gopt_set` Opt_DeferTypeErrors
                                        `gopt_set` Opt_DeferTypedHoles
                                        `gopt_set` Opt_DeferOutOfScopeVariables
                                        `gopt_unset` Opt_WarnIsError
                                        `gopt_set` Opt_SuppressUniques
                                        `wopt_unset` Opt_WarnTabs
                                  ) dflags'
    getSessionDynFlags

-- Extract module names from file paths
extractModuleNames :: [(FilePath, String)] -> [FilePath] -> [(String, String, String)]
extractModuleNames projectRoots filePaths =
    let haskellFiles = filter (\fp -> takeExtension fp == ".hs" || ".hs-boot" `isSuffixOf` fp) filePaths
    in filter (\(m, _,_) -> m /= "NA") (map extractModNameAndPath haskellFiles)
    where
        extractModNameAndPath :: FilePath -> (String, String, String)
        extractModNameAndPath filePath = do
            let newPath =
                    if "euler-x" `isInfixOf` filePath 
                        then "euler-x/" 
                        else if "oltp" `isInfixOf` filePath 
                            then "oltp/" 
                        else if "dbTypes" `isInfixOf` filePath 
                            then "dbTypes/" 
                        else if "ecPrelude" `isInfixOf` filePath 
                            then "ecPrelude/"
                        else if "euler-api-decider" `isInfixOf` filePath 
                            then "euler-api-decider/"
                        else 
                            let res = catMaybes $ map (\(x,y) -> if y `isInfixOf` filePath then Just x else Nothing) projectRoots
                            in if length res > 0 then (head res) else ""
            case filePath =~ ("src-generated/(.*).hs" :: String) :: (String, String, String, [String]) of
                (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-generated",filePath)
                _                    ->
                    case filePath =~ (".*/src-generated/(.*).hs" :: String) :: (String, String, String, [String]) of
                        (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-generated",filePath)
                        _                    ->
                            case filePath =~ (".*src/(.*).hs" :: String) :: (String, String, String, [String]) of
                                (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src",filePath)
                                _                    -> 
                                    case filePath =~ (".*src-extras/(.*).hs" :: String) :: (String, String, String, [String]) of
                                        (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-extras",filePath)
                                        _                    -> ("NA", "NA",filePath)

useDirs :: [FilePath] -> Ghc ()
useDirs workingDirs = do
  dynflags <- getSessionDynFlags
  void $ setSessionDynFlags dynflags { importPaths = importPaths dynflags ++ workingDirs }

-- Parse module with Cabal configuration (preferred)
parseModuleWithCabal :: String -> CabalConfig -> FilePath -> String -> Ghc (Maybe ParsedModule)
parseModuleWithCabal actualFilePath cabalConfig modulePath moduleName = do
    useDirs [modulePath]
    dflags <- initGhcFlagsWithCabal actualFilePath cabalConfig [modulePath]
    eRes <- MC.try $ do
        target <- guessTarget moduleName Nothing
        setTargets [target]
        -- loadStatus <- load LoadAllTargets
        modGraph <- depanal [] False
        case find (\ms -> ms_mod_name ms == mkModuleName moduleName) (mgModSummaries modGraph) of
            Just modSum -> parseModule modSum
            Nothing -> do
                modSum <- getModSummary $ mkModuleName moduleName
                parseModule modSum
    case eRes of
      Left (err :: SomeException) -> do 
        liftIO $ print (err,actualFilePath)
        pure Nothing
      Right val -> pure $ Just val

-- Extract all declarations from a parsed module
getAllDecls :: ParsedModule -> [LHsDecl GhcPs]
getAllDecls pmod = 
  let parsedSource = pm_parsed_source pmod
      decls = hsmodDecls (unLoc parsedSource)
  in decls

-- Extract all functions from a parsed module
getAllFunctions :: ParsedModule -> [(String, (LHsDecl GhcPs))]
getAllFunctions pmod = 
  let decls = getAllDecls pmod
  in mapMaybe extractFunDecl decls
  where
    extractFunDecl :: LHsDecl GhcPs -> Maybe (String, LHsDecl GhcPs)
    extractFunDecl decl@(L _ (ValD _ bind)) = 
      case bind of
        FunBind{fun_id = L _ name} -> 
          Just (occNameString (occName name), decl)
        PatBind{pat_lhs = pat} -> 
          case getPatName pat of
            Just name -> Just (name, decl)
            Nothing -> Nothing
        _ -> Nothing
    extractFunDecl decl@(L _ (SigD _ sig)) =
      case sig of
        TypeSig _ ids _ -> 
          case ids of
            (L _ name:_) -> Just (occNameString (occName name), decl)
            _ -> Nothing
        _ -> Nothing
    extractFunDecl _ = Nothing
    
    getPatName :: LPat GhcPs -> Maybe String
    getPatName (L _ pat) = case pat of
      VarPat _ (L _ name) -> Just (occNameString (occName name))
      _ -> Nothing

-- Extract all type declarations from a parsed module
getAllTypeDecls :: ParsedModule -> [(String, (LHsDecl GhcPs))]
getAllTypeDecls pmod = 
  let decls = getAllDecls pmod
  in mapMaybe extractTypeDecl decls
  where
    extractTypeDecl :: LHsDecl GhcPs -> Maybe (String, LHsDecl GhcPs)
    extractTypeDecl d@(L _ decl) = case decl of
      TyClD _ (FamDecl _ fam) -> 
        case fam of
          FamilyDecl{fdLName = L _ name} -> 
            Just (occNameString (occName name), d)
      TyClD _ x -> case x of
        DataDecl{tcdLName = L _ name} -> 
          Just (occNameString (occName name), d)
        SynDecl{tcdLName = L _ name} -> 
          Just (occNameString (occName name), d)
        ClassDecl{tcdLName = L _ name} -> 
          Just (occNameString (occName name), d)
      _ -> Nothing

-- Extract all instance declarations from a parsed module
getAllInstances :: ParsedModule -> [(String, (LHsDecl GhcPs))]
getAllInstances pmod = 
  let decls = getAllDecls pmod
  in mapMaybe extractInstanceDecl decls
  where
    extractInstanceDecl :: LHsDecl GhcPs -> Maybe (String, LHsDecl GhcPs)
    extractInstanceDecl d@(L _ decl) = case decl of
      InstD _ (ClsInstD _ ClsInstDecl{cid_poly_ty = L _ typ}) -> 
        Just (showSDocUnsafe (ppr typ), d)
    --   InstD _ (DataFamInstD _ DataFamInstDecl{dfid_tycon = L _ name}) -> 
    --     Just (occNameString (occName name) ++ "_instance", d)
      InstD _ (TyFamInstD _ TyFamInstDecl{tfid_eqn = FamEqn{feqn_tycon = L _ name}}) -> 
        Just (occNameString (occName name) ++ "_instance", d)
      _ -> Nothing

-- Extract source location information
getSourceLocation :: SrcSpan -> SourceLocation
getSourceLocation srcSpan = case srcSpan of
  RealSrcSpan s _ -> 
    SourceLocation {
      startLine = srcSpanStartLine s,
      startCol = srcSpanStartCol s,
      endLine = srcSpanEndLine s,
      endCol = srcSpanEndCol s,
      fileName = unpackFS (srcSpanFile s)
    }
  UnhelpfulSpan _ -> 
    SourceLocation {
      startLine = 0,
      startCol = 0,
      endLine = 0,
      endCol = 0,
      fileName = "<unknown>"
    }

-- Extract function calls from a declaration
extractFunctionCalls :: HsDecl GhcPs -> [String]
extractFunctionCalls (ValD _ bind) = 
    let calls = bind ^? biplateRef :: [Name]
    in map (showSDocUnsafe . ppr) calls
extractFunctionCalls _ = []

-- extractCallsFromMatch :: Match GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromMatch (Match _ _ _ rhs) = extractCallsFromGRHS rhs

-- extractCallsFromGRHS :: GRHSs GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromGRHS (GRHSs _ grhss _) = concatMap extractCallsFromGRHS' grhss

-- extractCallsFromGRHS' :: LGRHS GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromGRHS' (L _ (GRHS _ _ expr)) = extractCallsFromExpr expr

-- extractCallsFromExpr :: LHsExpr GhcPs -> [String]
-- extractCallsFromExpr (L _ expr) = case expr of
--   HsVar _ (L _ name) -> [occNameString (occName name)]
--   HsApp _ f arg -> extractCallsFromExpr f ++ extractCallsFromExpr arg
--   OpApp _ l op r -> extractCallsFromExpr op ++ extractCallsFromExpr l ++ extractCallsFromExpr r
--   HsPar _ e -> extractCallsFromExpr e
--   HsLet _ _ e -> extractCallsFromExpr e
--   HsIf _ c t e -> extractCallsFromExpr c ++ extractCallsFromExpr t ++ extractCallsFromExpr e
--   HsCase _ e alts -> extractCallsFromExpr e ++ concatMap extractCallsFromAlt alts
--   _ -> []

-- extractCallsFromAlt :: LMatch GhcPs (LHsExpr GhcPs) -> [String]
-- extractCallsFromAlt (L _ (Match _ _ _ rhs)) = extractCallsFromGRHS rhs

-- Extract literals from a declaration
extractLiterals :: HsDecl GhcPs -> [(String, String)]
extractLiterals (ValD _ bind) = 
    let literals = bind ^? biplateRef :: [HsLit GhcPs]
    in map extractLit literals
extractLiterals _ = []

-- extractLitsFromMatch :: Match GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromMatch (Match _ _ _ rhs) = extractLitsFromGRHS rhs

-- extractLitsFromGRHS :: GRHSs GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromGRHS (GRHSs _ grhss _) = concatMap extractLitsFromGRHS' grhss

-- extractLitsFromGRHS' :: LGRHS GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromGRHS' (L _ (GRHS _ _ expr)) = extractLitsFromExpr expr

-- extractLitsFromExpr :: LHsExpr GhcPs -> [(String, String)]
-- extractLitsFromExpr (L _ expr) = case expr of
--   HsLit _ lit -> [extractLit lit]
--   HsApp _ f arg -> extractLitsFromExpr f ++ extractLitsFromExpr arg
--   OpApp _ l op r -> extractLitsFromExpr l ++ extractLitsFromExpr op ++ extractLitsFromExpr r
--   HsPar _ e -> extractLitsFromExpr e
--   HsLet _ _ e -> extractLitsFromExpr e
--   HsIf _ c t e -> extractLitsFromExpr c ++ extractLitsFromExpr t ++ extractLitsFromExpr e
--   HsCase _ e alts -> extractLitsFromExpr e ++ concatMap extractLitsFromAlt alts
--   _ -> []

-- extractLitsFromAlt :: LMatch GhcPs (LHsExpr GhcPs) -> [(String, String)]
-- extractLitsFromAlt (L _ (Match _ _ _ rhs)) = extractLitsFromGRHS rhs

extractLit :: HsLit GhcPs -> (String, String)
extractLit lit = case lit of
  HsChar _ c -> ("HsChar", [c])
  HsCharPrim _ c -> ("HsCharPrim", [c]) 
  HsString _ s -> ("HsString", show s)
  HsStringPrim _ s -> ("HsStringPrim", show s)
  HsInt _ i -> ("HsInt", show (i))
  HsIntPrim _ i -> ("HsIntPrim", show i)
  HsWordPrim _ w -> ("HsWordPrim", show w)
  HsFloatPrim _ f -> ("HsFloatPrim", show f)
  HsDoublePrim _ d -> ("HsDoublePrim", show d)
  _ -> ("UnknownLit", "")

-- Compare function calls between old and new versions
compareCalledFunctions :: SrcSpan -> SrcSpan -> HsDecl GhcPs -> HsDecl GhcPs -> CalledFunctionChanges
compareCalledFunctions oldl newl oldDecl newDecl =
  let oldCalls = extractFunctionCalls oldDecl
      newCalls = extractFunctionCalls newDecl
      added_functions = [call | call <- newCalls, call `notElem` oldCalls]
      removed_functions = [call | call <- oldCalls, call `notElem` newCalls]
      oldlits = extractLiterals oldDecl
      newlits = extractLiterals newDecl
      added_literals = [call | call <- newlits, call `notElem` oldlits]
      removed_literals = [call | call <- oldlits, call `notElem` newlits]
  in CalledFunctionChanges {
          added_functions = nub added_functions,
          removed_functions = nub removed_functions,
          added_literals = added_literals,
          removed_literals = removed_literals,
          old_function_src_loc = getSourceLocation oldl,
          new_function_src_loc = getSourceLocation newl
      }

-- Get declaration source code as a string
getDeclSourceCode :: (Outputable a) => a -> String
getDeclSourceCode decl = showSDocUnsafe (ppr decl)

-- Track parsing statistics
data ParseStats = ParseStats {
    totalFiles :: Int,
    successfullyParsed :: Int,
    skippedTemplateHaskell :: Int,
    skippedUnicodeSyntax :: Int,
    skippedGHCPlugin :: Int,
    skippedOtherErrors :: Int
} deriving (Show)

data SomeCompilerException = SomeCompilerException Text

instance Show SomeCompilerException where
    show (SomeCompilerException e) = show e

instance Exception SomeCompilerException

-- Process modules and track changes with Cabal configuration
processModuleSafe :: [CabalConfig] -> Bool -> String -> String -> String -> FilePath -> IO (String, Maybe ParsedModule, Bool)
processModuleSafe cabalConfigs isTried actualFilePath moduleName path localRepoPath = do
  let filePath = localRepoPath <> path
  -- First check if the file actually exists
  fileExists <- doesFileExist actualFilePath
  if not fileExists
    then pure (moduleName, Nothing, False)
    else do
      -- Find the specific cabal config for this file
      let maybeCabalConfig = findCabalForFile actualFilePath cabalConfigs
      result <- case maybeCabalConfig of
        Just cabalConfig -> do
          -- putStrLn $ "Using cabal config " ++ cabalPackageName cabalConfig ++ " for module " ++ moduleName
          try (runGhc (Just libdir) $ parseModuleWithCabal actualFilePath cabalConfig filePath moduleName) :: IO (Either SomeException (Maybe ParsedModule))
        Nothing -> do
          -- putStrLn $ "No cabal config found for " ++ moduleName ++ ", using default flags"
          pure $ Left $ toException $ SomeCompilerException ("cabal config not found" :: Text)
      
      case result of
        Right (Just val) -> pure (moduleName, Just val, True)
        Right Nothing -> pure (moduleName, Nothing, True)
        Left err -> do
          let errMsg = show err
          
          -- Categorize and handle different error types
          let errorType
                | "Perhaps you intended to use TemplateHaskell" `isInfixOf` errMsg = "TH"
                | "Could not find module 'Data.Record.Plugin" `isInfixOf` errMsg = "Plugin"
                | any (`isInfixOf` errMsg) ["parse error on input `→'", "parse error on input `∀'", 
                                           "parse error on input `←'", "parse error on input `∷'",
                                           "parse error on input `⇒'", "parse error on input `(#'"] = "Unicode"
                | "Lambda-syntax in pattern" `isInfixOf` errMsg = "LambdaPattern"
                | "Operator applied to too few arguments" `isInfixOf` errMsg = "Operator"
                | "Parse error in pattern" `isInfixOf` errMsg = "PatternError"
                | "lexical error in string/character literal" `isInfixOf` errMsg = "LexicalError"
                | "user interrupt" `isInfixOf` errMsg = "Interrupted"
                | otherwise = "Other"
          
          -- Only log detailed errors for unexpected cases
          when (errorType == "Other") $
            appendFile "error.log" (errMsg <> " " <> show filePath <> " " <> moduleName <> "\n")
          
          -- Log summary to console
          appendFile "parse_summary.log" (errorType <> "\t" <> moduleName <> "\n")
          
          pure (moduleName, Nothing, True)

-- Helper function to add a function to the modified list
addFunctionModified :: FunctionModified -> String -> FunctionModified
addFunctionModified (FunctionModified del mod add mn) name =
  FunctionModified del (name:mod) add mn

-- Helper function to add a function to the deleted list
addFunctionDeleted :: FunctionModified -> String -> FunctionModified
addFunctionDeleted (FunctionModified del mod add mn) name =
  FunctionModified (name:del) mod add mn

-- Get basic function modifications
getFunctionModifiedSimple :: HM.HashMap String (LHsDecl GhcPs)
                          -> HM.HashMap String (LHsDecl GhcPs)
                          -> [String]
                          -> String
                          -> FunctionModified
getFunctionModifiedSimple newFuns oldFuns removed moduleName = 
  let initialFunMod = FunctionModified [] [] removed moduleName
      result = HM.foldlWithKey (\acc k val ->
                  case HM.lookup k newFuns of
                      Just newVal -> if ((showSDocUnsafe $ ppr val) == (showSDocUnsafe $ ppr newVal)) 
                                    then acc 
                                    else addFunctionModified acc k
                      Nothing -> addFunctionDeleted acc k)
                initialFunMod oldFuns
  in result

-- Get granular function changes
getGranularChangeForFunctions :: [(String, HM.HashMap String (LHsDecl GhcPs), 
                                 HM.HashMap String (LHsDecl GhcPs))] -> IO ()
getGranularChangeForFunctions l = do
  listOfModifications <- mapM (\(moduleName, old, new) -> 
                              pure $ (moduleName, HM.fromList $ HM.foldlWithKey 
                                    (\acc k oldDecl@(L oldl oldDeclInner) ->
                                      case HM.lookup k new of
                                          Just newDecl@(L newl newDeclInner) -> 
                                              if ((showSDocUnsafe $ ppr oldDecl) == (showSDocUnsafe $ ppr newDecl)) 
                                              then acc 
                                              else acc ++ [(k, compareCalledFunctions (locA oldl) (locA newl) oldDeclInner newDeclInner)]
                                          Nothing -> acc)
                                    [] old)) l
  writeFile "function_changes_granular.json" 
      (BLU.toString $ encodePretty $ HM.fromList listOfModifications)

-- Collect all changes with code
getAllChangesWithCode :: HM.HashMap String (LHsDecl GhcPs)
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> [String]
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> [String]
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> HM.HashMap String (LHsDecl GhcPs)
                      -> [String]
                      -> String
                      -> DetailedChanges
getAllChangesWithCode newFuns oldFuns addedFns
                    newTypes oldTypes addedTypes
                    newInsts oldInsts addedInsts
                    m  =
  DetailedChanges {
      Diff.moduleName = m,
      -- Functions
      addedFunctions = [(name, getDeclSourceCode decl) | 
                      name <- addedFns, 
                      Just decl <- [HM.lookup name newFuns]],
                      
      modifiedFunctions = getModifiedDecls newFuns oldFuns,
      deletedFunctions = getDeletedDecls newFuns oldFuns,
      
      -- Types
      addedTypes = [(name, getDeclSourceCode decl) | 
                  name <- addedTypes, 
                  Just decl <- [HM.lookup name newTypes]],
                  
      modifiedTypes = getModifiedDecls newTypes oldTypes,
      deletedTypes = getDeletedDecls newTypes oldTypes,
      
      -- Instances
      addedInstances = [(name, getDeclSourceCode decl) | 
                      name <- addedInsts, 
                      Just decl <- [HM.lookup name newInsts]],
                      
      modifiedInstances = getModifiedDecls newInsts oldInsts,
      deletedInstances = getDeletedDecls newInsts oldInsts
  }
  where
    getModifiedDecls new old = HM.foldlWithKey (\acc k oldDecl ->
      case HM.lookup k new of
          Just newDecl -> 
              if ((showSDocUnsafe $ ppr oldDecl) == (showSDocUnsafe $ ppr newDecl)) 
              then acc 
              else acc ++ [(k, getDeclSourceCode oldDecl, getDeclSourceCode newDecl)]
          Nothing -> acc) [] old
          
    getDeletedDecls new old = 
      let deletedKeys = HM.keys $ HM.difference old new
      in [(k, getDeclSourceCode decl) | 
        k <- deletedKeys, 
        Just decl <- [HM.lookup k old]]

-- Create output files with changes
createCodeFiles :: [DetailedChanges] -> IO ()
createCodeFiles changes = do
  -- Write the pretty-printed detailed JSON
  writeFile "all_code_changes.json" 
      (BLU.toString $ encodePretty changes)
  
  -- Create separate files for functions, types, and instances
  let allFunctionChanges = object [
          "added" .= concatMap (\c -> map (\(name, code) -> 
                              object ["module" .= Diff.moduleName c, 
                                      "name" .= name, 
                                      "code" .= code]) 
                              (addedFunctions c)) changes,
          "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                  object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "oldCode" .= oldCode,
                                        "newCode" .= newCode]) 
                                  (modifiedFunctions c)) changes,
          "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "code" .= code]) 
                                (deletedFunctions c)) changes
          ]
      
  let allTypeChanges = object [
          "added" .= concatMap (\c -> map (\(name, code) -> 
                              object ["module" .= Diff.moduleName c, 
                                      "name" .= name, 
                                      "code" .= code]) 
                              (addedTypes c)) changes,
          "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                  object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "oldCode" .= oldCode,
                                        "newCode" .= newCode]) 
                                  (modifiedTypes c)) changes,
          "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "code" .= code]) 
                                (deletedTypes c)) changes
          ]
      
  let allInstanceChanges = object [
          "added" .= concatMap (\c -> map (\(name, code) -> 
                              object ["module" .= Diff.moduleName c, 
                                      "name" .= name, 
                                      "code" .= code]) 
                              (addedInstances c)) changes,
          "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                  object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "oldCode" .= oldCode,
                                        "newCode" .= newCode]) 
                                  (modifiedInstances c)) changes,
          "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                object ["module" .= Diff.moduleName c, 
                                        "name" .= name, 
                                        "code" .= code]) 
                                (deletedInstances c)) changes
          ]
  
  -- Write separate files for each type
  writeFile "function_changes.json" (BLU.toString $ encodePretty allFunctionChanges)
  writeFile "type_changes.json" (BLU.toString $ encodePretty allTypeChanges)
  writeFile "instance_changes.json" (BLU.toString $ encodePretty allInstanceChanges)

-- Main entry point
run :: IO ()
run = do
  x <- getArgs
  case x of
      [repoUrl, localRepoPath, branchName, currentCommit, path] -> do
          cloneRepo repoUrl localRepoPath
          
          -- Save the current commit/HEAD state before any operations
          setCurrentDirectory localRepoPath
          initialCommit <- readProcess "git" ["rev-parse", "HEAD"] ""
          let initialCommitHash = T.unpack $ T.stripEnd (T.pack initialCommit)
          putStrLn $ "Initial commit: " ++ initialCommitHash
          
          -- Use finally to ensure we always restore the initial commit
          flip finally (do
              putStrLn $ "Restoring to initial commit: " ++ initialCommitHash
              _ <- readProcess "git" ["checkout", initialCommitHash] ""
              pure ()) $ do
            cabalpaths <- findCabalFiles localRepoPath
            
            -- Parse all Cabal files to extract configuration
            putStrLn $ "Found " ++ show (length cabalpaths) ++ " cabal files"
            cabalConfigs <- parseAllCabalFiles cabalpaths
            putStrLn $ "Successfully parsed " ++ show (length cabalConfigs) ++ " cabal configurations"
            mapM_ (\cfg -> putStrLn $ "  - " ++ cabalPackageName cfg ++ 
                                     " (extensions: " ++ show (length (cabalExtensions cfg)) ++ 
                                     ", src dirs: " ++ show (cabalSourceDirs cfg) ++ ")") cabalConfigs
            
            changedFiles <- getChangedFiles branchName currentCommit localRepoPath
            let modifiedModsAndPaths = extractModuleNames (traceShowId $ generateProjectRoots cabalpaths) changedFiles
            -- print ("modified files: " <> show changedFiles)
            -- print ("modified files: " <> show modifiedModsAndPaths)
            
            -- Process modules for previous commit with Cabal config (sequentially to avoid file locking)
            maybePreviousAST <- mapM (\(m, p, fp) -> processModuleSafe cabalConfigs False fp m p localRepoPath) modifiedModsAndPaths
            
            -- Switch to current commit
            _ <- readProcess "git" ["checkout", currentCommit] ""
            
            -- Re-parse cabal files for current commit (in case they changed)
            cabalConfigsCurrent <- parseAllCabalFiles cabalpaths
            
            -- Process modules for current commit with Cabal config (sequentially to avoid file locking)
            maybeCurrentAST <- mapM (\(m, p, fp) -> processModuleSafe cabalConfigsCurrent False fp m p localRepoPath) modifiedModsAndPaths
            
            -- Pair up the results and separate into different categories
            let listOfAstTuple = zip maybePreviousAST maybeCurrentAST
            
            -- Separate modules into categories
            let (newModules, deletedModules, modifiedModules) = partitionModules listOfAstTuple
            
            -- Handle completely new modules (entire module as "added")
            newModuleChanges <- catMaybes <$> mapM (\(moduleName, x) -> 
                                            case x of 
                                              Just ast -> pure $ Just $ getEntireModuleAsChanges moduleName ast True
                                              Nothing -> do 
                                                print ("Module not found " ++ moduleName) 
                                                pure Nothing
                                    ) newModules
            
            -- Handle completely deleted modules (entire module as "deleted")  
            deletedModuleChanges <- catMaybes <$> mapM (\(moduleName, x) -> 
                                            case x of 
                                              Just ast -> pure $ Just $ getEntireModuleAsChanges moduleName ast True
                                              Nothing -> do 
                                                print ("Module not found " ++ moduleName) 
                                                pure Nothing
                                         ) deletedModules
            
            -- Handle modified modules (existing logic)
            modifiedChanges <- mapM (\((moduleName, mPreviousAST), (_, mCurrentAST)) -> do
                                      let currentFunctions = maybe HM.empty 
                                                          (HM.fromList . getAllFunctions) 
                                                          mCurrentAST
                                          previousFunctions = maybe HM.empty 
                                                            (HM.fromList . getAllFunctions) 
                                                            mPreviousAST
                                          currentTypes = maybe HM.empty 
                                                       (HM.fromList . getAllTypeDecls) 
                                                       mCurrentAST
                                          previousTypes = maybe HM.empty 
                                                        (HM.fromList . getAllTypeDecls) 
                                                        mPreviousAST
                                          currentInstances = maybe HM.empty 
                                                           (HM.fromList . getAllInstances) 
                                                           mCurrentAST
                                          previousInstances = maybe HM.empty 
                                                            (HM.fromList . getAllInstances) 
                                                            mPreviousAST
                                          addedFns = HM.keys $ HM.difference currentFunctions previousFunctions
                                          addedTypes = HM.keys $ HM.difference currentTypes previousTypes
                                          addedInsts = HM.keys $ HM.difference currentInstances previousInstances
                                      pure $ getAllChangesWithCode 
                                          currentFunctions previousFunctions addedFns
                                          currentTypes previousTypes addedTypes
                                          currentInstances previousInstances addedInsts
                                          moduleName) 
                               modifiedModules
            
            -- Combine all changes
            let allDetailedChanges = newModuleChanges ++ deletedModuleChanges ++ modifiedChanges
            
            -- Create output files
            createCodeFiles allDetailedChanges
            
            -- Also create the original function modification summary
            let functionModifications = map (\changes -> 
                                          let addedFns = map fst (addedFunctions changes)
                                              modifiedFns = map (\(name, _, _) -> name) (modifiedFunctions changes)
                                              deletedFns = map fst (deletedFunctions changes)
                                          in FunctionModified deletedFns modifiedFns addedFns (Diff.moduleName changes))
                                      allDetailedChanges
            
            writeFile "funs_modified.json" (BLU.toString $ encodePretty functionModifications)
            
            print "Processing complete. Check output files for details."
      _ -> fail $ "Can't proceed. Please pass all the arguments in the order of repoUrl localPath oldCommit newCommit path but got: " <> show x

partitionModules :: [((String, Maybe ParsedModule, Bool), (String, Maybe ParsedModule, Bool))] 
                 -> ([(String, Maybe ParsedModule)], [(String, Maybe ParsedModule)], [((String, Maybe ParsedModule), (String, Maybe ParsedModule))])
partitionModules astTuples = 
  let newModules = [(name, ast) | ((_, Nothing, False), (name, ast, True)) <- astTuples]
      deletedModules = [(name, ast) | ((name, ast, True), (_, Nothing, False)) <- astTuples]
      modifiedModules = [((oldName, oldAst), (newName, newAst)) | 
                        ((oldName, oldAst, True), (newName, newAst, True)) <- astTuples]
  in (newModules, deletedModules, modifiedModules)

getEntireModuleAsChanges :: String -> ParsedModule -> Bool -> DetailedChanges
getEntireModuleAsChanges moduleName pmod isAdded =
  let functions = getAllFunctions pmod
      types = getAllTypeDecls pmod
      instances = getAllInstances pmod
      
      functionData = [(name, getDeclSourceCode decl) | (name, decl) <- functions]
      typeData = [(name, getDeclSourceCode decl) | (name, decl) <- types]
      instanceData = [(name, getDeclSourceCode decl) | (name, decl) <- instances]
  in
  if isAdded
    then DetailedChanges {
        Diff.moduleName = moduleName,
        addedFunctions = functionData,
        modifiedFunctions = [],
        deletedFunctions = [],
        addedTypes = typeData,
        modifiedTypes = [],
        deletedTypes = [],
        addedInstances = instanceData,
        modifiedInstances = [],
        deletedInstances = []
    }
    else DetailedChanges {
        Diff.moduleName = moduleName,
        addedFunctions = [],
        modifiedFunctions = [],
        deletedFunctions = functionData,
        addedTypes = [],
        modifiedTypes = [],
        deletedTypes = typeData,
        addedInstances = [],
        modifiedInstances = [],
        deletedInstances = instanceData
    }
