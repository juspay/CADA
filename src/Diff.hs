{-# LANGUAGE BangPatterns,DeriveGeneric #-}

module Diff where

import Data.Aeson
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes,mapMaybe)
import Data.List
import qualified Data.Text as T
import GHC.Hs.Extension
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Parser.FlowChange (compareASTForFuns, getAllFunctions, addFunctionModifed, FunctionModified(..))
import Language.Haskell.Tools.Parser.ParseModule (moduleParser)
import Language.Haskell.Tools.AST.Ann
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Process
import Text.Regex.Posix
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List.Extra (splitOn)
import GHC.Generics (Generic)
import Data.Aeson.Encode.Pretty
import Language.Haskell.Tools.PrettyPrint
import Data.Text.Encoding
import Data.Generics.Uniplate.Data ()
import Control.Reference ((^.), (!~), biplateRef,(^?))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import System.IO (IOMode(WriteMode), hSetBuffering, BufferMode(BlockBuffering), hClose, openFile)
import Control.Concurrent.Async (mapConcurrently)

-- Helper function to write file with proper buffering
writeFileBuffered :: FilePath -> BL.ByteString -> IO ()
writeFileBuffered path contents = do
    handle <- openFile path WriteMode
    hSetBuffering handle (BlockBuffering (Just 8192)) -- 8KB buffer size
    BL.hPut handle contents
    hClose handle


--TODO: NEED TO GET FROM CABAL.PROJECT
determineNewPath :: String -> String
determineNewPath filePath
    | "euler-x" `isInfixOf` filePath = "euler-x/"
    | "oltp" `isInfixOf` filePath = "oltp/"
    | "dbTypes" `isInfixOf` filePath = "dbTypes/"
    | "ecPrelude" `isInfixOf` filePath = "ecPrelude/"
    | "euler-api-decider" `isInfixOf` filePath = "euler-api-decider/"
    | "app/alchemist" `isInfixOf` filePath = "app/alchemist/"
    | "app/provider-platform/dynamic-offer-driver-drainer" `isInfixOf` filePath = "app/provider-platform/dynamic-offer-driver-drainer/"
    | "app/beckn-cli" `isInfixOf` filePath = "app/beckn-cli/"
    | "app/provider-platform/dynamic-offer-driver-app/Main" `isInfixOf` filePath = "app/provider-platform/dynamic-offer-driver-app/Main/"
    | "app/provider-platform/dynamic-offer-driver-app/Allocator" `isInfixOf` filePath = "app/provider-platform/dynamic-offer-driver-app/Allocator/"
    | "app/rider-platform/rider-app-drainer" `isInfixOf` filePath = "app/rider-platform/rider-app-drainer/"
    | "app/rider-platform/rider-app/Main" `isInfixOf` filePath = "app/rider-platform/rider-app/Main/"
    | "app/rider-platform/rider-app/Scheduler" `isInfixOf` filePath = "app/rider-platform/rider-app/Scheduler/"
    | "app/rider-platform/rider-app/search-result-aggregator" `isInfixOf` filePath = "app/rider-platform/rider-app/search-result-aggregator/"
    | "app/safety-dashboard" `isInfixOf` filePath = "app/safety-dashboard/"
    | "app/mocks/sms" `isInfixOf` filePath = "app/mocks/sms/"
    | "app/mocks/fcm" `isInfixOf` filePath = "app/mocks/fcm/"
    | "app/dashboard/rider-dashboard" `isInfixOf` filePath = "app/dashboard/rider-dashboard/"
    | "app/dashboard/provider-dashboard" `isInfixOf` filePath = "app/dashboard/provider-dashboard/"
    | "app/dashboard/Lib" `isInfixOf` filePath = "app/dashboard/Lib/"
    | "app/dashboard/CommonAPIs" `isInfixOf` filePath = "app/dashboard/CommonAPIs/"
    | "app/example-service" `isInfixOf` filePath = "app/example-service/"
    | "app/special-zone" `isInfixOf` filePath = "app/special-zone/"
    | "app/sdk-event-pipeline" `isInfixOf` filePath = "app/sdk-event-pipeline/"
    | "app/mocks/rider-platform" `isInfixOf` filePath = "app/mocks/rider-platform/"
    | "app/kafka-consumers" `isInfixOf` filePath = "app/kafka-consumers/"
    | "app/mocks/idfy" `isInfixOf` filePath = "app/mocks/idfy/"
    | "app/mocks/google" `isInfixOf` filePath = "app/mocks/google/"
    | "app/rider-platform/public-transport-rider-platform/Main" `isInfixOf` filePath = "app/rider-platform/public-transport-rider-platform/Main/"
    | "app/rider-platform/public-transport-rider-platform/search-consumer" `isInfixOf` filePath = "app/rider-platform/public-transport-rider-platform/search-consumer/"
    | "app/mocks/public-transport-provider-platform" `isInfixOf` filePath = "app/mocks/public-transport-provider-platform/"
    | "app/utils/route-extractor" `isInfixOf` filePath = "app/utils/route-extractor/"
    | "app/utils/image-api-helper" `isInfixOf` filePath = "app/utils/image-api-helper/"
    | "lib/beckn-spec" `isInfixOf` filePath = "lib/beckn-spec/"
    | "lib/beckn-services" `isInfixOf` filePath = "lib/beckn-services/"
    | "lib/payment" `isInfixOf` filePath = "lib/payment/"
    | "lib/shared-services" `isInfixOf` filePath = "lib/shared-services/"
    | "lib/location-updates" `isInfixOf` filePath = "lib/location-updates/"
    | "lib/special-zone" `isInfixOf` filePath = "lib/special-zone/"
    | "lib/scheduler" `isInfixOf` filePath = "lib/scheduler/"
    | "lib/sessionizer-metrics" `isInfixOf` filePath = "lib/sessionizer-metrics/"
    | "lib/yudhishthira" `isInfixOf` filePath = "lib/yudhishthira/"
    | "test" `isInfixOf` filePath = "test/"
    | "lib/producer" `isInfixOf` filePath = "lib/producer/"
    | "lib/utils" `isInfixOf` filePath = "lib/utils/"
    | "lib/external" `isInfixOf` filePath = "lib/external/"
    | "lib/webhook" `isInfixOf` filePath = "lib/webhook/"
    | otherwise = ""

extractModuleNames :: [FilePath] -> [(String, String)]
extractModuleNames filePaths =
    filter (\(m, _) -> m /= "NA") (map (\x -> extractModNameAndPath x) filePaths)
    where
        extractModNameAndPath :: FilePath -> (String, String)
        extractModNameAndPath filePath = do
            let newPath = determineNewPath filePath
            case filePath =~ "src-generated/(.*).hs" :: (String, String, String, [String]) of
                (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-generated")
                _                    ->
                    case filePath =~ ".*/src-generated/(.*).hs" :: (String, String, String, [String]) of
                        (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-generated")
                        _                    ->
                            case filePath =~ ".*src/(.*).hs" :: (String, String, String, [String]) of
                                (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName,newPath ++ "src")
                                _                    -> 
                                    case filePath =~ ".*src-extras/(.*).hs" :: (String, String, String, [String]) of
                                        (_, _, _, [modName]) -> (map (\c -> if c == '/' then '.' else c) modName, newPath ++ "src-extras")
                                        _                    -> ("NA", "NA")

cloneRepo :: String -> FilePath -> IO ()
cloneRepo repoUrl localPath = do
    exists <- doesPathExist localPath
    if not exists
        then callCommand $ "git clone " <> repoUrl <> " " <> localPath
        else putStrLn "Repository already cloned."

getChangedFiles :: String -> String -> FilePath -> IO [FilePath]
getChangedFiles branchName newCommit localPath = do
    setCurrentDirectory localPath
    readProcess "git" ["checkout", branchName] ""
    commit <- readProcess "git" ["rev-parse", branchName] ""
    result <- readProcess "git" ["diff", "--name-only", (T.unpack $ T.stripEnd (T.pack commit)), newCommit] ""
    pure $ lines result

-- Function to get all declarations by type
getAllTypeDecls :: Ann AST.UModule (Dom GhcPs) SrcTemplateStage -> [(String, Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)]
getAllTypeDecls moduleAST = mapMaybe traverseOverTypeDecl (moduleAST ^? biplateRef)

-- Function to get all instances
getAllInstances :: Ann AST.UModule (Dom GhcPs) SrcTemplateStage -> [(String, Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)]
getAllInstances moduleAST = mapMaybe traverseOverInstance (moduleAST ^? biplateRef)

-- Extract type declaration name
traverseOverTypeDecl :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, Ann UDecl (Dom GhcPs) SrcTemplateStage)
traverseOverTypeDecl decl@(Ann _ d) = case d of
    UTypeDecl { _declHead = Ann _ (UDeclHead (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) } -> 
        Just (name, decl)
    UDataDecl { _declHead = Ann _ (UDeclHead (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) } -> 
        Just (name, decl)
    UClassDecl { _declHead = Ann _ (UDeclHead (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) } -> 
        Just (name, decl)
    UTypeFamilyDecl { _declTypeFamily = Ann _ tf } -> 
        case _tfHead tf of
            Ann _ (UDeclHead (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) -> Just (name, decl)
            _ -> Nothing
    _ -> Nothing

-- Extract instance name (class name + instance types)
traverseOverInstance :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, Ann UDecl (Dom GhcPs) SrcTemplateStage)
traverseOverInstance decl@(Ann _ d) = 
    case d of
        UInstDecl {} -> getInstanceDetails decl
        UDerivDecl {} -> getInstanceDetails decl
        UTypeInstDecl {} -> getInstanceDetails decl
        UDataInstDecl {} -> getInstanceDetails decl
        UGDataInstDecl {} -> getInstanceDetails decl
        _ -> Nothing
  where
    getInstanceDetails :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, Ann UDecl (Dom GhcPs) SrcTemplateStage)
    getInstanceDetails i@(Ann _ inst) =
        let className = extractClassName inst
            instanceId = extractTypeName inst
        in Just (className<>"::"<>instanceId, i)

    extractTypeName :: UDecl (Dom GhcPs) SrcTemplateStage -> String
    extractTypeName (UInstDecl _ (Ann _ rule) _) = getTypeNameFromRule rule
    extractTypeName (UDerivDecl _ _ (Ann _ rule)) = getTypeNameFromRule rule
    extractTypeName (UTypeInstDecl (Ann _ rule) _) = getTypeNameFromRule rule
    extractTypeName (UDataInstDecl _ (Ann _ rule) _ _) = getTypeNameFromRule rule
    extractTypeName (UGDataInstDecl _ (Ann _ rule) _ _) = getTypeNameFromRule rule
    extractTypeName _ = "UnknownType"

    getTypeNameFromRule :: UInstanceRule (Dom GhcPs) SrcTemplateStage -> String
    getTypeNameFromRule (UInstanceRule _ _ (Ann _ head')) = getTypeName head'

    getTypeName :: UInstanceHead (Dom GhcPs) SrcTemplateStage -> String
    getTypeName (UInstanceHeadCon _) = "Unknown" -- Just class, no applied type
    getTypeName (UInstanceHeadParen (Ann _ head')) = getTypeName head'
    getTypeName (UInstanceHeadInfix (Ann _ typ) _) = extractTypeString typ
    getTypeName (UInstanceHeadApp (Ann _ _) (Ann _ typ)) = extractTypeString typ
    getTypeName _ = "UnknownType"

    extractTypeString :: UType (Dom GhcPs) SrcTemplateStage -> String
    extractTypeString (UTyVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) = name
    extractTypeString (UTyApp (Ann _ t1) _) = extractTypeString t1
    extractTypeString (UTyParen (Ann _ t)) = extractTypeString t
    extractTypeString _ = "Complex"

    extractClassName :: UDecl (Dom GhcPs) SrcTemplateStage -> String
    extractClassName (UInstDecl _ (Ann _ rule) _) = getRuleName rule
    extractClassName (UDerivDecl _ _ (Ann _ rule)) = getRuleName rule
    extractClassName (UTypeInstDecl (Ann _ rule) _) = getRuleName rule
    extractClassName (UDataInstDecl _ (Ann _ rule) _ _) = getRuleName rule
    extractClassName (UGDataInstDecl _ (Ann _ rule) _ _) = getRuleName rule
    extractClassName _ = "UnknownInstance"
    
    getRuleName :: UInstanceRule (Dom GhcPs) SrcTemplateStage -> String
    getRuleName (UInstanceRule _ _ (Ann _ head')) = getHeadName head'
    
    getHeadName :: UInstanceHead (Dom GhcPs) SrcTemplateStage -> String
    getHeadName (UInstanceHeadCon (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) = name
    getHeadName (UInstanceHeadApp (Ann _ head') _) = getHeadName head'
    getHeadName (UInstanceHeadInfix _ (Ann _ (UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart name))))))) = name
    getHeadName (UInstanceHeadParen (Ann _ head')) = getHeadName head'
    getHeadName _ = "UnknownClass"
-- -- Extract instance name (class name + instance types)
-- traverseOverInstance :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Maybe (String, Ann UDecl (Dom GhcPs) SrcTemplateStage)
-- traverseOverInstance decl@(Ann _ d) =
--     case d of
--         UInstDecl { _declInstRule = Ann _ rule } -> do
--             case rule of
--                 UInstanceRule _ _ (Ann _ head') -> 
--                     case head' of
--                         UInstanceHeadCon (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart className)))))) ->
--                             Just (className ++ "_instance", decl)
--                         _ -> Nothing
--                 _ -> Nothing
--     _ -> Nothing

-- Update the run function to handle types and instances
run :: IO ()
run = do
    x <- getArgs
    case x of
        [repoUrl, localRepoPath, branchName, currentCommit, path] -> do
            cloneRepo repoUrl (localRepoPath)
            changedFiles <- getChangedFiles branchName currentCommit localRepoPath
            let modifiedModsAndPaths = extractModuleNames changedFiles
            print ("modified files: " <> show changedFiles)
            
            -- Get AST for previous commit
            maybePreviousAST  <- mkAst modifiedModsAndPaths localRepoPath
            
            -- Switch to current commit
            _                 <- readProcess "git" ["checkout", currentCommit] ""
            
            -- Get AST for current commit
            maybeCurrentAST   <- mkAst modifiedModsAndPaths localRepoPath
            
            -- Process differences for functions, types, and instances
            let listOfAstTuple = zip maybePreviousAST maybeCurrentAST
            listOfChanges <- mapM (\((moduleName, mPreviousAST), (_, mCurrentAST)) -> do
                                    let currentFunctions = maybe [] getAllFunctions mCurrentAST
                                        previousFunctions = maybe [] getAllFunctions mPreviousAST
                                        currentTypes = maybe [] getAllTypeDecls mCurrentAST
                                        previousTypes = maybe [] getAllTypeDecls mPreviousAST
                                        currentInstances = maybe [] getAllInstances mCurrentAST
                                        previousInstances = maybe [] getAllInstances mPreviousAST
                                    pure $ (moduleName, 
                                        HM.fromList currentFunctions, 
                                        HM.fromList previousFunctions,
                                        HM.fromList currentTypes,
                                        HM.fromList previousTypes,
                                        HM.fromList currentInstances,
                                        HM.fromList previousInstances)) 
                                 listOfAstTuple
                
                -- Process function changes for funs_modified.json (unchanged)
            let funChanges = map (\(moduleName, currentFns, previousFns, _, _, _, _) -> 
                                let addedFns = HM.keys $ HM.difference currentFns previousFns
                                in getFunctionModifiedSimple currentFns previousFns addedFns moduleName) 
                             listOfChanges
                
                result = toString $ encode funChanges
            
            -- Write original results file (unchanged)
            writeFile "funs_modified.json" result
            
            -- Process detailed changes for all declaration types
            let detailedChanges = map (\(moduleName, currentFns, previousFns, currentTypes, previousTypes, currentInsts, previousInsts) -> 
                                    let addedFns = HM.keys $ HM.difference currentFns previousFns
                                        addedTypes = HM.keys $ HM.difference currentTypes previousTypes
                                        addedInsts = HM.keys $ HM.difference currentInsts previousInsts
                                    in getAllChangesWithCode 
                                        currentFns previousFns addedFns
                                        currentTypes previousTypes addedTypes
                                        currentInsts previousInsts addedInsts
                                        moduleName) 
                                 listOfChanges
            
            -- Create code files with detailed changes
            createCodeFiles detailedChanges
            
            print "Processing complete. Check output files for details."
            pure ()
        _ -> fail $ "can't proceed please pass all the arguments in the order of repoUrl localPath oldCommit newCommit but got: " <> show x
    where
        mkAst :: [(String, String)] -> FilePath -> IO [(String, (Maybe (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))]
        mkAst modifiedModsAndPaths localRepoPath =
            mapM (\(m, p) -> mkModuleNameAndAstTuple m p localRepoPath) modifiedModsAndPaths

        mkModuleNameAndAstTuple :: String -> String -> FilePath -> IO (String, (Maybe (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        mkModuleNameAndAstTuple moduleName path localRepoPath = do
            ast <- processFile moduleName path localRepoPath
            pure (moduleName, ast)

        processFile :: String -> String -> FilePath -> IO (Maybe ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        processFile moduleName path localRepoPath = do
            result <- try (moduleParser (localRepoPath <> path) moduleName) :: IO (Either SomeException ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
            print $ localRepoPath <> path
            case result of
                Right val -> pure $ Just val
                Left err  -> do
                    print ("Error Parsing module. Error is " <> show err)
                    appendFile "error.log" (show err <> " " <> show (localRepoPath <> path) <> " " <> moduleName <> "\n")
                    pure Nothing

        getFunctionModifiedSimple :: (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) 
                                -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) 
                                -> [String] 
                                -> String 
                                -> FunctionModified
        getFunctionModifiedSimple newFuns oldFuns removed moduleName = do
            let !y = HM.foldlWithKey (\acc k val ->
                        case HM.lookup k newFuns of
                            Just newVal -> if ((show val) == (show newVal)) then acc else addFunctionModifed acc (FunctionModified [] [k] [] moduleName)
                            Nothing -> addFunctionModifed acc (FunctionModified [k] [] [] moduleName) ) (FunctionModified [] [] removed moduleName) oldFuns
            y


-- Enhanced data type to capture all declaration types
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

-- Function to collect all changes with code
getAllChangesWithCode :: (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) 
                     -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage))
                     -> [String]
                     -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage))
                     -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage))
                     -> [String]
                     -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage))
                     -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage))
                     -> [String]
                     -> String
                     -> DetailedChanges
getAllChangesWithCode newFuns oldFuns addedFns
                     newTypes oldTypes addedTypes
                     newInsts oldInsts addedInsts
                     moduleName =
    DetailedChanges {
        moduleName = moduleName,
        -- Functions
        addedFunctions = [(name, getDeclSourceCode decl) | name <- addedFns, Just decl <- [HM.lookup name newFuns]],
        modifiedFunctions = getModifiedDecls newFuns oldFuns,
        deletedFunctions = getDeletedDecls newFuns oldFuns,
        -- Types
        addedTypes = [(name, getDeclSourceCode decl) | name <- addedTypes, Just decl <- [HM.lookup name newTypes]],
        modifiedTypes = getModifiedDecls newTypes oldTypes,
        deletedTypes = getDeletedDecls newTypes oldTypes,
        -- Instances
        addedInstances = [(name, getDeclSourceCode decl) | name <- addedInsts, Just decl <- [HM.lookup name newInsts]],
        modifiedInstances = getModifiedDecls newInsts oldInsts,
        deletedInstances = getDeletedDecls newInsts oldInsts
    }
  where
    getModifiedDecls new old = HM.foldlWithKey (\acc k oldDecl ->
        case HM.lookup k new of
            Just newDecl -> if ((show oldDecl) == (show newDecl)) 
                           then acc 
                           else acc ++ [(k, getDeclSourceCode oldDecl, getDeclSourceCode newDecl)]
            Nothing -> acc
        ) [] old
        
    getDeletedDecls new old = 
        -- Find keys that exist in old but not in new
        let deletedKeys = HM.keys $ HM.difference old new
        in [(k, getDeclSourceCode decl) | k <- deletedKeys, Just decl <- [HM.lookup k old]]

-- Updated function to create detailed files with optimized writing
createCodeFiles :: [DetailedChanges] -> IO ()
createCodeFiles changes = do
    -- Create all JSON values first (compute once, reuse multiple times)
    let allChangesJson = encodePretty changes
    
    let functionChangesJson = encodePretty $ object [
            T.pack "added" .= concatMap (\c -> map (\(name, code) -> 
                                     object [T.pack "module" .= moduleName c, 
                                            T.pack "name" .= name, 
                                            T.pack "code" .= code]) 
                                     (addedFunctions c)) changes,
            T.pack "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                        object [T.pack "module" .= moduleName c, 
                                               T.pack "name" .= name, 
                                               T.pack "oldCode" .= oldCode,
                                               T.pack "newCode" .= newCode]) 
                                        (modifiedFunctions c)) changes,
            T.pack "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                       object [T.pack "module" .= moduleName c, 
                                              T.pack "name" .= name, 
                                              T.pack "code" .= code]) 
                                       (deletedFunctions c)) changes
            ]
        
    let typeChangesJson = encodePretty $ object [
            T.pack "added" .= concatMap (\c -> map (\(name, code) -> 
                                     object [T.pack "module" .= moduleName c, 
                                            T.pack "name" .= name, 
                                            T.pack "code" .= code]) 
                                     (addedTypes c)) changes,
            T.pack "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                        object [T.pack "module" .= moduleName c, 
                                               T.pack "name" .= name, 
                                               T.pack "oldCode" .= oldCode,
                                               T.pack "newCode" .= newCode]) 
                                        (modifiedTypes c)) changes,
            T.pack "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                       object [T.pack "module" .= moduleName c, 
                                              T.pack "name" .= name, 
                                              T.pack "code" .= code]) 
                                       (deletedTypes c)) changes
            ]
        
    let instanceChangesJson = encodePretty $ object [
            T.pack "added" .= concatMap (\c -> map (\(name, code) -> 
                                     object [T.pack "module" .= moduleName c, 
                                            T.pack "name" .= name, 
                                            T.pack "code" .= code]) 
                                     (addedInstances c)) changes,
            T.pack "modified" .= concatMap (\c -> map (\(name, oldCode, newCode) -> 
                                        object [T.pack "module" .= moduleName c, 
                                               T.pack "name" .= name, 
                                               T.pack "oldCode" .= oldCode,
                                               T.pack "newCode" .= newCode]) 
                                        (modifiedInstances c)) changes,
            T.pack "deleted" .= concatMap (\c -> map (\(name, code) -> 
                                       object [T.pack "module" .= moduleName c, 
                                              T.pack "name" .= name, 
                                              T.pack "code" .= code]) 
                                       (deletedInstances c)) changes
            ]
    
    -- Write files concurrently
    mapConcurrently_ (\(filepath, content) -> writeFileBuffered filepath content)
        [ ("all_code_changes.json", allChangesJson)
        , ("function_changes.json", functionChangesJson)
        , ("type_changes.json", typeChangesJson)
        , ("instance_changes.json", instanceChangesJson)
        ]
    
    where
        -- Helper function for concurrent writes without collecting results
        mapConcurrently_ f = void . mapConcurrently f

write :: FilePath -> BL.ByteString -> IO ()
write path contents = writeFileBuffered path contents

getDeclSourceCode :: Ann AST.UDecl (Dom GhcPs) SrcTemplateStage -> String
getDeclSourceCode decl = prettyPrint decl