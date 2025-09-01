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
import Language.Haskell.Tools.AST.Ann
import SrcLoc (SrcSpan(..), noSrcSpan,srcSpanStartLine,srcSpanStartCol,srcSpanEndLine,srcSpanEndCol,srcSpanFile)
import FastString (unpackFS)
extractModuleNames :: [FilePath] -> [(String, String)]
extractModuleNames filePaths =
    filter (\(m, _) -> m /= "NA") (map (\x -> extractModNameAndPath x) filePaths)
    where
        extractModNameAndPath :: FilePath -> (String, String)
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
                        else ""
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
            getGranularChangeForFunctions (map ((\(moduleName, currentFns, previousFns, _, _, _, _) -> (moduleName,currentFns,previousFns) )) listOfChanges)
            
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

        getGranularChangeForFunctions :: [(String,(HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)),HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage))] -> IO ()
        getGranularChangeForFunctions l = do
            listOfModifications <- mapM (\(moduleName,old,new) -> pure $ (moduleName,HM.fromList $ HM.foldlWithKey (\acc k oldDecl ->
                case HM.lookup k new of
                    Just newDecl -> if ((show oldDecl) == (show newDecl)) 
                                then acc 
                                else acc ++ [(k,compareCalledFunctions oldDecl newDecl)]
                    Nothing -> acc
                ) [] old)) l
            writeFile "function_changes_granular.json" (toString $ encodePretty $ HM.fromList listOfModifications)

        compareCalledFunctions :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> Ann UDecl (Dom GhcPs) SrcTemplateStage -> CalledFunctionChanges
        compareCalledFunctions oldDecl newDecl =
            let oldCalls = extractFunctionCalls oldDecl
                newCalls = extractFunctionCalls newDecl
                added_functions =
                    [ fn | (fn, newCount) <- Map.toList newCalls
                        , let oldCount = Map.findWithDefault 0 fn oldCalls
                        , newCount > oldCount ]
                removed_functions =
                    [ fn | (fn, oldCount) <- Map.toList oldCalls
                        , let newCount = Map.findWithDefault 0 fn newCalls
                        , oldCount > newCount ]
                oldlits = extractLiteralsCalls oldDecl
                newlits = extractLiteralsCalls newDecl
                added_literals = [call | call <- newlits, call `notElem` oldlits]
                removed_literals = [call | call <- oldlits, call `notElem` newlits]
            in CalledFunctionChanges {
                    added_functions = added_functions,
                    removed_functions = removed_functions,
                    added_literals = added_literals,
                    removed_literals = removed_literals,
                    old_function_src_loc = getSourceLocation oldDecl,
                    new_function_src_loc = getSourceLocation newDecl
                }

        -- | Extract literals from a declaration
        extractLiteralsCalls :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> [(String,String)]
        extractLiteralsCalls decl =
            let literals = decl ^? biplateRef :: [Ann ULiteral (Dom GhcPs) SrcTemplateStage]
            in nub ((concatMap extractLiterals literals))

        -- | Extract function calls from a declaration
        extractFunctionCalls :: Ann UDecl (Dom GhcPs) SrcTemplateStage -> [String]
        extractFunctionCalls decl =
            let exprs = decl ^? biplateRef :: [Ann UExpr (Dom GhcPs) SrcTemplateStage]
                names = decl ^? biplateRef :: [Ann UName (Dom GhcPs) SrcTemplateStage]
                calls = concatMap extractCallsFromExpr exprs <>(concatMap extractNames names)
            in Map.fromListWith (+) [(c, 1) | c <- calls]

        extractLiterals :: Ann ULiteral (Dom GhcPs) SrcTemplateStage -> [(String,String)]
        extractLiterals name =
            case name of           
                (Ann _ (UCharLit (lit))) -> [("UCharLit",[lit])]
                (Ann _ (UStringLit (lit))) -> [("UStringLit",lit)]
                (Ann _ (UIntLit (lit))) -> [("UIntLit",show lit)]
                (Ann _ (UFracLit (lit))) -> [("UFracLit",show lit)]
                (Ann _ (UPrimIntLit (lit))) -> [("UPrimIntLit",show lit)]
                (Ann _ (UPrimWordLit (lit))) -> [("UPrimWordLit",show lit)]
                (Ann _ (UPrimFloatLit (lit))) -> [("UPrimFloatLit",show lit)]
                (Ann _ (UPrimDoubleLit (lit))) -> [("UPrimDoubleLit",show lit)]
                (Ann _ (UPrimCharLit (lit))) -> [("UPrimCharLit",[lit])]
                (Ann _ (UPrimStringLit (lit))) -> [("UPrimStringLit",lit)]

        extractNames :: Ann UName (Dom GhcPs) SrcTemplateStage -> [String]
        extractNames name =
            case name of           
                (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> [name]
                (Ann _ (UParenName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> [name]
                (Ann _ (UImplicitName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> [name]

        -- | Extract function calls from an expression
        extractCallsFromExpr :: Ann UExpr (Dom GhcPs) SrcTemplateStage -> [String]
        extractCallsFromExpr (Ann _ expr) = case expr of
            UVar (Ann _ (UNormalName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> [name]
            UVar (Ann _ (UParenName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> [name]
            UVar (Ann _ (UImplicitName (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))))) -> [name]
            UApp func arg -> (extractCallsFromExpr func) <> (extractCallsFromExpr arg)
            UInfixApp l op r -> (extractOperatorName op) <> (extractCallsFromExpr l) <> (extractCallsFromExpr r)
            _ -> []

        -- | Extract operator name if possible
        extractOperatorName :: Ann UOperator (Dom GhcPs) SrcTemplateStage -> [String]
        extractOperatorName (Ann _ op) = case op of
            UNormalOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))) -> [name]
            UBacktickOp (Ann _ (UQualifiedName _ (Ann _ (UNamePart name)))) -> [name]

data SourceLocation = SourceLocation {
    startLine :: Int,
    startCol :: Int,
    endLine :: Int,
    endCol :: Int,
    fileName :: String
} deriving (Show, Generic)

instance ToJSON SourceLocation

-- | Extract source location from an AST node
getSourceLocation :: Ann e (Dom GhcPs) SrcTemplateStage -> SourceLocation
getSourceLocation (Ann annotation _) = 
    let srcInfo = _sourceInfo annotation
    in extractLocation srcInfo

-- | Extract location from source span info
extractLocation :: SpanInfo SrcTemplateStage -> SourceLocation
extractLocation x = 
    case getRange x of
        RealSrcSpan realSpan -> 
            SourceLocation {
                startLine = srcSpanStartLine realSpan,
                startCol = srcSpanStartCol realSpan,
                endLine = srcSpanEndLine realSpan,
                endCol = srcSpanEndCol realSpan,
                fileName = unpackFS (srcSpanFile realSpan)
            }
        UnhelpfulSpan _ -> 
            SourceLocation {
                startLine = 0,
                startCol = 0,
                endLine = 0,
                endCol = 0,
                fileName = "<unknown>"
            }

data CalledFunctionChanges = CalledFunctionChanges {
    added_functions :: [String],
    removed_functions :: [String],
    added_literals :: [(String,String)],
    removed_literals :: [(String,String)],
    old_function_src_loc :: SourceLocation,
    new_function_src_loc :: SourceLocation
    } deriving (Show, Generic)
instance ToJSON CalledFunctionChanges

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

-- Updated function to create detailed files
createCodeFiles :: [DetailedChanges] -> IO ()
createCodeFiles changes = do
    -- Write the pretty-printed detailed JSON
    writeFile "all_code_changes.json" 
        (toString $ encodePretty changes)
    
    -- Create separate files for functions, types, and instances
    let allFunctionChanges = object [
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
        
    let allTypeChanges = object [
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
        
    let allInstanceChanges = object [
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
    
    -- Write separate files for each type
    writeFile "function_changes.json" (toString $ encodePretty allFunctionChanges)
    writeFile "type_changes.json" (toString $ encodePretty allTypeChanges)
    writeFile "instance_changes.json" (toString $ encodePretty allInstanceChanges)

getDeclSourceCode :: Ann AST.UDecl (Dom GhcPs) SrcTemplateStage -> String
getDeclSourceCode decl = prettyPrint decl