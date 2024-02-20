{-# LANGUAGE BangPatterns #-}

module Diff where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import Fdep.Group as FDep
import GHC.Hs.Extension
import qualified Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Parser.FlowChange (compareASTForFuns, getAllFunctions, addFunctionModifed, FunctionModified(..))
import Language.Haskell.Tools.Parser.ParseModule (moduleParser)
import Language.Haskell.Tools.AST.Ann
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Process
import Text.Regex.Posix

extractModuleName :: FilePath -> String
extractModuleName filePath =
    let (_, _, _, [moduleName]) = filePath =~ ".*src/(.*).hs" :: (String, String, String, [String])
    in map (\c -> if c == '/' then '.' else c) moduleName

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

checkoutToBranch :: String -> IO ()
checkoutToBranch branch = do
    let command = "git checkout " <> branch
    (_, _, _, process) <- createProcess (shell command) {std_out = CreatePipe}
    terminateProcess process

-- [(moduleName, cFuncs, pFuncs)]

-- [(moduleName, cFuncs, pFuncs, rFuncs)]

-- getFunctionModified
-- FunctionModified deleted modified removed moduleName

run :: IO ()
run = do
    x <- getArgs
    case x of
        [repoUrl, localRepoPath, branchName, currentCommit] -> do
            FDep.run
            cloneRepo repoUrl localRepoPath
            changedFiles <- getChangedFiles branchName currentCommit localRepoPath
            let modifiedFiles = filter (\x -> (".hs" `isSuffixOf` x)) changedFiles
                modifiedModules = map extractModuleName modifiedFiles
            print ("modified files: " <> show modifiedFiles)
            print ("modified modules: " <> show modifiedModules)
            checkoutToBranch currentCommit
            maybeCurrentAST   <- mkAst modifiedFiles localRepoPath
            checkoutToBranch branchName
            maybePreviousAST  <- mkAst modifiedFiles localRepoPath
            let listOfAstTuple = zip maybeCurrentAST maybePreviousAST
                listOfFunMod   = map (\((moduleName, mCurrentAST), (_, mPreviousAST)) -> (moduleName, getAllFunctions mCurrentAST, getAllFunctions mPreviousAST)) listOfAstTuple
                finalList      = map (\(moduleName, currentFns, previousFns) -> (moduleName, currentFns, previousFns, HM.keys $ HM.difference (HM.fromList currentFns) (HM.fromList previousFns))) listOfFunMod
                finalResult    = map (\(moduleName, currentFns, previousFns, removedFns) -> getFunctionModified (HM.fromList currentFns) (HM.fromList previousFns) removedFns moduleName) finalList
            print (show finalResult)
            pure ()
        _ -> fail $ "can't proceed please pass all the arguments in the order of repoUrl localPath oldCommit newCommit but got: " <> show x

    where
        mkAst :: [FilePath] -> FilePath -> IO [(String, (Maybe (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))]
        mkAst filePaths localRepoPath =
            mapConcurrently (\f -> mkModuleNameAndAstTuple f localRepoPath) filePaths

        mkModuleNameAndAstTuple :: FilePath -> FilePath -> IO (String, (Maybe (Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        mkModuleNameAndAstTuple filePath localRepoPath = do
            ast <- processFile filePath localRepoPath
            pure (extractModuleName filePath, ast)

        processFile :: FilePath -> FilePath -> IO (Maybe ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
        processFile filePath localRepoPath = do
            result <- try (moduleParser (localRepoPath <> "/src") (extractModuleName filePath)) :: IO (Either SomeException ((Ann AST.UModule (Dom GhcPs) SrcTemplateStage)))
            case result of
                Right val -> pure $ Just val
                Left err  -> do
                    print err
                    pure Nothing

        getFunctionModified :: (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) -> (HM.HashMap String (Ann AST.UDecl (Dom GhcPs) SrcTemplateStage)) -> [String] -> String -> FunctionModified
        getFunctionModified newFuns oldFuns added moduleName = do
            let !y = HM.foldlWithKey (\acc@(FunctionModified dx mx ax _) k val ->
                        case HM.lookup k newFuns of
                            Just newVal -> if (val == newVal) then acc else FunctionModified dx (k : mx) ax moduleName
                            Nothing -> FunctionModified (k : dx) mx ax moduleName) (FunctionModified [] [] added moduleName) oldFuns
            y
