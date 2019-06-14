{- 
	This file is part of stverif.

    stverif is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    stverif is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with stverif.  If not, see <https://www.gnu.org/licenses/>.
  -}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Data.Char( toLower )
import qualified Data.Map as M (empty, toList, lookup)
import Control.Monad.Except

import PLCparser.AbsPlc(Tree)
import PLCparser.Parsing as P(parseFile, Verbosity)
import PLCparser.SymbolTable(SymbolTable, Table, fillTypeTable, fillSymbolTable, resolveNames, printTypeTable, printSymbolTable)
import PLCparser.AstToIR(astToIR, runMonadIR)
import PLCparser.IntermediateRepresentation(Program, ProgLine(..), printProgram, showProgram)
import PLCparser.UnknownFunctions(addUnknownAndMakeMap, KnownMap, getUnknownFunctionsTable)
import PLCparser.IRToXml(makePetriNet)
import PLCparser.Reduction(performReduction)

import Data.Time.Clock.POSIX(getPOSIXTime)

compilerFunction :: (MonadIO m, MonadPlus m, MonadError String m) => P.Verbosity -> FilePath -> [FilePath] -> Integer -> Bool -> Bool -> Bool -> m ()
compilerFunction v o fs maxt t e i = do
  starttime    <- liftIO $ getPOSIXTime
  typeTable'   <- makeTableFromFiles fillTypeTable                 M.empty      v $ filter (\s -> getFileType s == TYP) fs
  typeTable''  <- makeTableFromFiles fillTypeTable                 typeTable'   v $ filter (\s -> getFileType s == FUN) fs
  symbolTable' <- makeTableFromFiles (fillSymbolTable typeTable'') M.empty      v $ filter (\s -> getFileType s == VAR) fs
  when (v==2) $ liftIO $ putStrLn "**** PRINTING TYPE TABLE ****"
  when (v==2) $ liftIO $ putStr $ printTypeTable 0 typeTable''
  when (v==2) $ liftIO $ putStrLn "**** PRINTING SYMBOL TABLE ****"
  when (v==2) $ liftIO $ putStr $ printSymbolTable 0 symbolTable'
  (typeTable, symbolTable)  <- resolveNames typeTable'' symbolTable'
  when (v>2) $ liftIO $ putStrLn "**** TYPE TABLE - WITH NAMES RESOLVED ****"
  when (v>2) $ liftIO $ putStr $ printTypeTable 0 typeTable
  when (v>2) $ liftIO $ putStrLn "**** SYMBOL TABLE - WITH NAMES RESOLVED ****"
  when (v>2) $ liftIO $ putStr $ printSymbolTable 0 symbolTable
  irProgram' <- makeIRFromFiles symbolTable v $ filter (\s -> getFileType s == ST) fs
  if not $ any (\(ProgLine l _ _) -> l == "CYCLIC_start") irProgram' then
    return ()
  else
    let (irProgram'', unknownmap') = addUnknownAndMakeMap irProgram' in do
      when (v>2) $ liftIO $ putStrLn "**** PRINTING IR PROGRAM ****"
      when (v>2) $ liftIO $ printProgram irProgram''
      when i $ liftIO $ writeFile (o ++ ".ir") $ showProgram irProgram''
      when (v>=1) $ liftIO $ putStrLn $ "\n**** Unknown Functions : " ++ o ++ " ****"
      maybemap' <- liftIO $ getUnknownFunctionsTable "unknownfunctions.txt"
      when (v>=1) $ liftIO $ putStr . unlines $ map (\(inst, name) -> show inst ++ " : " ++ name ++ getUknownTiming maybemap' name) $ M.toList unknownmap'
      let (irProgram, unknownmap, maybemap) = performReduction irProgram'' unknownmap' maybemap' in do
        liftIO $ writeFile (o ++ (if e then ".tapn" else ".xml")) $ makePetriNet e o maxt irProgram unknownmap maybemap
        endtime <- liftIO $ getPOSIXTime
        when t $ liftIO $ putStrLn $ o ++ " : " ++ show (endtime - starttime)
        return ()

getUknownTiming :: Maybe KnownMap -> String -> String
getUknownTiming Nothing _ = ""
getUknownTiming (Just map) name = let elem = M.lookup name map in
                                     case elem of
                                         Just a -> " - " ++ show a
                                         Nothing -> " - not in 'unknownfunctions.txt'"

makeIRFromFiles :: (MonadIO m, MonadError String m) => SymbolTable -> P.Verbosity -> [FilePath] -> m Program
makeIRFromFiles symbolTable v fs = runMonadIR $ mapM_ (\file -> do tree <- P.parseFile v file
                                                                   when (v==4) $ liftIO $ putStrLn $ show tree
                                                                   catchError (astToIR symbolTable tree) (\e -> throwError $ file ++ "\n" ++ e)) fs

makeTableFromFiles :: (MonadIO m, MonadError String m) =>
  (forall a. Table t -> Tree a -> m (Table t)) -> Table t -> P.Verbosity -> [FilePath] -> m (Table t)
makeTableFromFiles fillTableFunction initTable v fs = foldM (\table file -> do
                                                                tree <- P.parseFile v file
                                                                when (v==4) $ liftIO $ putStr $ show tree
                                                                catchError (fillTableFunction table tree) (\e -> throwError $ file ++ "\n" ++ e)) initTable fs

runCompiler :: MonadIO m => P.Verbosity -> FilePath -> [FilePath] -> Integer -> Bool -> Bool -> Bool -> m ()
runCompiler v o fs maxt t e i = do
  r <- runExceptT $ compilerFunction v o fs maxt t e i
  case r of
    Left w -> liftIO $ putStrLn $ w
    Right _ -> return ()

runMany :: P.Verbosity -> Integer -> Bool -> Bool -> Bool -> String -> IO ()
runMany v maxt t e i input = forM_ (lines input) $ (\line -> case words line of
                                                    (o:fs) -> runCompiler v o fs maxt t e i
                                                    [] -> runCompiler v "" [] maxt t e i)

data FileType = TYP | VAR | FUN | ST | UnknownFileType deriving (Ord, Eq, Show)

getFileType :: FilePath -> FileType
getFileType filename = getTypeHelper . map toLower . last $ wordsWhen (=='.') filename where
    getTypeHelper :: String -> FileType
    getTypeHelper "typ" = TYP
    getTypeHelper "var" = VAR
    getTypeHelper "fun" = FUN
    getTypeHelper "st"  = ST
    getTypeHelper _     = UnknownFileType
    wordsWhen     :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                          "" -> []
                          s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

-- Command line arguments --
data CompilePLC = CompilePLC
    {outfile :: FilePath
    ,verbosity_ :: Integer
    ,maxt :: Integer
    ,time :: Bool
    ,editor :: Bool
    ,generateIR :: Bool
    ,files :: [FilePath]
    }
    deriving (Show, Eq, Data, Typeable)
compileArgs = CompilePLC{
   outfile = def &= help "Output file for TAPN" &= opt "out.tapn" &= typ "FILENAME"
  ,verbosity_ = def &= help "Verbosity" &= opt "2"
  ,maxt = def &= help "Maximum execution time to generate for the query" &= opt "12500"
  ,time = def &= help "Timing of compilation"
  ,editor = def &= help "Produce tapaal editor friendly xml"
  ,generateIR = def &= help "Produce Intermediate Representation files"
  ,files = def &= args &= typ "FILES/DIRS"} &=
    help "Compile Structered Text programs into Timed-Arc Petri Net" &=
    summary "CompilePLC" &=
    details ["CompilePLC lets you transform ST programs into a formal model."
            ,"You can use this model for verification in the tool TAPAAL"]

main :: IO ()
main = do
  args <- cmdArgs compileArgs
  case args of
    CompilePLC o n m t e i [] -> getContents >>= runMany n m t e i
    CompilePLC "" n m t e i fs -> runCompiler n "out.tapn" fs m t e i
    CompilePLC o n m t e i fs -> runCompiler n o fs m t e i
