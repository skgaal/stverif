{-# LANGUAGE FlexibleContexts #-}

module PLCparser.UnknownFunctions(addUnknownAndMakeMap, UnknownMap, KnownMap) where

import Control.Monad.State
import Data.List(nub, words)
import Data.Text(stripSuffix, pack, unpack)
import Data.Map(Map, fromList)
import System.Directory

import PLCparser.IntermediateRepresentation(Program, ProgLine(..), Instruction(..), Label)
import PLCparser.IRGenState(startLabel, endLabel)

--                map unknown     functionname
type UnknownMap = Map Instruction String   
--                map funcname time-interval
type KnownMap   = Map String   (Integer,Integer)

addUnknownAndMakeMap :: Program -> (Program, UnknownMap, IO (Maybe KnownMap))
addUnknownAndMakeMap program =
  let names = findUnknownFunctions program
      inst_names = zip (evalState (replicateM (length names) nextUnknown) 0) names
      new_lines = map (\(inst, name) -> ProgLine (startLabel name) inst (endLabel name)) inst_names in
    (program ++ new_lines, fromList inst_names, getUnknownFunctionsTable "unknownfunctions.txt")

findUnknownFunctions :: Program -> [String]
findUnknownFunctions program = map stripStartSuffix . filter isUnknown . nub . functionLabels $ map getInstruction program
  where
    stripStartSuffix label = case stripSuffix (pack $ startLabel "") (pack label) of
      Just t -> unpack t
      Nothing -> label
    isUnknown label = notElem label $ map (\(ProgLine startLabel _ _) -> startLabel) program
    getInstruction (ProgLine _ instruction _) = instruction

functionLabels :: [Instruction] -> [Label]
functionLabels intructions = map (\(Call ls le) -> ls) $ filter isCall intructions
  where
    isCall (Call _ _) = True
    isCall _ = False

nextUnknown :: MonadState Integer m => m Instruction
nextUnknown = next >>= return . Unknown
next :: Num a => MonadState a m => m a
next = do
  i <- get
  put $ i + 1
  return i

-- Parse Unknown Functions Timetable
-- Returns a maybe type, because if the doesn't exist, then Nothing'
getUnknownFunctionsTable :: FilePath -> IO (Maybe KnownMap)
getUnknownFunctionsTable file = do -- Simply just error handling
                                  fileHandle <- doesFileExist file
                                  case fileHandle of
                                    True  -> do
                                               a <- parseUnknownFunctionsTable file
                                               return $ Just a
                                    False -> return Nothing
-- Goes through the file line by line.
parseUnknownFunctionsTable :: FilePath -> IO (KnownMap)
parseUnknownFunctionsTable file = do
                                    lines <- readLines file
                                    return $ fromList $ map parseUnknownFunctionLine lines
-- Parses the individual line
parseUnknownFunctionLine :: String -> (String,(Integer,Integer))
parseUnknownFunctionLine line = let wds = words line in
                                  -- A line is structured like so: 'low high name'
                                  -- So, word index 2 is the name,
                                  --     word index 0 is the low
                                  --     word index 1 is the hight.
                                  (wds!!2
                                  ,(read $ wds!!0::Integer
                                  , read $ wds!!1::Integer) )
-- Helper function
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

