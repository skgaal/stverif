
--  This file is part of stverif
--
--  stverif is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  stverif is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with stverif.  If not, see <https://www.gnu.org/licenses/>.
{-# LANGUAGE FlexibleContexts #-}

module PLCparser.Parsing(parseFile, Verbosity) where

import System.IO
import Data.Maybe(fromJust)
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad (when, ap)

import PLCparser.LexPlc
import PLCparser.ParPlc
import PLCparser.PrintPlc
import PLCparser.AbsPlc
import PLCparser.ErrM


type ParseFun a = [[Token]] -> (GLRResult, GLR_Output (Err a))
type Verbosity = Integer

parseFile :: (MonadIO m, MonadError String m) => Verbosity -> FilePath -> m Entrypoint
parseFile v f = do
  liftIO $ when (v > 3) $ putStrLn f
  --content <- liftIO $ readFile f
  h <- liftIO $ openFile f ReadMode
  liftIO $ hSetEncoding h latin1
  content <- liftIO $ hGetContents h
  parse v content

parse :: MonadError String m => Verbosity -> String -> m Entrypoint
parse v s
 = let ts = map (:[]) $ myLexer s
       (raw_output, simple_output) = the_parser ts in
   case simple_output of
     GLR_Fail major minor -> do
                               throwError $ if v > 1 then major ++ minor else major
     GLR_Result df trees  -> do
                               case trees of
                                 []       -> throwError "No results but parse succeeded?"
                                 [Ok ast] -> return ast
                                 xs@(_:_) -> throwError "More than one possible parse tree."

the_parser :: ParseFun Entrypoint
the_parser = lift_parser pEntrypoint

type Forest = M.Map ForestId [Branch]      -- omitted in ParX export.
data GLR_Output a
 = GLR_Result { pruned_decode     :: (Forest -> Forest) -> [a]
              , semantic_result   :: [a]
              }
 | GLR_Fail   { main_message :: String
              , extra_info   :: String
              }

lift_parser :: (TreeDecode a, Show a, Print a)
 => ([[Token]] -> GLRResult) -> ParseFun a
lift_parser parser ts
 = let result = parser ts in
   (\o -> (result, o)) $
   case result of
     ParseError ts f -> GLR_Fail "Parse failed, unexpected token(s)\n"
                                 ("Tokens: " ++ show ts)
     ParseEOF   f    -> GLR_Fail "Parse failed, unexpected EOF\n"
                                 ("Partial forest:\n"
                                    ++ unlines (map show $ M.toList f))
     ParseOK r f     -> let find   f = fromJust . ((flip M.lookup) f)
                            dec_fn f = decode (find f) r
                        in GLR_Result (\ff -> dec_fn $ ff f) (dec_fn f)
