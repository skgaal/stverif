
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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PLCparser.IRGenGotos(MonadIR(..)) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Function((&))

import PLCparser.IRGenState(StateIR(nextLabel, getLabel))
import qualified PLCparser.IntermediateRepresentation as IR
type Program = IR.Program
type Instruction = IR.Instruction
type Label = IR.Label

-- Creates program lines and puts them in sequence.
class (StateIR m, MonadWriter Program m) => MonadIR m where
  deferGoto :: Instruction -> m (Label -> m ())
  progLine :: Label -> Instruction -> (Label -> m ())
  writeInstruction :: Instruction -> m ()
  sequentially  :: (a -> m (Label -> m ())) -> [a] -> m (Label -> m ())
  sequentially_ :: (a -> m (Label -> m ())) -> [a] -> m ()
  joinEnds :: [(Label -> m ())] -> (Label -> m ())
  skip :: m (Label -> m ())

instance (StateIR m, MonadWriter Program m) => MonadIR m where
  deferGoto i = do { l <- nextLabel ; return $ progLine l i }
  progLine l instruction goto = tell [IR.ProgLine l instruction goto]
  writeInstruction instruction = deferGoto instruction >=< getLabel
  sequentially _ [] = skip
  sequentially f xs = sequentially_ f (init xs) >> f (last xs)
  sequentially_ _ [] = return ()
  sequentially_ f (x:xs) = f x >=< getLabel >> sequentially_ f xs
  joinEnds fs l = mapM_ (l &) fs
  skip = return (\_ -> return ())

(>=<) :: Monad m => m (a -> m b) -> m a -> m b
a >=< b = a >>= (b >>=)
