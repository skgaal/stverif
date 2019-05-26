{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module PLCparser.IRGenState(StateIR(..), runMonadIR, halt, startLabel, endLabel) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

import qualified PLCparser.IntermediateRepresentation as IR
type Label = IR.Label
type Var = IR.Var

type LabelState = Either Integer (String, Integer)
type S = (LabelState, (Integer,Integer))
initState = (initLabel,initVar)
initLabel = Left 0
initVar = (0,0)

runMonadIR :: (MonadError String m) => StateT S (WriterT IR.Program m) () -> m IR.Program
runMonadIR m = execWriterT $ execStateT m initState

class MonadState S m => StateIR m where
  --getVar :: m Var
  nextVar :: IR.Typed a => a -> m Var
  nextVarI :: m Var
  nextVarR :: m Var
  getLabel :: m Label
  setLabel :: Label -> m ()
  nextLabel :: m Label
  peekLabel :: m Label
  peekLabelN :: Integer -> m Label

instance MonadState S m => StateIR m where
  --getVar = get >>= (return . var . snd)
  nextVar t = case IR.typeOf t of
    IR.T_Int  -> nextVarI
    IR.T_Real -> nextVarR
  nextVarI = do (l, (i, r)) <- get
                put (l, (i + 1, r))
                return $ IR.I_Var i
  nextVarR = do (l, (i, r)) <- get
                put (l, (i, r + 1))
                return $ IR.R_Var r
  getLabel = get >>= (return . label . fst)
  setLabel l_new = do (l, v) <- get
                      case l of
                        Left i      -> put (Right (l_new, i), v)
                        Right (_,i) -> put (Right (l_new, i), v)
  nextLabel = do (l, v) <- get
                 put (inc 1 l, v)
                 return $ label l
  peekLabel = peekLabelN 1
  peekLabelN n = do (l, v) <- get
                    return . label $ inc n l

label :: LabelState -> Label
label (Left i) = "l" ++ show i
label (Right (l,_)) = l
inc :: Integer -> LabelState -> LabelState
inc 0 l = l
inc n (Left i) = Left $ i + n
inc n (Right (l,i)) = Left $ i + n - 1
halt :: Monad m => m Label
halt = return "halt"
startLabel :: String -> Label
startLabel s = s ++ "_start"
endLabel :: String -> Label
endLabel s = ":" ++ s ++ "_end"
