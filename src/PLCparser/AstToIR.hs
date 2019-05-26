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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module PLCparser.AstToIR(astToIR, runMonadIR) where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Function((&))
import qualified Data.Map as M(lookup, union, fromList, keys)

import PLCparser.AbsPlc
import PLCparser.ComposOp
import PLCparser.PrintPlc(printTree)
import PLCparser.SymbolTable(SymbolTable, SymbolList, TypeInfo(..), SymbolInfo(..), lookUpReader)
import PLCparser.ParseConstant
import PLCparser.VariableLookUp(getFuncSymTab, lookUpVar, lookUpSymbolTable, lookUpSymbolList, lookUpFuncReturnType, appendAccessor)
import PLCparser.IRGenState(StateIR(..), runMonadIR, startLabel, endLabel)
import PLCparser.IRGenGotos(MonadIR(..))
import qualified PLCparser.IntermediateRepresentation as IR
type Program = IR.Program
type Instruction = IR.Instruction
type Label = IR.Label
type Var = IR.Var
type Value = IR.Value

sepChar = "__"


astToIR :: (MonadError String m, MonadIR m) => SymbolTable -> Tree a -> m ()
astToIR symtab tree = runReaderT (makeIR tree) symtab

makeIR :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Tree a -> m ()
makeIR (Prog_Decl name ds (FB_Body1 stmts)) = do
  setLabel $ startLabel (getProgName name)
  mapM_ (composOpM_ makeIR) ds
  sequentially compileStmt stmts >>= (endLabel (getProgName name) &)
makeIR (Action (Identifier name) (FB_Body1 stmts)) = do
  setLabel $ startLabel name
  sequentially compileStmt stmts >>= (endLabel name &)
makeIR (FB_Decl _ (Identifier name) _ _ _ _ _ (FB_Body1 stmts)) = do
  symbolInfo <- lookUpReader name
  case symbolInfo of
    (SymbolInfo (TFuncBlock _ symbolTable) _) -> local (M.union (M.fromList symbolTable)) (do
      setLabel (startLabel name)
      sequentially compileStmt stmts >>= (endLabel name &))
makeIR (Func_Decl_Body (Identifier name) (Func_Body1 stmts)) = do
  symbolInfo <- lookUpReader name
  case symbolInfo of
    (SymbolInfo (TFunc _ symbolTable) _) -> local (M.union (M.fromList symbolTable)) (do
      setLabel (startLabel name)
      sequentially compileStmt stmts >>= (endLabel name &))
makeIR node = composOpM_ makeIR node

getProgName :: Prog_Name -> String
getProgName Init_Prog   = "INIT"
getProgName Cyclic_Prog = "CYCLIC"
getProgName Exit_Prog   = "EXIT"
getProgName (Named_Prog (Identifier s)) = s

-- Handle statements
compileStmt :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Stmt -> m (Label -> m ())
compileStmt (Assign_Stmt v _ e) = do
  instructions <- execWriterT $ visitAssignStmt v e
  sequentially deferGoto instructions

compileStmt (Action_Call (Identifier s)) = do
  table <- ask
  case M.lookup s table of
    Just v -> skip -- throwError $ "Calling a symbol: " ++ s ++ " with info " ++ show v
    _ -> deferGoto $ IR.Call (startLabel s) (endLabel s)

compileStmt (If_Stmt c stms elsifs elseopt) = do
  v <- compileExpNoMut c
  k_if <- peekLabel >>= deferGoto . IR.If v
  k_end  <- sequentially compileStmt stms
  case (elsifs, elseopt) of
    ([],Else_None) -> return $ joinEnds [k_if, k_end]
    _ -> do
      k_if =<< getLabel
      k_ends <- mapM compileElseIf elsifs
      k_else <- compileElse elseopt
      return . joinEnds $ [k_end] ++ k_ends ++ [k_else]
  where
    compileElseIf :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Else_If -> m (Label -> m ())
    compileElseIf (Else_If c stms) = do
      v <- compileExpNoMut c
      k <- peekLabel >>= deferGoto . IR.If v
      k_end <- sequentially compileStmt stms
      k =<< getLabel
      return k_end

compileStmt (For_Stmt (Identifier v) fl stms) = do
  (i, v1, v2) <- compileForList fl
  writeInstruction $ IR.Eval (IR.Named_Var IR.T_Int v) IR.I_Assign v1
  l_loop <- peekLabel
  l_exit <- peekLabelN 2
  l_start <- peekLabelN 3
  k_loop <- deferGoto $ IR.Loop i l_start ('s':l_loop) ('s':l_exit)
  nextLabel >> nextLabel
  sequentially_ compileStmt stms
  deferGoto (IR.Eval (IR.Named_Var IR.T_Int v) IR.I_Add v2) >>= ('s':l_loop &)
  return k_loop
  where
    compileForList :: (MonadError String m, MonadReader SymbolTable m, StateIR m) => For_List -> m (Integer, IR.Rhs, IR.Rhs)
    compileForList (For_List_To from to) = do
      f <- getConstant from
      t <- getConstant to
      return (t - f + 1, IR.Int f, IR.Int 1)
    compileForList (For_List_To_By from to by) = do
      f <- getConstant from
      t <- getConstant to
      b <- getConstant by
      return (ceiling ((fromIntegral $ t - f + 1) / (fromIntegral b)), IR.Int f, IR.Int b)
--    compileForList fl = throwError $ "Cannot calculate amount of loops from: " ++ show fl

compileStmt (Case_Stmt e cs elseopt) = do
  v <- compileExpNoMut e
  l_case <- nextLabel
  temp <- mapM compileCase cs
  let (k_ends, vls) = unzip temp
      k_case = progLine l_case $ IR.Case v (concat vls) in do
    case elseopt of
      Else_None -> return . joinEnds $ [k_case] ++ k_ends
      _ -> do k_case =<< getLabel
              k_else <- compileElse elseopt
              return . joinEnds $ k_ends ++ [k_else]

compileStmt (Invocation_Stmt symVar params) = do
  (funcSymTab, f_var) <- getFuncSymTab symVar
  instructions <- execWriterT $ compileCallParams f_var funcSymTab params
  sequentially deferGoto instructions

compileStmt stmt = throwError $ "Cannot generate IR for " ++ show stmt

visitAssignStmt :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Variable -> Exp -> m ()
visitAssignStmt v e = do
  typeInfo <- lookUpVar v
  case (typeInfo, e) of
    (TArray it subs, Evaracc ev _) -> do
      e_typeInfo <- lookUpVar ev
      case (subs, e_typeInfo) of
        ([sub], TArray eit esubs) -> do
          (f, t) <- parseSubrange sub
          e_type <- typeToIRtype eit
          v_type <- typeToIRtype it
          forM_ [f..t] (\i -> makeTypedAssign (IR.Named_Var v_type ((getVarName v) ++ show i)) (IR.Var (IR.Named_Var e_type ((getVarName ev) ++ show i))))
        ([sub1, sub2], TArray eit esubs) -> do
          (f1, t1) <- parseSubrange sub1
          (f2, t2) <- parseSubrange sub2
          e_type <- typeToIRtype eit
          v_type <- typeToIRtype it
          forM_ [f1..t1] (\j -> forM_ [f2..t2] (\i -> makeTypedAssign (IR.Named_Var v_type ((getVarName v) ++ show j ++ sepChar ++ show i)) (IR.Var (IR.Named_Var e_type ((getVarName ev) ++ show j ++ sepChar ++ show i)))))
        _ -> throwError $ "Multidimentional (more than 2) array assignment not supported. For: " ++ show typeInfo
    (TStruct typeTable, Evaracc ev _) -> forM_ (M.keys typeTable) (\s -> visitAssignStmt (appendAccessor v s) (Evaracc (appendAccessor ev s) Multibit_Part_Access_Opt1))
    (TFuncBlock _ symbolTable, Evaracc ev _) -> forM_ (fst $ unzip symbolTable) (\s -> visitAssignStmt (appendAccessor v s) (Evaracc (appendAccessor ev s) Multibit_Part_Access_Opt1))
    (TAny, Evaracc ev _) -> do
      -- We model TAny assignment as integers, since it is most likely implemented with a pointer..
      makeTypedAssign (IR.Named_Var IR.T_Int (getVarName v)) (IR.Var (IR.Named_Var IR.T_Int (getVarName ev)))
    (TAnyNum, _) -> do
      v1 <- visitExpNoMut e
      makeTypedAssign (IR.Named_Var (IR.typeOf v1) (getVarName v)) v1
    (TAny, _) -> do
      v1 <- visitExpNoMut e
      makeTypedAssign (IR.Named_Var (IR.typeOf v1) (getVarName v)) v1
    _ -> do
      v1 <- visitExpNoMut e
      v2 <- compileVar v
      makeTypedAssign v2 v1
parseSubrange :: MonadError String m => Subrange -> m (Integer, Integer)
parseSubrange (From_To (Constant_Expr (Econst (Numeric_Literal from))) (Constant_Expr (Econst (Numeric_Literal to)))) = do
  f_rhs <- parseNumLit from
  f <- IR.rhsToInt f_rhs
  t_rhs <- parseNumLit to
  t <- IR.rhsToInt t_rhs
  return (f, t)
makeTypedAssign :: MonadWriter [Instruction] m => IR.Var -> IR.Rhs -> m ()
makeTypedAssign v2 v1 = tell [IR.Eval v2 (case (IR.typeOf v2, IR.typeOf v1) of
  (IR.T_Int,  IR.T_Int)  -> IR.I_Assign
  (IR.T_Real, IR.T_Real) -> IR.R_Assign
  (IR.T_Int,  IR.T_Real) -> IR.I_Assign_R
  (IR.T_Real, IR.T_Int)  -> IR.R_Assign_I) v1]

compileElse :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Else_Opt -> m (Label -> m ())
compileElse (Else stms) = sequentially compileStmt stms
compileElse Else_None = skip

compileCase :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Case_Selection -> m (Label -> m (), IR.Cases)
compileCase (Case_Selection elems stms) = do
  vals <- mapM compileCaseElem elems
  l <- getLabel
  k <- sequentially compileStmt stms
  return (k, [(v,l) | v <- vals])
compileCaseElem :: (MonadError String m, MonadReader SymbolTable m) => Case_List_Elem -> m Value
compileCaseElem (Case_Elem_Const_Expr (Constant_Expr e)) = getConstant e
compileCaseElem (Case_Elem_Subrange s) = throwError $ "Subrange in Switch Case is not yet supported: " ++ show s

getConstant :: (MonadError String m, MonadReader SymbolTable m) => Exp -> m Value
getConstant (Econst (Numeric_Literal n)) = parseNumLit n >>= IR.rhsToInt
getConstant (Evaracc (VariableSymbolic_Variable (Symbolic_Variable1 [Accessor (Identifier s)] [])) _) = do
  symbolinfo <- lookUpReader s
  case symbolinfo of
    SymbolInfo _ (Just (Constant_Expr (Econst (Numeric_Literal n)))) -> parseNumLit n >>= IR.rhsToInt
    _ -> throwError $ "Variable not a constant numeric literal: " ++ s ++ " of info " ++ show symbolinfo
getConstant c = throwError $ "Unknown constant expression not supported: " ++ show c

compileCallParams :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Symbolic_Variable -> SymbolList -> [Param_Assign] -> m ()
compileCallParams functionVar symTab params = do
  mapM_ (compileInParam functionVar) $ zip (fst $ unzip symTab) params
  let functionName = getVarName (VariableSymbolic_Variable functionVar) in do
    tell [IR.Call (startLabel functionName) (endLabel functionName)]
  mapM_ (compileOutParam functionVar symTab) params

compileInParam :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Symbolic_Variable -> (String, Param_Assign) -> m ()
compileInParam functionVar (in_var_name,(Param_Assign_Exp e)) = compileInParamHelp functionVar e in_var_name
compileInParam functionVar (s,(In_Param_Assign (Identifier in_var_name) e)) = compileInParamHelp functionVar e in_var_name
compileInParam _ _ = return ()
compileInParamHelp functionVar e in_var_name = visitAssignStmt (appendAccessor (VariableSymbolic_Variable functionVar) in_var_name) e

compileOutParam :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Symbolic_Variable -> SymbolList -> Param_Assign -> m ()
compileOutParam functionVar symTab (Out_Param_Assign (Identifier i) v) = compileOutParamHelp functionVar symTab i v IR.Assign
compileOutParam functionVar symTab (Negated_Out_Param_Assign (Identifier i) v) = compileOutParamHelp functionVar symTab i v IR.Not
compileOutParam _ _ _ = return ()
compileOutParamHelp functionVar symTab i v inst = do
  v1 <- compileVar v
  t_op <- IR.typeOperator inst (IR.typeOf v1)
  i_type <- lookUpSymbolList symTab i >>= typeToIRtype
  if (IR.typeOf v1) == i_type then
    tell [IR.Eval v1 t_op $ IR.Var $ IR.Named_Var i_type (getVarName (VariableSymbolic_Variable functionVar) ++ sepChar ++ i)]
  else
    throwError $ "Out parameter " ++ i ++ " has type " ++ show i_type ++ " but is assigned to variable " ++ show v1 ++ " of type " ++ show (IR.typeOf v1)

-- Handle expressions
compileExp :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Exp -> m Var
compileExp e = do
  rhs <- compileExp' False e
  case rhs of
    (IR.Var v) -> return v
    n -> throwError $ "Returns a const literal: " ++ show n ++ " from mutable compileExp: " ++ show e
compileExpNoMut :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Exp -> m IR.Rhs
compileExpNoMut e = compileExp' True e
compileExp' :: (MonadError String m, MonadReader SymbolTable m, MonadIR m) => Bool -> Exp -> m IR.Rhs
compileExp' b e = do
  (v, instructions) <- runWriterT (visitExp' b e)
  sequentially_ deferGoto instructions
  return v

visitExp :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Exp -> m IR.Var
visitExp e = do
  rhs <- visitExp' False e
  case rhs of
    (IR.Var v) -> return v
    n -> throwError $ "Returns a const literal: " ++ show n ++ " from mutable exp visit: " ++ show e
visitExpNoMut :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Exp -> m IR.Rhs
visitExpNoMut e = visitExp' True e
visitExp' :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Bool -> Exp -> m IR.Rhs
visitExp' _ (Eor    e1 e2) = visitBinaryOp e1 IR.Or e2
visitExp' _ (Exor   e1 e2) = visitBinaryOp e1 IR.Xor e2
visitExp' _ (Eand e1 _ e2) = visitBinaryOp e1 IR.And e2
visitExp' _ (Eplus  e1 e2) = visitBinaryOp e1 IR.Add e2
visitExp' _ (Eminus e1 e2) = visitBinaryOp e1 IR.Subtract e2
visitExp' _ (Etimes e1 e2) = visitBinaryOp e1 IR.Multiply e2
visitExp' _ (Ediv   e1 e2) = visitBinaryOp e1 IR.Divide e2
visitExp' _ (Eeq    e1 e2) = visitCompare e1 IR.Eq e2
visitExp' _ (Eneq   e1 e2) = visitCompare e1 IR.Neq e2
visitExp' _ (Elt    e1 e2) = visitCompare e1 IR.Less e2
visitExp' _ (Egt    e1 e2) = visitCompare e1 IR.Greater e2
visitExp' _ (Ele    e1 e2) = visitCompare e1 IR.LessEqual e2
visitExp' _ (Ege    e1 e2) = visitCompare e1 IR.GreaterEqual e2
--visitExp' _ (Emod   e1 e2) = visitBinaryOp e1 IR.Modulo e2
visitExp' _ (Epow   e1 (Econst (Numeric_Literal (Unsigned_Int_Literal (Unsigned_Int (UnsignedInt "2")))))) = do
  v1 <- visitExp e1
  t_op <- IR.typeOperator IR.Multiply v1
  tell [IR.Eval v1 t_op (IR.Var v1)]
  return $ IR.Var v1
--visitExp' _ (Epow   e1 e2) = visitBinaryOp e1 IR.Power e2
visitExp' _ (Euminus   e1) = do
  v2 <- visitExpNoMut e1
  v1 <- nextVar v2 -- use the type of v2
  case IR.typeOf v2 of
    IR.T_Int -> tell [IR.Eval v1 IR.I_Assign (IR.Int 0), IR.Eval v1 IR.I_Subtract v2]
    IR.T_Real -> tell [IR.Eval v1 IR.R_Assign (IR.Real 0.0), IR.Eval v1 IR.R_Subtract v2]
  return $ IR.Var v1
visitExp' b (Euplus    e1) = visitExp' b e1
visitExp' _ (Enot      e1) = do
  v2 <- visitExpNoMut e1
  if IR.typeOf v2 == IR.T_Int then do
    v1 <- nextVarI
    tell [IR.Eval v1 IR.I_Not v2]
    return $ IR.Var v1
  else
    throwError $ "Cannot apply the 'NOT' operator on real-valued: " ++ show v2
visitExp' False (Econst c) = do
  n <- parseConstant c
  v1 <- nextVar n
  n <- parseConstant c
  t_op <- IR.typeOperator IR.Assign n
  tell [IR.Eval v1 t_op n]
  return $ IR.Var v1
visitExp' True (Econst c) = do
  n <- parseConstant c
  return n
visitExp' False (Evaracc  v _) = do
  v2 <- compileVar v
  v1 <- nextVar v2
  t_op <- IR.typeOperator IR.Assign v2
  tell [IR.Eval v1 t_op (IR.Var v2)]
  return $ IR.Var v1
visitExp' True  (Evaracc  v _) = compileVar v >>= return . IR.Var
--visitExp' _ (Eenum i1 i2) =
visitExp' _ e@(Efunccall name params) =
  let symVar = Symbolic_Variable1 name []
      varName = getVarName (VariableSymbolic_Variable symVar) in do
        (funcSymTab, f_var) <- getFuncSymTab symVar
        compileCallParams f_var funcSymTab params
        returnTyp <- lookUpFuncReturnType symVar
        returnType <- case returnTyp of
          TAnyNum -> case (funcSymTab, params) of
            ((_,SymbolInfo TAnyNum _):_,(Param_Assign_Exp (Evaracc ev _)):_) -> lookUpVar ev >>= typeToIRtype
            _ -> throwError $ "Cannot find return type for ANY_NUM function: " ++ show e
          TAny -> case (funcSymTab, params) of
            ((_,SymbolInfo TAny _):_,(Param_Assign_Exp (Evaracc ev _)):_) -> lookUpVar ev >>= typeToIRtype
            _ -> throwError $ "Cannot find return type for ANY function: " ++ show e
          _ -> typeToIRtype returnTyp
        v1 <- nextVar returnType
        t_op <- IR.typeOperator IR.Assign returnType
        tell [IR.Eval v1 t_op (IR.Var (IR.Named_Var returnType varName))]
        return (IR.Var v1)
--visitExp' _ (Erefval ref_val) =
visitExp' _ e = throwError $ "Cannot make IR for expression: " ++ show e

visitBinaryOp :: (MonadError String m, MonadReader SymbolTable m, StateIR m, MonadWriter [Instruction] m) => Exp -> IR.UntypedOp -> Exp -> m IR.Rhs
visitBinaryOp e1 op e2 = do v1 <- visitExp e1
                            v2 <- visitExpNoMut e2
                            case (IR.typeOf v1, IR.typeOf v2) of
                              (IR.T_Int,  IR.T_Int)  -> tellOperatorTyped v1 v2 op IR.T_Int
                              (IR.T_Real, IR.T_Real) -> tellOperatorTyped v1 v2 op IR.T_Real
                              (IR.T_Int,  IR.T_Real) -> do  -- throwError $ "Incompatible operand types: int: " ++ show v1 ++ ", real: " ++ show v2 ++ " for operator: " ++ show op
                                vtemp <- nextVarR
                                tell [IR.Eval vtemp IR.R_Assign_I (IR.Var v1)] -- Maybe swap operands?
                                tellOperatorTyped vtemp v2 op IR.T_Real
                              (IR.T_Real, IR.T_Int)  -> do  -- throwError $ "Incompatible operand types: real: " ++ show v1 ++ ", int: " ++ show v2 ++ " for operator: " ++ show op
                                vtemp <- nextVarR
                                tell [IR.Eval vtemp IR.R_Assign_I v2]
                                tellOperatorTyped v1 (IR.Var vtemp) op IR.T_Real
tellOperatorTyped v1 v2 op typ = do
  t_op <- IR.typeOperator op typ
  tell [IR.Eval v1 t_op v2]
  return $ IR.Var v1

visitCompare :: (MonadError String m, MonadReader SymbolTable m , StateIR m, MonadWriter [Instruction] m) => Exp -> IR.UntypedCmp -> Exp -> m IR.Rhs
visitCompare e1 cmp e2 = do v1 <- visitExpNoMut e1
                            v2 <- visitExpNoMut e2
                            if IR.typeOf v1 == IR.typeOf v2 then tellCompareTyped v1 v2 cmp (IR.typeOf v1)
                            else
                              case (v1, v2) of
                                ((IR.Int i),_) -> tellCompareTyped (IR.Real $ fromIntegral i) v2 cmp IR.T_Real
                                (_,(IR.Int i)) -> tellCompareTyped v1 (IR.Real $ fromIntegral i) cmp IR.T_Real
                                _ -> throwError $ "Cannot compare " ++ show (IR.typeOf v1) ++ " with " ++ show (IR.typeOf v2) ++ ". In : " ++ show v1 ++ " " ++ show cmp ++ " " ++ show v2
tellCompareTyped v1 v2 cmp typ = do
  v3 <- nextVarI
  tell [IR.Compare v3 v1 (IR.typeComparator cmp typ) v2]
  return $ IR.Var v3

compileVar :: (MonadError String m, MonadReader SymbolTable m) => Variable -> m Var
compileVar v = do
  typeInfo <- lookUpVar v
  irType <- catchError (typeToIRtype typeInfo) (\e -> throwError $ e ++ ". Var: " ++ printTree v)
  return $ IR.Named_Var irType (getVarName v)
typeToIRtype :: MonadError String m => TypeInfo -> m IR.Type
typeToIRtype typeInfo =
  case typeInfo of
    TBool -> return IR.T_Int
    TInt  -> return IR.T_Int
    TReal -> return IR.T_Real
    TEnum t _ -> typeToIRtype t
    TFunc returnType _ -> typeToIRtype returnType
    TTime -> return IR.T_Int
    TString -> return IR.T_Int  -- WARNING: This is wrong!!!
    t -> throwError $ "Cannot convert type to IR-type: " ++ show t

-- Get the name of a variable.
-- TODO Handle array variables better!!!
getVarName :: Variable -> String
getVarName (VariableDirect_Variable (Direct_Variable (IQM_DV_Option s) ns)) = s ++ show ns
getVarName (VariableSymbolic_Variable (Symbolic_Variable1 as ms)) = accessorName as
getVarName (VariableSymbolic_Variable (Symbolic_Variable2 as ms ss)) = accessorName as
getVarName (VariableSymbolic_Variable (Symbolic_Variable3 as ms)) = "THIS." ++ accessorName as
getVarName (VariableSymbolic_Variable (Symbolic_Variable4 as ms ss)) = "THIS." ++ accessorName as

accessorName :: [Accessor] -> String
accessorName [] = ""
accessorName [Accessor (Identifier s)] = s
accessorName (Accessor (Identifier s):xs) = s ++ sepChar ++ accessorName xs
