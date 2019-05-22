
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
{-# LANGUAGE GADTs #-}

module PLCparser.VariableLookUp(getFuncSymTab, lookUpVar, lookUpSymVar, lookUpSymbolTable, lookUpSymbolList, lookUpFuncReturnType, appendAccessor) where

import Control.Monad.Reader
import Control.Monad.Except

import PLCparser.AbsPlc
import PLCparser.SymbolTable(SymbolTable, SymbolList, TypeInfo(..), SymbolInfo(..), lookUpTable, lookUpList, printType)

getFuncSymTab :: (MonadError String m, MonadReader SymbolTable m) => Symbolic_Variable -> m (SymbolList, Symbolic_Variable)
getFuncSymTab symVar = do
  typeInfo <- lookUpSymVar symVar
  case typeInfo of
    (TFunc returnTyp symTab) -> return (symTab, symVar)
    (TFuncBlock fb_name symTab) -> return (symTab, (Symbolic_Variable1 [(Accessor (Identifier fb_name))] []))
    t -> throwError $ "[getFuncSymTab]: Expected TFunc or TFuncBlock. Got: " ++ show t

lookUpVar :: (MonadError String m, MonadReader SymbolTable m) => Variable -> m TypeInfo
lookUpVar (VariableSymbolic_Variable symVar) = lookUpSymVar symVar
lookUpVar (VariableDirect_Variable (Direct_Variable (IQM_DV_Option s) ns)) = throwError $ "Cannot lookup typeInfo for direct_variable: " ++ s ++ show ns

lookUpSymVar :: (MonadError String m, MonadReader SymbolTable m) => Symbolic_Variable -> m TypeInfo
lookUpSymVar symVar = do
  symTab <- ask
  getTypeSymVar (lookUpSymbolTable symTab) symVar

lookUpFuncReturnType :: (MonadError String m, MonadReader SymbolTable m) => Symbolic_Variable -> m TypeInfo
lookUpFuncReturnType symVar = do
  funcType <- lookUpSymVar symVar
  case funcType of
    (TFunc returnType _) -> return returnType
    t -> throwError $ "Failed to lookup return type, because its a " ++ show t ++ "not a function: " ++ show symVar

lookUpSymbolTable :: MonadError String m => SymbolTable -> String -> m TypeInfo
lookUpSymbolTable table x = do
  symInfo <- lookUpTable table x
  case symInfo of
    (SymbolInfo typeInfo _) -> return typeInfo
lookUpSymbolList :: MonadError String m => SymbolList -> String -> m TypeInfo
lookUpSymbolList table x = do
  symInfo <- lookUpList table x
  case symInfo of
    (SymbolInfo typeInfo _) -> return typeInfo

getTypeSymVar :: MonadError String m => (String -> m TypeInfo) -> Symbolic_Variable -> m TypeInfo
getTypeSymVar getType (Symbolic_Variable1 as ms) = getTypeSymVarHelp getType as ms []
getTypeSymVar getType (Symbolic_Variable2 as ms ss) = getTypeSymVarHelp getType as ms ss
getTypeSymVar _ v = throwError $ "THIS. variables are not supported: " ++ show v
--getTypeSymVar getType (Symbolic_Variable3 as ms)) = getTypeSymVarHelp getType as ms []
--getTypeSymVar getType (Symbolic_Variable4 as ms ss)) = getTypeSymVarHelp getType as ms ss
getTypeSymVarHelp :: MonadError String m => (String -> m TypeInfo) -> [Accessor] -> [Multi_Elem_Var_Access] -> [Subscript_Access] -> m TypeInfo
getTypeSymVarHelp getType as ms ss = do
  typeInfo1 <- getTypeAccessors getType as
  typeInfo2 <- foldM getTypeMultiElem typeInfo1 ms
  getTypeSubscripts typeInfo2 ss

getTypeMultiElem :: MonadError String m => TypeInfo -> Multi_Elem_Var_Access -> m TypeInfo
getTypeMultiElem typeInfo (Subscript_Var_Access ss as) = getTypeSubscripts typeInfo ss >>= getInnerType as
getTypeMultiElem _ (Deref_Var_Access da) = throwError $ "Deref var access not supported: " ++ show da
getTypeMultiElem _ (More_Deref ds) = throwError $ "Deref var access not supported: " ++ show ds

getTypeSubscripts :: MonadError String m => TypeInfo -> [Subscript_Access] -> m TypeInfo
getTypeSubscripts typeInfo [] = return typeInfo
getTypeSubscripts typeInfo ((Subscript_Access subscripts):xs) =
  case typeInfo of
    TArray inner _ -> getTypeSubscripts inner xs
    _ -> throwError $ "Expected array type. Found: " ++ printType 2 typeInfo

getTypeAccessors :: MonadError String m => (String -> m TypeInfo) -> [Accessor] -> m TypeInfo
getTypeAccessors getType [(Accessor (Identifier x))] = getType x
getTypeAccessors getType ((Accessor (Identifier x)):xs) = getType x >>= getInnerType xs

getInnerType :: MonadError String m => [Accessor] -> TypeInfo -> m TypeInfo
getInnerType xs typeInfo = case typeInfo of
  TStruct typeTable -> getTypeAccessors (lookUpTable typeTable) xs
  TFuncBlock _ symbolTable -> getTypeAccessors (lookUpSymbolList symbolTable) xs
  TFunc _ symbolTable -> getTypeAccessors (lookUpSymbolList symbolTable) xs
  _ -> throwError $ "Expected struct, function or function block type in " ++ show xs ++ ". Found: " ++ printType 2 typeInfo

--appendAccessor :: MonadError String m => Variable -> Accessor -> m Variable
appendAccessor :: Variable -> String -> Variable
appendAccessor (VariableSymbolic_Variable (Symbolic_Variable1 as [])) s    = VariableSymbolic_Variable (Symbolic_Variable1 (as ++ [(Accessor (Identifier s))]) [])
appendAccessor (VariableSymbolic_Variable (Symbolic_Variable1 as ms)) s    = VariableSymbolic_Variable (Symbolic_Variable1 as (init ms ++ [appendAccessorHelp (last ms) (Accessor (Identifier s))]))
  where
    appendAccessorHelp :: Multi_Elem_Var_Access -> Accessor -> Multi_Elem_Var_Access
    appendAccessorHelp (Subscript_Var_Access ss as) a = Subscript_Var_Access ss (as ++ [a])
appendAccessor (VariableSymbolic_Variable (Symbolic_Variable2 as ms ss)) s = VariableSymbolic_Variable (Symbolic_Variable1 as (ms ++ [Subscript_Var_Access ss [(Accessor (Identifier s))]]))
--appendAccessor v a = throwError $ "Cannot append accessor " ++ show a ++ " to: " ++ show v
