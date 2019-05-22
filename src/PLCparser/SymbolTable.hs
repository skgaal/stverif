
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
{-# LANGUAGE RankNTypes #-}

module PLCparser.SymbolTable where

import Data.Maybe(fromJust)
import Data.List(find)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import PLCparser.AbsPlc
import PLCparser.ComposOp


type Table a = M.Map String a
type TypeTable = Table TypeInfo
type SymbolTable = Table SymbolInfo
type SymbolList = [(String,SymbolInfo)]

data TypeInfo = TAny | TAnyNum | TBool | TInt | TReal | TString | TTime | TDate | TDateAndTime
              | TFunc TypeInfo SymbolList | TFuncBlock String SymbolList -- remember the name of function blocks.
              | TEnum TypeInfo Named_Spec_Init | TNamed String
              | TArray TypeInfo [Subrange] | TStruct TypeTable deriving (Show, Eq)
data SymbolInfo = SymbolInfo TypeInfo (Maybe Constant_Expr) deriving (Show, Eq)


lookUp :: (MonadError String m, MonadState (Table t) m) => String -> m t
lookUp str = do
    table <- get
    lookUpTable table str

lookUpReader :: (MonadError String m, MonadReader (Table t) m) => String -> m t
lookUpReader str = do
    table <- ask
    lookUpTable table str

lookUpTable :: MonadError String m => Table t -> String -> m t
lookUpTable table str = do
    case M.lookup str table of
      Just v  -> return v
      Nothing -> throwError $ "Undefined variable or type name: " ++ str

lookUpList :: MonadError String m => SymbolList -> String -> m SymbolInfo
lookUpList table str = do
    case find (\(a,_) -> a == str) table of
      Just (_,v) -> return v
      Nothing    -> throwError $ "Undefined variable or type name: " ++ str

addEntry :: (Show t, MonadError String m, MonadState (Table t) m) => String -> t -> m ()
addEntry str val = do
    table <- get
    when (M.member str table) $ throwError $ "Key: " ++ str ++ " already exists in table. Original value: " ++ show (fromJust (M.lookup str table)) ++ ". New value: " ++ show val
    put $ M.insert str val table
addEntryList :: (MonadError String m, MonadState SymbolList m) => String -> SymbolInfo -> m ()
addEntryList str val = do
    list <- get
    when (elem str . fst $ unzip list) $ throwError $ "Key: " ++ str ++ " already exists in table. Original value: " ++ show (fromJust (find (\(a,_) -> a == str) list)) ++ ". New value: " ++ show val
    put $ list ++ [(str, val)]

traverseUntill :: (MonadPlus m, MonadError String m) => (forall a. Tree a -> m b) -> Tree a -> m b
traverseUntill f node = catchError (f node) (\e -> composOpMPlus (traverseUntill f) node)


fillSymbolTable :: (MonadPlus m, MonadError String m) => TypeTable -> SymbolTable -> Tree a -> m SymbolTable
--fillSymbolTable types symbols tree = catchError (execStateT (extractSymbol types tree) symbols) (\e -> throwError $ show tree ++ "\n" ++ e)
fillSymbolTable types symbols tree = fillSymbolList types (M.toList symbols) tree >>= return . M.fromList
fillSymbolList :: (MonadPlus m, MonadError String m) => TypeTable -> SymbolList -> Tree a -> m SymbolList
fillSymbolList types symbols tree = catchError (execStateT (extractSymbol types tree) symbols) (\e -> throwError $ show tree ++ "\n" ++ e)


extractSymbol :: (MonadPlus m, MonadError String m, MonadState SymbolList m) => TypeTable -> Tree a -> m ()
extractSymbol types (Var_Decl_Init idents rhs) = do
  typ <- catchError (composOpMPlus (traverseUntill $ getSymbolType types) rhs) (\e -> throwError $ e ++ " in Var_Decl_Init for " ++ show idents ++ show rhs)
  forM_ idents (\(Identifier i) -> addEntryList i typ)
extractSymbol types (Var_Decl idents rhs) = do
  typ <- catchError (composOpMPlus (traverseUntill $ getSymbolType types) rhs) (\e -> throwError $ e ++ " in Var_Decl for " ++ show idents ++ show rhs)
  forM_ idents (\(Identifier i) -> addEntryList i typ)
extractSymbol types node = composOpM_ (extractSymbol types) node

getSymbolType :: (MonadPlus m, MonadError String m) => TypeTable -> Tree a -> m SymbolInfo
getSymbolType types (Unknown_Spec a)        = do
  t <- getAccessorType types a
  return $ SymbolInfo t Nothing
getSymbolType types (Unknown_Spec_Init a b) = do
  t <- getAccessorType types a
  return $ SymbolInfo t $ Just b
getSymbolType types (Simple_Spec_Init a b) = do
  t <- traverseUntill getType a
  return $ SymbolInfo t $ Just b
getSymbolType types node = do
  t <- getType node
  return $ SymbolInfo t Nothing

getAccessorType :: (MonadError String m) => TypeTable -> [Accessor] -> m TypeInfo
getAccessorType types [Accessor (Identifier i)] = do
                                                  t <- lookUpTable types i
                                                  case t of
                                                    TStruct _ -> throwError $ "Invalid access to member of non-struct type at: " ++ show i
                                                    _ -> lookUpTable types i
getAccessorType types ((Accessor (Identifier i)):tail) = do
                                                 t <- lookUpTable types i
                                                 case t of
                                                   TStruct k -> getAccessorType k tail
                                                   _ -> throwError $ "Invalid access to member of non-struct type at: " ++ show i


fillTypeTable :: (MonadPlus m, MonadError String m) => TypeTable -> Tree a -> m TypeTable
fillTypeTable table tree = execStateT (extractType tree) table

makeTypeTableOnChildren :: (MonadPlus m, MonadError String m) => Tree a -> m TypeTable
makeTypeTableOnChildren tree = fillTypeTableOnChildren M.empty tree
fillTypeTableOnChildren :: (MonadPlus m, MonadError String m) => TypeTable -> Tree a -> m TypeTable
fillTypeTableOnChildren table tree = execStateT (composOpM_ extractType tree) table

extractType :: (MonadPlus m, MonadError String m, MonadState TypeTable m) => Tree a -> m ()
extractType (Type_Decl (Identifier s) spec)        = do t <- catchError (composOpMPlus (traverseUntill getType) spec) (\e -> throwError $ e ++ " in Type_Decl for " ++ s)
                                                        addEntry s t
extractType (Struct_Elem_Decl (Identifier s) spec) = do t <- catchError (composOpMPlus (traverseUntill getType) spec) (\e -> throwError $ e ++ " in Struct_Elem_Decl for " ++ s)
                                                        addEntry s t
extractType (Func_Decl_Interface (Func_NameIdentifier (Identifier name)) typ _ decls) = do
                                        typp <- catchError (traverseUntill getType typ) (\e -> throwError $ e ++ " in Func_Decl_Interface for " ++ name)
                                        typeTable <- get
                                        symtab <- foldM (fillSymbolList typeTable) [] decls
                                        addEntry name $ TFunc typp symtab
extractType (FB_Decl _ (Identifier name) _ _ _ fbdecls methoddecls _) = do -- TODO Support Extends & Implements
                                        typeTable <- get
                                        symtab  <- foldM (fillSymbolList typeTable) [] fbdecls
                                        symtab' <- foldM (fillSymbolList typeTable) symtab methoddecls
                                        addEntry name $ TFuncBlock name symtab'
extractType node = composOpM_ extractType node

getType :: (MonadPlus m, MonadError String m) => Tree a -> m TypeInfo
getType Any_Type                  = return TAny
getType Any_Num                   = return TAnyNum
getType Bool                      = return TBool
getType (Bit_Str_Type_Name _)     = return TInt
getType (Int_Type_Name _)         = return TInt
getType (Real_Type_Name _)        = return TReal
getType (String_Type_Name _)      = return TString
getType (Time_Type_Name _)        = return TTime
getType (Tod_Type_Name _)         = return TTime
getType (Date_Type_Name _)        = return TDate
getType (DT_Type_Name _)          = return TDateAndTime
getType (Typed_Enum_Decl t spec)  = do
  typ <- getType t
  return $ TEnum typ spec
getType (Enum_Decl spec)          = return $ TEnum TInt spec
getType (Accessor (Identifier s)) = return $ TNamed s
getType (Array_Spec subs it) = do
  t <- catchError (composOpMPlus (traverseUntill getType) it) (\e -> throwError $ e ++ " in Array_Spec") -- More detail to error message
  return $ TArray t subs
getType node@(Struct_Decl _ _) = do
  table <- catchError (makeTypeTableOnChildren node) (\e -> throwError $ e ++ " in Struct_Decl")
  return $ TStruct table
getType node = throwError  $ "Can't get type information from: " ++ show node


printType :: Int -> TypeInfo -> String
printType _ TAny  = "Any"
printType _ TAnyNum = "Any_Num"
printType _ TBool = "Bool"
printType _ TInt  = "Int"
printType _ TReal = "Real"
printType _ TString = "String"
printType _ TTime = "Time"
printType _ TDate = "Date"
printType _ TDateAndTime = "Date_And_Time"
printType i (TFunc typ table) = "Function: " ++ printType 0 typ ++ "\n" ++ printSymbolTable i (M.fromList table)
printType i (TFuncBlock _ table) = "Function Block: \n" ++ printSymbolTable i (M.fromList table)
printType _ (TEnum typ spec) = "Enum: (" ++ show typ ++ ") " ++ show spec
printType _ (TNamed s) = s
printType i (TArray t _) = "[" ++ printType i t ++ "]"
printType i (TStruct t) = "Struct\n" ++ printTypeTable i t
printSymbol :: Int -> SymbolInfo -> String
printSymbol i (SymbolInfo typ _) = printType i typ
printTypeTable :: Int -> TypeTable -> String
printTypeTable i table = M.foldMapWithKey (\k v -> replicate i ' ' ++ k ++ " -> " ++ printType (i+4) v ++ "\n") table
printSymbolTable :: Int -> SymbolTable -> String
printSymbolTable i table = M.foldMapWithKey (\k v -> replicate i ' ' ++ k ++ " -> " ++ printSymbol (i+4) v ++ "\n") table


resolveNames :: MonadError String m => TypeTable -> SymbolTable -> m (TypeTable, SymbolTable)
resolveNames typeTable symbolTable = do
    typeTable'   <- mapM (resolveNamedTypes typeTable) typeTable
    typeTable''  <- mapM (resolveInnerSymbols typeTable') typeTable'
    symbolTable' <- mapM (updateSymbolInfo typeTable'') symbolTable
    let funcSymbolTable = M.map (\v -> SymbolInfo v Nothing) $ M.filter isFunc typeTable''
        enumSymbolTable = M.fromList . concat . map resolveEnum . M.toList $ M.filter isEnum typeTable'' in
      return (typeTable'', symbolTable' `M.union` funcSymbolTable `M.union` enumSymbolTable)

resolveNamedTypes :: MonadError String m => TypeTable -> TypeInfo -> m TypeInfo
resolveNamedTypes table (TNamed s)             = lookUpTable table s >>= resolveNamedTypes table --mfix (resolveNamedType table)  -- TODO: Potential infinite recursion here if (a: b; b: a). Not so good.!
resolveNamedTypes table (TStruct innerTable)   = mapM (resolveNamedTypes table) innerTable >>= return . TStruct
resolveNamedTypes table (TArray innerTyp subs) = resolveNamedTypes table innerTyp >>= \t -> return $ TArray t subs
resolveNamedTypes table (TFunc rtyp t)         = resolveNamedTypes table rtyp >>= \r -> return $ TFunc r t
resolveNamedTypes table (TFuncBlock s symTab)  = mapSndM (updateSymbolInfo table) symTab >>= return . TFuncBlock s
resolveNamedTypes table typ                    = return typ


resolveInnerSymbols :: MonadError String m => TypeTable -> TypeInfo -> m TypeInfo
resolveInnerSymbols table (TFunc rtyp symbolTable)   = mapSndM (updateSymbolInfo table) symbolTable >>= return . (TFunc rtyp)
resolveInnerSymbols table (TFuncBlock s symbolTable) = mapSndM (updateSymbolInfo table) symbolTable >>= return . TFuncBlock s
resolveInnerSymbols table typ = return typ

updateSymbolInfo :: MonadError String m => TypeTable -> SymbolInfo -> m SymbolInfo
updateSymbolInfo table (SymbolInfo typInfo s) = resolveNamedTypes table typInfo >>= \t -> return $ SymbolInfo t s


resolveEnum :: (String, TypeInfo) -> SymbolList
resolveEnum (s, TEnum typ (Named_Spec specs)) = map (resolveEnumSpec typ) (zip specs [1..])
resolveEnum (s, TEnum typ (Named_Spec_Init specs i)) = map (resolveEnumSpec typ) (zip specs [1..])
resolveEnumSpec :: TypeInfo -> (Enum_Value_Spec, Integer) -> (String, SymbolInfo)
resolveEnumSpec typ (Enum_Value_Spec (Identifier i), k)        = (i, SymbolInfo typ $ Just (Constant_Expr (Econst (Numeric_Literal (Int_Literal (Unsigned_Int (UnsignedInt (show k))))))))
resolveEnumSpec typ (Enum_Value_Spec_Init (Identifier i) e, _) = (i, SymbolInfo typ $ Just e)

isFunc :: TypeInfo -> Bool
isFunc (TFunc _ _) = True
isFunc (TFuncBlock _ _) = True
isFunc _ = False
isEnum :: TypeInfo -> Bool
isEnum (TEnum _ _) = True
isEnum _ = False


-- From GHC.MonadUtils
-- | Monadic version of mapSnd
mapSndM :: Monad m => (b -> m c) -> [(a,b)] -> m [(a,c)]
mapSndM _ []         = return []
mapSndM f ((a,b):xs) = do { c <- f b; rs <- mapSndM f xs; return ((a,c):rs) }
