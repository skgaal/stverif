{-# LANGUAGE FlexibleContexts #-}

module PLCparser.IntermediateRepresentation where

import Data.List(sort, isSuffixOf, nub)
import Control.Monad.Except

type Label = String

--type Var = String
type Value = Integer
type Cases = [(Value,Label)]

data UntypedOp = Assign | And | Xor | Or | Not | Add | Subtract | Multiply | Divide deriving (Eq, Show)
data UntypedCmp = Eq | Neq | Less | Greater | LessEqual | GreaterEqual deriving (Eq, Show)
data Operator = I_Assign | I_And | I_Xor | I_Or | I_Not | I_Add | I_Subtract | I_Multiply | I_Divide
              | R_Assign | R_Add | R_Subtract | R_Multiply | R_Divide | R_Assign_I | I_Assign_R deriving (Eq, Ord)
data Comparator = I_Eq | I_Neq | I_Less | I_Greater | I_LessEqual | I_GreaterEqual
                | R_Eq | R_Neq | R_Less | R_Greater | R_LessEqual | R_GreaterEqual deriving (Eq, Ord)

data Var = I_Var Integer | R_Var Integer | Named_Var Type String deriving (Eq, Ord)
data Rhs = Var Var | Int Integer | Real Double deriving (Eq, Ord)
data Instruction = Eval Var Operator Rhs     -- v1 op= return v2
                 | Compare Var Rhs Comparator Rhs
                 | If Rhs Label
                 | Case Rhs Cases
                 | Call Label Label
                 --             start loop  exit
                 | Loop Integer Label Label Label
                 | Unknown Integer
                 deriving (Eq, Ord)
data ProgLine = ProgLine Label Instruction Label deriving Eq
type Program = [ProgLine]

data Type = T_Int | T_Real deriving (Eq, Ord, Show)
class Typed a where
  typeOf :: a -> Type
instance Typed Type where
  typeOf = id
instance Typed Var where
  typeOf (I_Var _) = T_Int
  typeOf (R_Var _) = T_Real
  typeOf (Named_Var t _) = t
instance Typed Rhs where
  typeOf (Var v) = typeOf v
  typeOf (Int _) = T_Int
  typeOf (Real _) = T_Real

typeOperator :: (MonadError String m, Typed a) => UntypedOp -> a -> m Operator
typeOperator op t = typeOperator' op $ typeOf t
typeOperator' :: MonadError String m => UntypedOp -> Type -> m Operator
typeOperator' Assign   T_Int  = return I_Assign
typeOperator' And      T_Int  = return I_And
typeOperator' Xor      T_Int  = return I_Xor
typeOperator' Or       T_Int  = return I_Or
typeOperator' Not      T_Int  = return I_Not
typeOperator' Add      T_Int  = return I_Add
typeOperator' Subtract T_Int  = return I_Subtract
typeOperator' Multiply T_Int  = return I_Multiply
typeOperator' Divide   T_Int  = return I_Divide
typeOperator' Assign   T_Real = return R_Assign
typeOperator' Add      T_Real = return R_Add
typeOperator' Subtract T_Real = return R_Subtract
typeOperator' Multiply T_Real = return R_Multiply
typeOperator' Divide   T_Real = return R_Divide
typeOperator' op typ = throwError $ "Could not match untyped operator " ++ show op ++ " with type " ++ show typ

typeComparator :: Typed a => UntypedCmp -> a -> Comparator
typeComparator cmp t = typeComparator' cmp $ typeOf t
typeComparator' :: UntypedCmp -> Type -> Comparator
typeComparator' Eq           T_Int  = I_Eq
typeComparator' Neq          T_Int  = I_Neq
typeComparator' Less         T_Int  = I_Less
typeComparator' Greater      T_Int  = I_Greater
typeComparator' LessEqual    T_Int  = I_LessEqual
typeComparator' GreaterEqual T_Int  = I_GreaterEqual
typeComparator' Eq           T_Real = R_Eq
typeComparator' Neq          T_Real = R_Neq
typeComparator' Less         T_Real = R_Less
typeComparator' Greater      T_Real = R_Greater
typeComparator' LessEqual    T_Real = R_LessEqual
typeComparator' GreaterEqual T_Real = R_GreaterEqual


instance Show Var where
  show (I_Var i) = "vi" ++ show i
  show (R_Var i) = "vr" ++ show i
  show (Named_Var T_Int  s) = "vi_" ++ s
  show (Named_Var T_Real s) = "vr_" ++ s
instance Show Rhs where
  show (Var v)  = show v
  show (Int i)  = show i
  show (Real r) = show r

rhsToInt :: MonadError String m => Rhs -> m Integer
rhsToInt (Var v)  = throwError $ "Expected an integer but found variable: " ++ show v
rhsToInt (Int i)  = return i
rhsToInt (Real r) = throwError $ "Expected an integer but found real: " ++ show r

instance Show Operator where
  show I_Assign   = ":=i"
  show I_And      = "&&"
  show I_Xor      = "XOR"
  show I_Or       = "||"
  show I_Not      = ":= NOT"
  show I_Add      = "+=i"
  show I_Subtract = "-=i"
  show I_Multiply = "*=i"
  show I_Divide   = "/=i"
  show R_Assign   = ":=r"
  show R_Add      = "+=r"
  show R_Subtract = "-=r"
  show R_Multiply = "*=r"
  show R_Divide   = "/=r"
  show R_Assign_I = ":r=i"
  show I_Assign_R = ":i=r"
instance Show Comparator where
  show I_Eq           = "==i"
  show I_Neq          = "!=i"
  show I_Less         = "<i"
  show I_Greater      = ">i"
  show I_LessEqual    = "<=i"
  show I_GreaterEqual = ">=i"
  show R_Eq           = "==r"
  show R_Neq          = "!=r"
  show R_Less         = "<r"
  show R_Greater      = ">r"
  show R_LessEqual    = "<=r"
  show R_GreaterEqual = ">=r"

instance Show Instruction where
  show (Eval l op r) = show l ++ " " ++ show op ++ " " ++ show r
  show (Compare res l cmp r) = show res ++ " <- " ++ show l ++ " " ++ show cmp ++ " " ++ show r
  show (If v l) = "if " ++ show v ++ " then " ++ l
  show (Case v ls) = "case " ++ show v ++ " of " ++ show ls
  show (Call ls (':':le))   = "call " ++ ls ++ " " ++ le
  show (Call ls le)   = "call " ++ ls ++ " " ++ le
  show (Loop i start loop exit) = "loop " ++ show i ++ " : " ++ start ++ " " ++ loop ++ " " ++ exit
  show (Unknown i) = "unknown_" ++ show i

instance Show ProgLine where
  show (ProgLine l i (':':gotol)) = show (ProgLine l i gotol)
  show (ProgLine l i@(Loop _ _ loop exit) gotol) =
    l ++ ": " ++ show i ++ "; goto " ++ gotol ++ "\n"
    ++ loop ++ ": end\n"
    ++ exit ++ ": exit"
  show (ProgLine l i gotol) =
    l ++ ": " ++ show i ++ "; goto " ++ if "_end" `isSuffixOf` gotol then gotol ++ "\n" ++ gotol ++ ": return" else gotol
instance Ord ProgLine where
  compare (ProgLine ('l':x) _ _) (ProgLine ('l':y) _ _) = compare (read x :: Integer) (read y :: Integer)
  compare (ProgLine _ (If _ ('l':x)) ('l':_)) (ProgLine ('l':y) _ _) = let cmp = compare (read x :: Integer) (read y :: Integer) in if cmp == EQ then LT else cmp
  compare (ProgLine ('l':x) _ _) (ProgLine _ (If _ ('l':y)) ('l':_)) = let cmp = compare (read x :: Integer) (read y :: Integer) in if cmp == EQ then GT else cmp
  compare (ProgLine _ _ ('l':x)) (ProgLine ('l':y) _ _) = compare (read x :: Integer) (read y :: Integer)
  compare (ProgLine ('l':x) _ _) (ProgLine _ _ ('l':y)) = compare (read x :: Integer) (read y :: Integer)
  compare (ProgLine _ _ ('l':x)) (ProgLine _ _ ('l':y)) = compare (read x :: Integer) (read y :: Integer)
  compare (ProgLine lx _ _) (ProgLine ly _ _) = compare lx ly

printProgram :: Program -> IO ()
printProgram p = mapM_ putStrLn $ reverse . nub . reverse . concat $ map (lines . show) (sort p)

showProgram :: Program -> String
showProgram p = foldr (\a b -> a ++ "\n" ++ b) "" . reverse . nub . reverse . concat $ map (lines . show) (sort p)
