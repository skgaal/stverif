{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module PLCparser.Annotation where

import Control.Monad.State
import PLCparser.AbsPlc
import PLCparser.ComposOp

import qualified Data.Map as M

type Annotation = String  -- Insert the real thing here.

data AnnotatedTree where
  Annotate :: Annotation -> [AnnotatedTree] -> Tree a -> AnnotatedTree

instance Show AnnotatedTree where
  showsPrec n (Annotate s b t) = case b of
    [] -> opar n . showString s . showString ": " . showsPrec 1 t. cpar n
    [xs] -> opar n . showString s . showString ": " . showsPrec 1 xs . cpar n
    xs@(_:_) -> opar n . showString s . showString ": " . showsPrec 1 xs . cpar n
    where opar n = if n > 0 then showChar '(' else id
          cpar n = if n > 0 then showChar ')' else id

annotate :: forall a. Tree a -> ([AnnotatedTree] -> Tree a -> AnnotatedTree)
annotate (Identifier s)   = Annotate "T_Ident"
annotate (Int_Literal i)     = Annotate "T_Int"
annotate (Eplus e1 e2) = Annotate "T_Sum"
annotate _ = Annotate "?"

annotateTree :: forall a. Tree a -> AnnotatedTree
annotateTree node = annotate node (annotateChildren node) node
annotateChildren = composOpMonoid (\n -> [annotateTree n])
