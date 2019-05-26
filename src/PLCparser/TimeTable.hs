{-# LANGUAGE FlexibleContexts #-}
module PLCparser.TimeTable where

import PLCparser.IntermediateRepresentation

data Operation = O Operator
               | C Comparator
               | OCall
               | Return
               | Jump
               | Nop

data TInterval = TI Integer Integer
               | Ti Integer
               | Tinf Integer
               | Tmaxt Integer
               | Tmaxtinf deriving Eq

addTI :: TInterval -> TInterval -> TInterval
addTI (TI al ah) (TI bl bh) = TI (al + bl) (ah + bh)
addTI (TI al ah) (Ti b)     = TI (al + b)  (ah + b)
addTI (TI a  _ ) (Tinf b)   = Tinf (a + b)
addTI (TI a  _ ) (Tmaxt b)  = error ""-- Tmaxt (a + b)
addTI (TI a  _ ) Tmaxtinf   = error ""-- Tmaxtinf
addTI (Ti a)     (Ti b)     = Ti (a + b)
addTI (Ti a)     (Tinf b)   = Tinf (a + b)
addTI (Ti a)     (Tmaxt b)  = error ""-- Tmaxt (a + b)
addTI (Ti a)     Tmaxtinf   = error ""-- Tmaxtinf
addTI (Tinf a)   (Tinf b)   = Tinf (a + b)
addTI (Tinf a)   (Tmaxt b)  = error ""-- Tmaxtinf
addTI (Tinf a)   Tmaxtinf   = error ""-- Tmaxtinf
addTI (Tmaxt a)  (Tmaxt b)  = error ""
addTI (Tmaxt a)  Tmaxtinf   = error ""
addTI Tmaxtinf   Tmaxtinf   = error ""
addTI a b = addTI b a

instance Show TInterval where
  show (TI _ j)   = "&lt;= "++show j++""
  show (Ti j)     = "&lt;= "++show j++""
  show (Tinf _)   = "&lt; inf"
  show (Tmaxt _)  = "&lt;= **PLACEHOLDER**"
  show (Tmaxtinf) = "&lt; inf"

showarci :: TInterval -> String
showarci (TI a b) = "["++show b++","++show b++"]"
showarci (Ti a)   = "["++show a++","++show a++"]"
showarci (Tinf a) = "["++show a++",inf)"
showarci (Tmaxt a)= "["++show a++",**PLACEHOLDER**]"
showarci (Tmaxtinf)="[**PLACEHOLDER**,inf)"

-- These are only for the lx's invariant
invariant :: Instruction -> TInterval
invariant (If _ _)          = time Jump
invariant (Case _ _)        = time Jump
invariant (Call _ _)        = time OCall
invariant (Loop _ _ _ _)    = time Jump
invariant (Compare _ _ c _) = time (C c)
invariant (Eval _ o _)      = time (O o)

time :: Operation -> TInterval
-- Operators
-- Ints
time (O I_Assign)   = (Ti 1) -- TODO: Define these
time (O I_And)      = (Ti 1)
time (O I_Xor)      = (Ti 1)
time (O I_Or)       = (Ti 1)
time (O I_Not)      = (Ti 1)
time (O I_Add)      = (Ti 1)
time (O I_Subtract) = (Ti 1)
time (O I_Multiply) = (Ti 1)
time (O I_Divide)   = (TI 2 12)
-- Reals
time (O R_Assign  ) = (Ti 1)
time (O R_Add     ) = (Ti 1)
time (O R_Subtract) = (Ti 1)
time (O R_Multiply) = (Ti 1)
time (O R_Divide  ) = (Ti 14)
-- casting
time (O R_Assign_I) = (Ti 1)
time (O I_Assign_R) = (Ti 1)
-- Comparators
-- Ints
time (C I_Eq)           = (Ti 1)
time (C I_Neq)          = (Ti 1)
time (C I_Less)         = (Ti 1)
time (C I_Greater)      = (Ti 1)
time (C I_LessEqual)    = (Ti 1)
time (C I_GreaterEqual) = (Ti 1)
-- Reals
time (C R_Eq)           = (Ti 1)
time (C R_Neq)          = (Ti 1)
time (C R_Less)         = (Ti 1)
time (C R_Greater)      = (Ti 1)
time (C R_LessEqual)    = (Ti 1)
time (C R_GreaterEqual) = (Ti 1)
-- Instructions
time OCall    = (TI 2 4)
time Return   = (TI 3 15)
time Jump     = (TI 2 4)
time Nop      = (Ti 1)
