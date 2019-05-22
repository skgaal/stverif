
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
module PLCparser.ParseConstant(parseConstant, parseNumLit) where

import Numeric(readInt, readDec, readOct, readHex)
import Text.Read(readMaybe)
import Data.Char(digitToInt)
import Control.Monad.Except

import PLCparser.AbsPlc
import qualified PLCparser.IntermediateRepresentation as IR(Rhs(..), Var(..), Type(..))


parseConstant :: MonadError String m => Constant -> m IR.Rhs
parseConstant (Numeric_Literal n) = parseNumLit n
parseConstant (Bool_Literal b) = return . IR.Int $ if parseBoolLit b then 1 else 0
parseConstant (Char_Literal (Char_Str c)) = return . IR.Var $ IR.Named_Var IR.T_Int (parseCharStr c) -- WARNING: Integer type variable contains string value!!
parseConstant (Time_Literal t) = parseTimeLit t >>= return . IR.Int
parseConstant c = throwError $ "Parsing constant not supported: " ++ show c
--Time_Literal :: Time_Literal -> Tree Constant_
--Bit_Str_Literal :: Bit_Str_Literal -> Tree Constant_

parseBoolLit :: Bool_Literal -> Bool
parseBoolLit (Typed_False_Literal _) = False
parseBoolLit (Typed_True_Literal _) = True
parseBoolLit False_Literal = False
parseBoolLit True_Literal = True

parseCharStr :: Char_Str -> String
parseCharStr (S_Byte_Char_Str (SByteCharStr s)) = s
parseCharStr (D_Byte_Char_Str (DByteCharStr s)) = s


parseNumLit :: (MonadError String m) => Numeric_Literal -> m IR.Rhs
parseNumLit (Int_Literal i) = parseIntLit i >>= return . IR.Int
parseNumLit (Typed_Int_Literal _ i) = parseIntLit i >>= return . IR.Int
parseNumLit (Real_Literal r) = parseRealLit r >>= return . IR.Real
parseNumLit (Typed_Real_Literal _ r) = parseRealLit r >>= return . IR.Real
parseNumLit (Unsigned_Int_Literal i) = parseIntLit i >>= return . IR.Int
parseNumLit (Unsigned_Real_Literal r) = parseRealLit r >>= return . IR.Real

parseIntLit :: MonadError String m => Int_Literal -> m Integer
parseIntLit (Signed_Int s) = parseSignedInt s
parseIntLit (Binary_Int (BinaryInt s@('2':'#':n))) = tryRead s $ readInt 2 (`elem` "01") digitToInt (filter (/= '_') n)
parseIntLit (Octal_Int   (OctalInt s@('8':'#':n))) = tryRead s $ readOct (filter (/= '_') n)
parseIntLit (Hex_Int   (HexInt s@('1':'6':'#':n))) = tryRead s $ readHex (filter (/= '_') n)
parseIntLit (Unsigned_Int u) = parseUnsignedInt u

parseRealLit :: MonadError String m => Real_Literal -> m Double
parseRealLit (Signed_Real n1 n2) = do
  i <- parseSignedInt n1
  f <- parseUnsignedInt n2
  return (read (show i ++ "." ++ show f) :: Double)
parseRealLit (Signed_Real_Exp r) = parseSignedRealExpLit r
parseRealLit (Unsigned_Real n1 n2) = do
  i <- parseUnsignedInt n1
  f <- parseUnsignedInt n2
  return (read (show i ++ "." ++ show f) :: Double)
parseRealLit (Unsigned_Real_Exp r) = parseUnsignedRealExpLit r

parseSignedRealExpLit :: MonadError String m => Signed_Real_Exp -> m Double
parseSignedRealExpLit (Negative_Real_Exp r) = parseUnsignedRealExpLit r >>= return . ((0::Double)-)
parseSignedRealExpLit (Positive_Real_Exp r) = parseUnsignedRealExpLit r

parseUnsignedRealExpLit :: MonadError String m => UnsignedRealExp -> m Double
parseUnsignedRealExpLit (UnsignedRealExp s) =
  case ((readMaybe (filter (/= '_') s))::Maybe Double) of
    Just d -> return d
    Nothing -> throwError $ "Error when parsing: " ++ s ++ " as an unsigned real exp lit."
parseSignedInt :: MonadError String m => Signed_Int -> m Integer
parseSignedInt (Negative_Int u) = parseUnsignedInt u >>= return . (0-)
parseSignedInt (Positive_Int u) = parseUnsignedInt u

parseUnsignedInt :: MonadError String m => UnsignedInt -> m Integer
parseUnsignedInt (UnsignedInt s) = tryRead s $ readDec (filter (/= '_') s)

tryRead :: MonadError String m => String -> [(a,String)] -> m a
tryRead _ ((x,_):_) = return x
tryRead s [] = throwError $ "Error when parsing: " ++ s


parseTimeLit :: (MonadError String m) => Time_Literal -> m Integer
parseTimeLit (Duration_Literal (Duration _ i)) = parseSignInterval i
parseTimeLit c = throwError $ "Parsing time literal not supported: " ++ show c
--Time_Of_Day_Literal :: Tod_Type -> Daytime -> Tree Time_Literal_
--Date_Literal :: Date_Type -> Date_Literal -> Tree Time_Literal_
--Date_And_Time_Literal :: DT_Type -> Date_Literal -> Daytime -> Tree Time_Literal_
parseSignInterval :: (MonadError String m) => Sign_Interval -> m Integer
parseSignInterval (Negative_Interval i) = parseInterval i >>= return . (0-)
parseSignInterval (Positive_Interval i) = parseInterval i
parseInterval :: (MonadError String m) => Interval -> m Integer
parseInterval (Milliseconds (MS_Token s) Empty) = tryRead s $ readDec (filter (\c -> c /= '_' && c /= 'm' && c /= 's') s)
-- Days_Real :: UnsignedInt -> D_Token -> Tree Interval_
-- Days :: D_Token -> Interval -> Tree Interval_
-- Hours_Real :: UnsignedInt -> H_Token -> Tree Interval_
-- Hours :: H_Token -> Interval -> Tree Interval_
-- Minutes_Real :: UnsignedInt -> M_Token -> Tree Interval_
-- Minutes :: M_Token -> Interval -> Tree Interval_
-- Seconds_Real :: UnsignedInt -> S_Token -> Tree Interval_
-- Seconds :: S_Token -> Interval -> Tree Interval_
-- Milliseconds_Real :: UnsignedInt -> MS_Token -> Tree Interval_
-- Milliseconds :: MS_Token -> Interval -> Tree Interval_
-- Microseconds_Real :: UnsignedInt -> US_Token -> Tree Interval_
-- Microseconds :: US_Token -> Interval -> Tree Interval_
-- Nanoseconds_Real :: UnsignedInt -> NS_Token -> Tree Interval_
-- Nanoseconds :: NS_Token -> Tree Interval_
-- Empty :: Tree Interval_
