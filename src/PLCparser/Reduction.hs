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
module PLCparser.Reduction(performReduction) where

import Control.Applicative
import qualified Data.Map as M

import PLCparser.IntermediateRepresentation(Program, ProgLine(..), Instruction(..), Label)
import PLCparser.UnknownFunctions(UnknownMap, KnownMap)
import PLCparser.TimeTable
import PLCparser.IRToXml(getUnknownTiming)

performReduction :: Program -> UnknownMap -> Maybe KnownMap -> (Program, UnknownMap, Maybe KnownMap)
performReduction program unknownmap knownmap = let
  linearInstructions = filter (isEvalCompareUnknown . getInstruction) program
  reducibles = filter (isReducible program) linearInstructions
  firsts = filter (isFirstInReduction reducibles) linearInstructions
  others = filter (\l -> notElem l (firsts ++ reducibles)) program
  newLines = map (calculateReductions (timeOf unknownmap knownmap) reducibles) $ zip firsts [(toInteger $ M.size unknownmap)..]
  moreUnknowns = map (\(ProgLine _ inst@(Unknown i) _) -> (inst, "__reduced" ++ show i)) . fst $ unzip newLines
  moreKnowns = zip (snd $ unzip moreUnknowns) (map intervalToPair . snd $ unzip newLines)
  in
    (others ++ fst (unzip newLines),
    unknownmap `M.union` (M.fromList moreUnknowns),
    fmap (M.union $ M.fromList moreKnowns) (knownmap <|> Just M.empty))

calculateReductions :: (Instruction -> TInterval) -> Program -> (ProgLine, Integer) -> (ProgLine, TInterval)
calculateReductions getTime reducibles (ProgLine start inst gotoLabel, i) =
  let (end, time) = findEndTime getTime gotoLabel reducibles in
    (ProgLine start (Unknown i) end, addTI time $ getTime inst)

findEndTime :: (Instruction -> TInterval) -> Label -> Program -> (Label, TInterval)
findEndTime getTime label reducibles = case filter (hasLabel label) reducibles of
  [ProgLine _ inst gotoLabel] -> (\(l, t) -> (l, addTI t $ getTime inst)) $ findEndTime getTime gotoLabel reducibles
  _ -> (label, Ti 0)

isFirstInReduction :: Program -> ProgLine -> Bool
isFirstInReduction reducible pl@(ProgLine _ _ gotoLabel) = notElem pl reducible && any (hasLabel gotoLabel) reducible

hasLabel :: Label -> ProgLine -> Bool
hasLabel otherLabel (ProgLine label _ _) = label == otherLabel

isReducible :: Program -> ProgLine -> Bool
isReducible program (ProgLine label _ _) =
  case filter (goesto label) program of
    [progline] -> isEvalCompareUnknown $ getInstruction progline
    _ -> False

goesto :: Label -> ProgLine -> Bool
goesto label (ProgLine _ (Case _ cases) gotoLabel) = elem label (snd $ unzip cases) || gotoLabel == label
goesto label (ProgLine _ (If _ branch)  gotoLabel) = branch == label || gotoLabel == label
goesto label (ProgLine _ (Call start _) gotoLabel) = start == label || gotoLabel == label
goesto label (ProgLine _ _ gotoLabel)              = gotoLabel == label


getInstruction :: ProgLine -> Instruction
getInstruction (ProgLine _ inst _) = inst

isEvalCompareUnknown :: Instruction -> Bool
isEvalCompareUnknown (Eval _ _ _)      = True
isEvalCompareUnknown (Compare _ _ _ _) = True
isEvalCompareUnknown (Unknown _)       = True
isEvalCompareUnknown _                 = False

timeOf :: UnknownMap -> Maybe KnownMap -> Instruction -> TInterval
timeOf _ _ (Eval _ op _)       = addTI (Ti 3) $ time $ O op
timeOf _ _ (Compare _ _ cmp _) = addTI (Ti 3) $ time $ C cmp
timeOf um km i@(Unknown _)     = getUnknownTiming um km i

intervalToPair :: TInterval -> (Integer, Integer)
intervalToPair (TI a b) = (a,b)
intervalToPair (Ti a)   = (a,a)
intervalToPair _        = error ""
