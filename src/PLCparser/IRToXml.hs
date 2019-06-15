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
{-# LANGUAGE RankNTypes #-}

module PLCparser.IRToXml(makePetriNet, getUnknownTiming, memoryaccesstime) where

import PLCparser.IntermediateRepresentation
import PLCparser.UnknownFunctions
import PLCparser.TimeTable
import Data.List
import qualified Data.Map as M (empty, toList, lookup)

data Place = Place String Integer TInterval
           | FPlace String deriving Eq
type ArcWeight = Integer
data Arc = ArcToT String String ArcWeight TInterval
         | ArcToP String String ArcWeight
data Transition = Trans String | Urgent String


memoryaccesstime = Ti 15 -- 3 * 5cycles


-- ****************************  show functions ****************************  --
class TapaalShow a where
  showeditor :: a -> String
  showengine :: a -> String -> String

-- <place id="l161" initialMarking="0" invariant="&lt;= 1" nameOffsetX="-5" nameOffsetY="35" positionX="0" positionY="0"/>
-- <place id="Logger6_l0" initialMarking="0" invariant="&lt;= 1" name="Logger6_l0"/>
instance TapaalShow Place where
  showeditor (Place i m t)   = "<place id='"++i++"' initialMarking='"++show m++"' invariant='"++show t++"' nameOffsetX='-5' nameOffsetY='35' positionX='0' positionY='0'/>"
  showeditor (FPlace i)      = "<place id='"++i++"' initialMarking='0' invariant='&lt;= 0' nameOffsetX='-5' nameOffsetY='35' positionX='0' positionY='0'/>"
  showengine (Place i m t) n = "<place id='"++n++i++"' initialMarking='"++show m++"' invariant='"++show t++"' name='"++n++i++"'/>"
  showengine (FPlace i) n    = "<place id='"++n++i++"' initialMarking='0' invariant='&lt;= 0' name='"++n++i++"'/>"

-- <transition angle="0" id="Tl9" name="Tl9" nameOffsetX="-5" nameOffsetY="35" positionX="0" positionY="0" priority="0"/>
-- <transition id="Logger6_Tl218" name="Logger6_Tl218" urgent="false"/>
instance TapaalShow Transition where
  showeditor (Trans n)    = "<transition angle='0' id='T"++n++"' name='T"++n++"' nameOffsetX='-5' nameOffsetY='35' positionX='0' positionY='0' priority='0'/>"
  showeditor (Urgent n)   = "<transition angle='0' id='T"++n++"' name='T"++n++"' nameOffsetX='-5' nameOffsetY='35' positionX='0' positionY='0' priority='0' urgent='true'/>"
  showengine (Trans a)  n = "<transition id='"++n++"T"++a++"' name='"++n++"T"++a++"' urgent='false'/>"
  showengine (Urgent a) n = "<transition id='"++n++"T"++a++"' name='"++n++"T"++a++"' urgent='true'/>"

-- <arc id="l106 to Tl106" inscription="[2,4]" nameOffsetX="0" nameOffsetY="0" source="l106" target="Tl106" type="timed" weight="1"/>
-- <inputArc inscription="[1,1]" source="Logger6_l33" target="Logger6_Tl33"/>
-- <outputArc inscription="1" source="Logger6_Tl187" target="Logger6_l188"/>
instance TapaalShow Arc where
  showeditor (ArcToT f t w i)   = "<arc id='" ++f++" to T"++t++"' inscription='"++showarci i++"' nameOffsetX='0' nameOffsetY='0' source='" ++f++"' target='T"++t++"' type='timed' weight='" ++show w++"'/>"
  showeditor (ArcToP f t w)     = "<arc id='T"++f++" to " ++t++"' inscription='1'                nameOffsetX='0' nameOffsetY='0' source='T"++f++"' target='" ++t++"' type='normal' weight='"++show w++"'/>"
  showengine (ArcToT f t w i) n = "<inputArc inscription='"++showarci i++"' source='"++n++f++"' target='"++n++"T"++t++"'" ++ (if w > 1 then " weight='"++show w++"' />" else "/>")
  showengine (ArcToP f t w)   n = "<outputArc inscription='1' source='"++n++"T"++f++"' target='"++n++t++"'" ++ (if w > 1 then " weight='"++show w++"' />" else "/>")

-- ****************************  Primary functions *************************  --
makePetriNet :: Bool -> (String -> Integer -> Program -> UnknownMap -> Maybe KnownMap -> String)
makePetriNet editor = if editor then makeEditorNet else makeEngineNet
  where
    makeEditorNet :: String -> Integer -> Program -> UnknownMap -> Maybe KnownMap -> String
    makeEditorNet _ _ [] _ _ = []
    makeEditorNet name maxt p m k = "<?xml version='1.0' encoding='UTF-8' standalone='no'?> <pnml> <constant name='t' value='"++show maxt++"'/><net active='true' id='"++name++"' type='P/T net'>" ++
                 (serializeWith showeditor . nub $ findAllPlaces p m k) ++
                 (serializeWith showeditor $ findAllTransitions p m k)  ++
                 (serializeWith showeditor $ findAllArcs p m k)         ++
                 generateWatchdogEditor ++
                  "</net><query active='true' approximationDenominator='2' capacity='4' discreteInclusion='false' enableOverApproximation='false' enableUnderApproximation='false' extrapolationOption='null' gcd='false' hashTableSize='null' inclusionPlaces='*NONE*' name='Ever deadlock' overApproximation='false' pTrie='true' query='EF (deadlock)' reduction='true' reductionOption='VerifyTAPNdiscreteVerification' searchOption='HEURISTIC' symmetry='true' timeDarts='false' traceOption='NONE' useStubbornReduction='true'/>" ++
                  "<query active='true' approximationDenominator='2' capacity='4' discreteInclusion='false' enableOverApproximation='false' enableUnderApproximation='false' extrapolationOption='null' gcd='false' hashTableSize='null' inclusionPlaces='*NONE*' name='AFok' overApproximation='false' pTrie='true' query='AF " ++name++ ".watchdog_ok__ = 1' reduction='true' reductionOption='VerifyTAPNdiscreteVerification' searchOption='HEURISTIC' symmetry='true' timeDarts='false' traceOption='NONE' useStubbornReduction='true'/>" ++
                  "<k-bound bound='3'/></pnml>"
    makeEngineNet :: String -> Integer -> Program -> UnknownMap -> Maybe KnownMap -> String
    makeEngineNet _ _ [] _ _ = []
    makeEngineNet name maxt p m k = "<pnml><net id='ComposedModel' type='P/T net'>" ++
                 (serializeWith ((flip showengine) "") . nub $ findAllPlaces p m k) ++
                 (serializeWith ((flip showengine) "") $ findAllTransitions p m k)  ++
                 (serializeWith ((flip showengine) "") $ findAllArcs p m k)         ++
                 generateWatchdogEngine "" ++
                  "</net></pnml>"

-- ****************************  Finding things ****************************  --
findAllPlaces :: Program -> UnknownMap -> Maybe KnownMap -> [Place]
findAllPlaces [] _ _ = []
findAllPlaces ((ProgLine l ins gl):xs) m k = findPlacesInstruction l ins m k ++
                                           findPlacesGotoLabel gl ++
                                           findAllPlaces xs m k
  where
    findPlacesInstruction :: Label -> Instruction -> UnknownMap -> Maybe KnownMap -> [Place]
    findPlacesInstruction l ins@(Eval _ _ _)      _ _ = [(Place l 0 $ addTI memoryaccesstime $ invariant ins)]
    findPlacesInstruction l ins@(Compare _ _ _ _) _ _ = [(Place l 0 $ addTI memoryaccesstime $ invariant ins)]
    findPlacesInstruction l ins@(If _ il)         _ _ = [(Place l 0 $ invariant ins)]
    findPlacesInstruction l ins@(Case _ _)        _ _ = [(Place l 0 $ invariant ins)]
    findPlacesInstruction l ins@(Loop n _ ll le)  _ _ = [(Place l 0 $ invariant ins)
                                                        ,(Place (l++"p")   n (Tinf 0))
                                                        ,(Place (l++"pp")  0 (Tinf 0))
                                                        ,(Place (l++"ppp") 0 $ time Jump)
                                                        ,(Place (l++"pppp")0 $ time Nop)
                                                        ,(Place ll         0 (TI 0 0))
                                                        ,(Place le         0 (TI 0 0))]
    findPlacesInstruction l ins@(Call _ le)       _ _ = [(Place l          0 $ invariant ins)
                                                        ,(Place (l++"p")   0 (Tinf 0))]
    findPlacesInstruction l ins@(Unknown _)       m k = [(Place l          0 $ getUnknownTiming m k ins)] -- TODO: Lookup the unknown in the map!
    findPlacesGotoLabel :: Label -> [Place]
    findPlacesGotoLabel (':':gl) = [(Place gl 0 $ time Return)]
    findPlacesGotoLabel _ = []

findAllTransitions :: Program -> UnknownMap -> Maybe KnownMap -> [Transition]
findAllTransitions [] _ _ = []
findAllTransitions (x:xs) m k = (findTransitions x m k) ++ findAllTransitions xs m k
  where
    findTransitions :: ProgLine -> UnknownMap -> Maybe KnownMap -> [Transition]
    findTransitions (ProgLine l ins@(Eval _ _ _) _)      _ _ = [(Trans l)]
    findTransitions (ProgLine l ins@(Compare _ _ c _) _) _ _ = [(Trans l)]
    findTransitions (ProgLine l ins@(If _ _) _)          _ _ = [(Trans $ l ++ "T")
                                                               ,(Trans $ l ++ "F")]
    findTransitions (ProgLine l ins@(Call _ le) _)       _ _ = [(Trans l)
                                                               ,(Trans $ l ++ removeColon le)]
    findTransitions (ProgLine l ins@(Loop _ ls _ le) _)  _ _ = [(Trans l)              -- initial transition     (start in report)
                                                               ,(Trans $ l ++ "P")     -- P = prime              (time in report)
                                                               ,(Trans $ l ++ "L")     -- L = loop transition    (loop in report)
                                                               ,(Trans $ l ++ "E")     -- E = Exit transition    (exiting in report)
                                                               ,(Trans $ l ++ "C")     -- C = Cleanup transition (cleanup in report)
                                                               ,(Trans $ l ++ "D")     -- D = Done trantition    (done in report)
                                                               ,(Trans $ l ++ "N")]    -- N = Nop trantition     (nop in report)
    findTransitions (ProgLine l ins@(Case _ cs) _)       _ _ = makeCaseTransitions l (nub . snd $ unzip cs)
      where
        makeCaseTransitions :: Label -> [Label] -> [Transition]
        makeCaseTransitions l []          = [(Trans $ l ++ "Else")]             -- We always generate an "Else" transition.
        makeCaseTransitions l (cl:xs) = [(Trans $ l ++ "_" ++ cl)] ++ makeCaseTransitions l xs
    findTransitions (ProgLine l ins@(Unknown i) _)       m k  = [(Trans l)]

testloop = [ProgLine "CYCLIC_start" (Loop 4 "l0" "sl1" "sl2") "l1"
           ,ProgLine "l0" (Eval (I_Var 5) I_Add (Int 2)) "sl1"
           ,ProgLine "l1" (Eval (I_Var 5) I_Add (Int 2)) "CYCLIC_end"
           ]

findAllArcs :: Program -> UnknownMap -> Maybe KnownMap -> [Arc]
findAllArcs [] _ _ = []
findAllArcs (x:xs) m k = (findArcs x m k) ++ findAllArcs xs m k
  where
    findArcs :: ProgLine -> UnknownMap -> Maybe KnownMap -> [Arc]
    findArcs (ProgLine l ins@(Eval _ op _) gl)     _ _= [(ArcToT l        l        1 $ addTI memoryaccesstime $ time $ O op)
                                                        ,(ArcToP l        (removeColon gl)       1)]
    findArcs (ProgLine l ins@(Compare _ _ c _) gl) _ _= [(ArcToT l        l        1 $ addTI memoryaccesstime $ time $ C c)
                                                        ,(ArcToP l        (removeColon gl)       1)]
    findArcs (ProgLine l ins@(If _ ly) gl)         _ _= [(ArcToT l        (l++"T") 1 $ time Jump)
                                                        ,(ArcToT l        (l++"F") 1 $ time Jump)
                                                        ,(ArcToP (l++"T") ly 1)
                                                        ,(ArcToP (l++"F") (removeColon gl) 1)]
    findArcs (ProgLine l ins@(Call ls le) gl)      _ _= [(ArcToT l        l          1 $ time OCall)
                                                        ,(ArcToT (l++"p") (l++ removeColon le)    1 $ (Tinf 0))
                                                        ,(ArcToT (removeColon le)       (l++ removeColon le)    1 $ time Return)
                                                        ,(ArcToP l        (l++"p")   1)
                                                        ,(ArcToP l        ls         1)
                                                        ,(ArcToP (l++ removeColon le)  (removeColon gl)         1)]
    findArcs (ProgLine l ins@(Loop n ls le ll) gl) _ _= [(ArcToT l          l          1 $ time Jump)
                                                        ,(ArcToP l          le         1)
                                                        ,(ArcToT le         (l++"L")   1 (TI 0 0))
                                                        ,(ArcToP (l++"L")   (l++"ppp") 1)
                                                        ,(ArcToT (l++"ppp") (l++"P")   1 $ time Jump)
                                                        ,(ArcToP (l++"P")   ls         1)
                                                        ,(ArcToT le         (l++"E")   1 (TI 0 0))
                                                        ,(ArcToP (l++"E")   ll         1)
                                                        ,(ArcToP (l++"L")   (l++"pp")  1)
                                                        ,(ArcToT ll         (l++"C")   1 (TI 0 0))
                                                        ,(ArcToT (l++"pp")  (l++"C")   1 (Tinf 0))
                                                        ,(ArcToP (l++"C")   ll         1)
                                                        ,(ArcToP (l++"C")   (l++"p")   1)
                                                        ,(ArcToT (l++"p")   (l++"L")   1 (Tinf 0))
                                                        ,(ArcToT (l++"p")   (l++"D")   n (Tinf 0))
                                                        ,(ArcToT ll         (l++"D")   1 (TI 0 0))
                                                        ,(ArcToP (l++"D")   (l++"pppp") 1)
                                                        ,(ArcToP (l++"D")   (l++"p")   n)
                                                        -- Optimization
                                                        -- ,(ArcToP (l++"E")   (l++"pp")  n)
                                                        -- ,(ArcToT (l++"pp")  (l++"E")   n (Tinf 0))
                                                        ,(ArcToT (l++"pppp") (l++"N")  1 $ time Nop)
                                                        ,(ArcToP (l++"N")   (removeColon gl) 1)
                                                        ]
    findArcs (ProgLine l ins@(Case _ cs) gl)       _ _= findCaseArcs l (nub . snd $ unzip cs) (removeColon gl)
      where
        findCaseArcs :: Label -> [Label] -> Label -> [Arc]
        findCaseArcs l [] gl      = [(ArcToT l           (l++"Else")  1   $ time Jump)
                                    ,(ArcToP (l++"Else") gl           1)]
        findCaseArcs l (cl:xs) gl = [(ArcToT l           (l++"_"++cl) 1 $ time Jump)
                                    ,(ArcToP (l++"_"++cl) cl          1)] ++
                                    findCaseArcs l xs gl
    findArcs (ProgLine l ins@(Unknown _) gl)       u k  = [(ArcToT l l  1 $ getUnknownTiming m k ins )
                                                          ,(ArcToP l (removeColon gl) 1)]

knowntointerval :: (Integer,Integer) -> TInterval
knowntointerval (a,b) = (TI a b)

-- ****************************  Utility things ****************************  --
getUnknownTiming :: UnknownMap -> Maybe KnownMap -> Instruction -> TInterval
getUnknownTiming _ Nothing _ = (TI 0 0)
getUnknownTiming m (Just map) i = let e = M.lookup i m in
                                    case e of
                                      Just l -> let elem = M.lookup l map in
                                        case elem of
                                          Just (a,b) -> (TI a b)
                                          Nothing -> (TI 0 0) -- Should we do this?
                                      Nothing -> (TI 0 0)

removeColon :: Label -> Label
removeColon (':':l) = l
removeColon l = l

serializeWith :: (a -> String) -> [a] -> String
serializeWith _ [] = []
serializeWith f (x:xs) = f x ++ serializeWith f xs

generateWatchdogEngine name = (serializeWith ((flip showengine) name) watchdogPlaces)      ++
                              (serializeWith ((flip showengine) name) watchdogTransitions) ++
                              (serializeWith ((flip showengine) name) watchdogArcs)

generateWatchdogEditor = (serializeWith showeditor watchdogPlaces)      ++
                         (serializeWith showeditor watchdogTransitions) ++
                         (serializeWith showeditor watchdogArcs)

watchdogPlaces :: [Place]
watchdogPlaces = [(Place "watchdog_start__" 1 (Ti 0))
                 ,(Place "watchdog_watch__" 0 (Tmaxt 0))
                 ,(Place "watchdog_ok__"    0 (Tinf 0))
                 ,(Place "watchdog_not_ok__"0 (Ti 0))]
watchdogTransitions :: [Transition]
watchdogTransitions = [(Trans "watchdog_T1")
                      ,(Trans "watchdog_T2")
                      ,(Trans "watchdog_T3")
                      ,(Trans "watchdog_T4")]
watchdogArcs :: [Arc]
watchdogArcs = [(ArcToT "watchdog_start__" "watchdog_T1" 1 (Ti 0))
               ,(ArcToT "watchdog_watch__" "watchdog_T2" 1 (Tinf 0))
               ,(ArcToT "CYCLIC_end" "watchdog_T2"       1 (Tinf 0))
               ,(ArcToT "watchdog_watch__" "watchdog_T3" 1 (Tmaxtinf))
               ,(ArcToT "watchdog_ok__" "watchdog_T4"    1 (Tinf 0))
               ,(ArcToP "watchdog_T1" "watchdog_watch__" 1)
               ,(ArcToP "watchdog_T1" "CYCLIC_start"     1)
               ,(ArcToP "watchdog_T2" "watchdog_ok__"    1)
               ,(ArcToP "watchdog_T3" "watchdog_not_ok__"1)
               ,(ArcToP "watchdog_T4" "watchdog_ok__"    1)]
