#!/bin/bash

bnfc --haskell-gadt --glr --ghc -o src -p PLCparser grammar/plc.cf 
sed -i '21 s/\$u = \[\\0-\\255\]/\$u = \[\\x00-\\xffff\]/' src/PLCparser/LexPlc.x
sed -i '29 a "{" [$u # \\}]* "}" ;' src/PLCparser/LexPlc.x
alex -g -i src/PLCparser/LexPlc.x

# change line 4 of output/PLCparser/ParPlc.y from "-- module name filled in by Happy" to "module PLCparser.ParPlc where"
sed -i '4 s/-- module name filled in by Happy/module PLCparser.ParPlc where/' src/PLCparser/ParPlc.y
happy -gca --glr --decode -i src/PLCparser/ParPlc.y
