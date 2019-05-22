#!/bin/bash
cd src && ghc -outputdir bin PLCparser/CompilePlc.hs && ghc -outputdir bin PLCparser/buildsystem.hs

if [ $? -eq 0 ]; then
    cd ..
fi
