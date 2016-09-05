#!/bin/bash

GHC_VER=`ghc -V | awk '{ print $NF }'`

if [ -z "$1" ]
then
    echo "Usage: $0 <file>"
else
    ghc --make Prof.hs -O2 -fforce-recomp -rtsopts -prof -auto-all -caf-all \
	-package-conf "../cabal-dev/packages-${GHC_VER}.conf" \
	-hide-package zip-archive

    ./Prof "$1" +RTS -p -hc -sProf.summ

    hp2ps -c Prof.hp

    rm -f Prof.aux Prof.hi Prof.o Prof.tix
fi
