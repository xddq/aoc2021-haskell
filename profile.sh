#!/bin/bash
#
# Author: Pierre Dahmani
# Created: 04.01.2022
#
# Description: builds projects with profiling enabled. (could also use a flag
# inside .cabal config file)

cabal build --enable-profiling

# profiles Day14.hs. -p will create the .prof file with the result.
./dist-newstyle/build/x86_64-linux/ghc-8.10.7/aoc2021-haskell-0.1.0.0/x/Day14/build/Day14/Day14 +RTS -p
