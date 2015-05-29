#!/bin/bash
cabal sandbox init
echo Installing packages in \*current\* working directory.
echo Sleep 3 seconds now...
sleep 3
cabal update
cabal install --only-dependencies -j1
cabal configure
cabal build
cp ./dist/build/pagerank/pagerank ./executePR.sh
