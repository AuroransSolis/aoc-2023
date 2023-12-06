#!/bin/zsh

for file in $(fd --extension='hs' . ./haskell-src)
do
    fourmolu --mode inplace $file
done
