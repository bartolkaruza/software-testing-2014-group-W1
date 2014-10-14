#!/bin/sh
ghc -o ex2exm ex2exm.hs
ghc -o ex2expm ex2expm.hs
clear
echo "times for exM:"
time ./ex2exm
echo "times for expM:"
time ./ex2expm