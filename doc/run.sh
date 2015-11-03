#!/bin/sh

bnfc -m --haskell caronch-v2.cf
make
./TestCaronchV example-v2.caronch
