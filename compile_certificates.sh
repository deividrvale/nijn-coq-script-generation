#!/bin/bash

FILES="$HOME/Documents/wanda/certificates/*"
for f in $FILES
do
    g=${f::${#f}-6}
    dune exec -- onijn $f -o $g.v
    mv $g.v "$HOME/Documents/wanda/coq_certificates"
done
