#!/bin/bash
ROOT=~/GitHomes/ScalaLR
echo Making parser components for flab $MODE
echo Depends on $ROOT/scripts/scalalrgen
$ROOT/scripts/scalalrgen $MODE --output=generated$MODE slab-notation.scalalr
echo Now sync with the generated components of the main scala source
echo rsync -av  generated$MODE $ROOT/slab/src/main/scala/generated

