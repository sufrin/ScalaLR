#!/bin/bash
# make parser components for flab
ROOT=/Users/sufrin/GitHomes/ScalaLR/
MODE=${1-"-boot"}
echo Making parser components for flab $MODE
echo Depends on $ROOT/scripts/scalalrgen
$ROOT/scripts/scalalrgen $MODE --output=generated$MODE flab-notation.scalalr
echo Now sync with the generated components of the main scala source
echo rsync -av  generated$MODE $ROOT/flab/src/main/scala/generated

