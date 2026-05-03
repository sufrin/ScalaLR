#!/bin/bash
# make parser components for flab
ROOT=/Users/sufrin/GitHomes/ScalaLR/
MODE=${1-"-boot"}
echo Making parser components for stage1-notation $MODE
echo Depends on $ROOT/scripts/scalalrgen
$ROOT/scripts/scalalrboot --output=generated$MODE flab-notation.scalalr
echo If all has gone well, then you can synchronise generated$MODE to the source directory by answering "y"
sync=n; read -p "Synchronise: [y] " sync
test $sync = "y" && rsync -av  generated$MODE $ROOT/flab/src/main/scala/generated
