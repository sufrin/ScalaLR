#!/bin/bash
ROOT=~/GitHomes/ScalaLR
SCRIPTS=$ROOT/scripts
[ -e ROOT ] || ln -s -f $ROOT ROOT
#
GEN=scalalrboot
STAGE=1
NOTATION=stage1-notation.scalalr
SCALA=stage$STAGE.scala
#
echo Making parser components for stage $STAGE with $GEN"($NOTATION)"
$SCRIPTS/$GEN --output=generated-$GEN $NOTATION
sync=n; read -p "Install the generated components in the stage$STAGE source code? [CR for yes]" sync
[ "$sync" = "" ] && rm -rf $ROOT/stage$STAGE/src/main/scala/generated/ && rsync -av generated-$GEN $ROOT/stage$STAGE/src/main/scala/generated/
if [ "$sync" = "" ]
then
  sync=n; read -p "Rebuild the stage$STAGE module incrementally with sbt? " sync       
  [ "$sync" = "" ] && ( cd $ROOT ; sbt "stage$STAGE / clean; stage$STAGE / package")
  if [ "$sync" = "" ]
  then
   read -p "Make the binary stage$STAGE? " sync
   [ "$sync" = "" ] && scala-cli --power package $SCALA -o stage$STAGE --assembly -f
  fi
fi

  