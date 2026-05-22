#!/bin/bash
#
# Common script to prepare one of the stages of a bootstrap
#
ROOT=~/GitHomes/ScalaLR
SCRIPTS=$ROOT/scripts
[ -e ROOT ] || ln -s -f $ROOT ROOT
#
#GEN=scalalrboot                    the generator to be used
#STAGE=1                            the number or name of the stage
#MODULE=$STAGE
#NOTATION=stage1-notation.scalalr   the notation desciption file
#SCALA=stage$STAGE.scala            the name of the scala driver file
#
MOD=${MODULE-$STAGE}
echo Making parser components for stage $STAGE with stage$MOD source $GEN"($NOTATION)"
$SCRIPTS/$GEN -c --output=generated-$GEN $NOTATION
sync=n; read -p "Install the generated components in the stage$STAGE source code? [ENTER for yes]" sync
[ "$sync" = "" ] && rm -rf $ROOT/stage$STAGE/src/main/scala/generated/ && rsync -av generated-$GEN $ROOT/stage$STAGE/src/main/scala/generated/
if [ "$sync" = "" ]
then
  sync=n; read -p "Rebuild the stage$MOD module incrementally with sbt [ENTER for yes]? " sync
  [ "$sync" = "" ] && ( cd $ROOT ; sbt "stage$MOD / clean; stage$MOD / package")
  if [ "$sync" = "" ]
  then
   read -p "Make the binary stage$STAGE? [ENTER for yes]" sync
   if [ "$sync" = "" ] && scala-cli --power package $SCALA -o stage$STAGE --assembly -f
   then
        read -p "Install the binary stage$STAGE in $ROOT/scripts? [ENTER for yes]" sync
        [ "$sync" = "" ] && cp stage$STAGE $ROOT/scripts/scalalrstage$STAGE
   fi
  fi
fi

  