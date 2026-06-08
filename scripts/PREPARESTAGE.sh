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
PROGRAM=stage$STAGE$SUFFIX

echo
echo Making parser components for stage$MOD: stage$STAGE=$GEN"($NOTATION)"
echo


$SCRIPTS/$GEN -html --output=generated-$GEN $NOTATION
sync=n; read -p "Install generated-$GEN in the stage$MOD module source code? [ENTER for yes]" sync
[ "$sync" = "" ] && rm -rfv $ROOT/stage$STAGE/src/main/scala/generated/* && rsync -av generated-$GEN/ $ROOT/stage$MOD/src/main/scala/generated/ 
if [ "$sync" = "" ]
then
  sync=""; read -p "Rebuild the stage$MOD module incrementally with sbt [ENTER for yes]? " sync
  if [ "$sync" = "" ] && ( cd $ROOT ; sbt "stage$MOD / package")
  then
   read -p "Make the binary $PROGRAM? [ENTER for yes]" sync
   if [ "$sync" = "" ] && scala-cli --power package $SCALA -o $PROGRAM --assembly -f
   then
        read -p "Install the binary $PROGRAM in $ROOT/scripts? [ENTER for yes]" sync
        [ "$sync" = "" ] && cp $PROGRAM $ROOT/scripts/scalalr$PROGRAM
   fi
  fi
fi

  