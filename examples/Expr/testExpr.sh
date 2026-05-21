#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH
[ ! -e ROOT  ] && ln -s $ROOT ROOT

echo Generating parser components 

scalalrboot   --output=generated-boot expr.scalalr

scalalrstage1 --output=generated-stage1 expr.scalalr

scalalrstage2 -html --output=generated-stage2 expr.scalalr

echo  THESE DIFFS SHOULD BE NEGLIGIBLE

diff -r -b generated-{boot,stage1}

scala-cli run runexpr.scala generated-boot   > runboot.log
scala-cli run runexpr.scala generated-stage1 > runstage1.log
scala-cli run runexpr.scala generated-stage2 > runstage2.log

if ( diff3 *log )
then
  echo "No differences in the logs"
else
  "UNEXPECTED differences in the logs"
fi


