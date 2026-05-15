#!/bin/bash
ROOT=../..
[ ! -e ROOT ] && ln -s $ROOT ROOT
(cd ROOT; sbt package)
for STAGE in stage2
do 
  echo Making a runtinyfun-$STAGE app using scalalr$STAGE 
  $ROOT/scripts/scalalr$STAGE -html --output=generated-$STAGE tinyfun.scalalr
  scala-cli --power package -f -o runtinyfun-$STAGE runtinyfun.scala TinyFun.scala generated-$STAGE
done
