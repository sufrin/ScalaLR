#!/bin/bash
ROOT=../..
[ ! -e ROOT ] && ln -s $ROOT ROOT
for STAGE in stage1 stage2
do 
  echo Making a runtinyfun-$STAGE app using scalalr$STAGE 
  $ROOT/scripts/scalalr$STAGE --output=generated-$STAGE tinyfun.scalalr
  scala-cli --power package -f -o runtinyfun-$STAGE runtinyfun.scala TinyFun.scala generated-$STAGE
done
