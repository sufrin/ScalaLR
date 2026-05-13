#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH

echo Generating parser components with both boot and stage1

scalalrboot     --output=generated-boot   small.scalalr

scalalrstage1   --output=generated-stage1 small.scalalr

scalalrstage2   --output=generated-stage2 small.scalalr


echo The differences between boot and stage1 generated code should be negligible

diff -r -b generated-{boot,stage1} 

scala-cli run runsmall.scala generated-boot >   runboot.log
scala-cli run runsmall.scala generated-stage1 > runstage1.log

echo The differences between boot and stage1 logs should be negligible

diff run{boot,stage1}.log


echo The differences between stage1 and stage2 logs should be negligible
echo * some symbols in the traces have different quotes *

scala-cli runsmall.scala generated-stage2 > runstage2.log

diff run{stage1,stage2}.log

echo The differences between stage1 and stage2 generated code will be substantial
echo Stage2: .y and .xml files have uniform representations "(T-ddd)" for tokens 
echo Stage2: symbolName mapping is generated differently 
echo Stage2: reductions have  line numbers in thir comments 
dodiff=""; read -p "Show diffs? [ENTER for yes]" dodiff

[ "$dodiff" = "" ] &&  diff -r -b generated-{stage1,stage2}
