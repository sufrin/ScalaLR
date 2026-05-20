#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH
[ ! -e ROOT  ] && ln -s $ROOT ROOT

echo 

scalalrstage2   -Lsym -html -rose --output=generated small.scalalr
scala-cli run runsmall.scala generated

scalalrstage2   -Lsym -html -rose --output=generated-hand small-hand.scalalr
scala-cli run runsmall.scala generated-hand
exit
