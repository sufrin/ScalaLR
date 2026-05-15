#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH
[ ! -e ROOT  ] && ln -s $ROOT ROOT

scalalrstage2   -Lsym -html --output=generated small.scalalr
scalalrstage2   -Lsym -html --output=generated-small small-hand.scalalr
#exit
scala-cli run runsmall.scala generated
