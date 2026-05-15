#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH
[ ! -e ROOT  ] && ln -s $ROOT ROOT

scalalrstage2   -Lsym -html --output=generated small.scalalr
scala-cli run runsmall.scala generated

scalalrstage2   -Lsym -html --output=generated-small small-hand.scalalr
scala-cli run runsmall.scala generated-small
exit
