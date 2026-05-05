#!/bin/bash
echo Making the runtinyfun app 
ROOT=../..
[ ! -e ROOT ] && ln -s $ROOT ROOT
$ROOT/scripts/scalalrbootstrap tinyfun.scalalr
scala-cli --power package -f -o runtinyfun runtinyfun.scala TinyFun.scala generated