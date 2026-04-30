#!/bin/bash
echo Making the runtinyfun app with scalalrlifeboat
ROOT=../..
[ ! -e ROOT ] && ln -s $ROOT ROOT
$ROOT/LIFEBOAT/scalalrlifeboat tinyfun.scalalr
scala-cli --power package -f -o runtinyfun runtinyfun.scala TinyFun.scala generated