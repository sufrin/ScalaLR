#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH

echo Generating parser components with both bootstrap and flab

scalalrgen -boot --output=generated-boot expr.scalalr

scalalrgen -flab --output=generated-flab expr.scalalr

echo The differences should only be in .html, .xml, and in the times of origin

diff -r -b generated-*

scala-cli run runexpr.scala generated-boot > runboot.log

scala-cli run runexpr.scala generated-flab > runflab.log

if ( diff -b *log )
then
  echo "No differences in the logs"
else
  "UNEXPECTED differences in the logs"
fi


