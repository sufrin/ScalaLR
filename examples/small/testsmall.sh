#!/bin/bash

ROOT=../..
PATH=$ROOT/scripts:$PATH

echo Generating parser components with both bootstrap and flab

scalalrgen -boot --output=generated-boot small.scalalr

scalalrgen -flab --output=generated-flab small.scalalr

echo The differences should only be in .html, .xml, and in the Components.scala times of origin

diff -r -b generated-*

scala-cli run runsmall.scala generated-boot > runboot.log

scala-cli runsmall.scala generated-flab > runflab.log

diff *log


