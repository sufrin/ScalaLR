#!/bin/bash
# bootstrap the stage 2 scalalr parser generator
# using the stage1 parser generator to generate parsing components
SCRIPTS=/Users/sufrin/GitHomes/ScalaLR/scripts
#
#
GEN=scalalrstage1
STAGE=2
export MODULE=2
NOTATION=stage2-notation.scalalr
SCALA=stage2.scala

export SUFFIX=""

source $SCRIPTS/PREPARESTAGE.sh

 