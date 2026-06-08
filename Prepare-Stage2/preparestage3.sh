#!/bin/bash
# bootstrap the stage 2 scalalr parser generator
# using the stage2 parser generator itself to generate parsing components
SCRIPTS=/Users/sufrin/GitHomes/ScalaLR/scripts
#
#
GEN=scalalrstage2-sh
STAGE=3
export MODULE=2
NOTATION=stage3-notation.scalalr
SCALA=stage2.scala

export SUFFIX=-sh

source $SCRIPTS/PREPARESTAGE.sh
