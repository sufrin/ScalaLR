#!/bin/bash
# bootstrap the stage 2 scalalr parser generator
# using the stage2 parser generator itself to generate parsing components
SCRIPTS=/Users/sufrin/GitHomes/ScalaLR/scripts
#
#
GEN=scalalrstage2
STAGE=2
export MODULE=2
NOTATION=stage2-notation.scalalr
SCALA=stage2.scala

export SUFFIX=-sh

echo STAGE2 SELF-HOSTED

source $SCRIPTS/PREPARESTAGE.sh
