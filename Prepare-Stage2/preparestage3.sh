#!/bin/bash
# bootstrap the stage 2 scalalr parser generator
# using the stage2 parser generator itself to generate parsing components
SCRIPTS=/Users/sufrin/GitHomes/ScalaLR/scripts
#
#
GEN=scalalrstage2
STAGE=3
export MODULE=2
NOTATION=stage2-notation.scalalr
SCALA=stage2.scala


source $SCRIPTS/PREPARESTAGE.sh
