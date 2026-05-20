#!/bin/bash
# bootstrap the stage 2 scalalr parser generator
# using the stage1 parser generator to generate parsing components
SCRIPTS=/Users/sufrin/GitHomes/ScalaLR/scripts
#
#
GEN=scalalrstage1
STAGE=2
NOTATION=stage2-notation.scalalr
SCALA=stage$STAGE.scala


source $SCRIPTS/PREPARESTAGE.sh

 