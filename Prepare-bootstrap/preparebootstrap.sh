#!/bin/bash
# Makes a command-line tool for scalalr using only the bootstrap components
# DEPENDS ON scala-cli and sbt being installed
# 
ROOT=/Users/sufrin/GitHomes/ScalaLR/
echo Making a command-line generator for scalalr components
(cd $ROOT; sbt "bootstrap / clean ; bootstrap / package")
scala-cli --power package scalalrboot.scala -o scalalrboot --assembly -f
echo If all has gone well you can synchronise this with scripts by answering "y" below
sync=n; read -p "Synchronise? [y for yes]: " sync
test $sync = "y" && rsync -av ./scalalrboot ../scripts/
