#/bin/bash

sync=""; read -p "Build the components first? [y for yes]" sync
[ "$sync" = "y" ] && sh preparestage2-sh

SCALA=stage2.scala
JAR=scalalr.jar

sync=""; read -p "Make the fat jar $PROGRAM? [ENTER for yes]" sync
[ "$sync" = "" ] && scala-cli --power package $SCALA -o $JAR --assembly --preamble=false -f
