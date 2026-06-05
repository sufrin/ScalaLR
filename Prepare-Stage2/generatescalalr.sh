#/bin/bash

sync=""; read -p "Build the components first? [y for yes]" sync
[ "$sync" = "y" ] && sh preparestage2-sh.sh

SCALA=stage2.scala
JAR=scalalr

sync=""; read -p "Make a RUNNABLE $JAR? [ENTER for yes]" sync
[ "$sync" = "" ] && scala-cli --power package $SCALA -o $JAR --assembly --preamble=true -f && chmod +x $JAR
