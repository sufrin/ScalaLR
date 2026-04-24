## The Command Line project

This project contains the wherewithal to generate a *self-contained* scalalr code generator

    scalalrgen
  
to be run from the command-line (or a shell script). 

### Building

In the root directory of `ScalaLR` run

    sbt "clean ; root package"

to regenerate all module jar files. Check that the version number in `build.sbt` is the
same as the version number mentioned in the scala-cli directive comments of the 
source code of `ScalaLR/COMMANDLINE/scalalrgen.scala` (adjust the latter if necessary)

In this directory `ScalaLR/COMMANDLINE`  run

    scala-cli --power package scalalrgen.scala -o scalalrgen --assembly

and ... you're done.

#### Acknowledgement
The authors of scala-cli, whoever they are, deserve our gratitude for taking the bull
by the horns and building a program that lets us manage scala production builds
straightforwardly.

