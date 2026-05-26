
# Generate the runtime library support for scalalr-generated parsers
scala-cli --power package --list-main-classes scalalrruntime.scala
scala-cli --power package --preamble=false --assembly scalalrruntime.scala -o scalalrruntime.jar -f
