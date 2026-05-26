//> using scala 2.13
//> using packaging.provided "org.scala-lang:scala-library"
//> using jar ROOT/shared/target/shared-0.8.0.jar
//> using jar ROOT/utilities/target/utilities-0.8.0.jar
//> using jar ROOT/logging-api/lib/Logging.jar

/*
    Building the runtime library

    scala-cli --power package --preamble=false --assembly scalalrruntime.scala -o scalalrruntime.jar -f
*/
package org.sufrin.scalalr
object scalalrruntime {
}
