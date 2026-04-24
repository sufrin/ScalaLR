//> using scala 2.13
//> using jar ../bootstrap/target/bootstrap-0.8.0.jar
//> using jar ../scalalr/target/scalalr-0.8.0.jar
//> using jar ../shared/target/shared-0.8.0.jar
//> using jar ../utilities/target/utilities-0.8.0.jar
//> using jar ../logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0


/**
 * BUILDING A RUNNABLE ASSEMBLY
 *
 *   scala-cli --power package scalalrgen.scala -o scalalrgen --assembly
 *  
 * AD-HOC RUN
 *
 *   scala-cli run scalalrgen.scala -- [source files]
 * 
 */
package org.sufrin.scalalr

object scalalrgen {
  def printHelp(): Unit = {
    println(
      """Usage: scalalrgen [-flab | -h | [--output=<outputpath>] [ <file> ...]
        | -flab uses the self-generated parser to parse the scalalr source
        | -boot uses the handwritten parser to parse the scalalr source
        |""".stripMargin
    )
  }
  def main(args: Array[String]): Unit = {
    var genargs: List[String] = Nil
    var flab = false
    for { arg <- args } arg match {
      case s"-h"    => printHelp()
      case "-flab"  => flab = true
      case "-boot"  => flab = false
      case _        => genargs = arg::genargs
    }
    val mainargs =  genargs.reverse.toArray   

    if (flab) generate.main(mainargs) else translateFLAB.main(mainargs)
  }
}

