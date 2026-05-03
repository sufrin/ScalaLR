//> using scala 2.13
//> using jar ../bootstrap/target/bootstrap-0.8.0.jar
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

object scalalrboot {
  def printHelp(): Unit = {
    println(
      """Usage: scalalrboot [--output=<outputpath>] [ <file> ...]
        | Generate parser tables from scalalr source files using the bootstrap generator
        | and the boostrap handwritten parser.
        |""".stripMargin
    )
  }
  def main(args: Array[String]): Unit = {
    var genargs: List[String] = Nil
    var boot = false
    for { arg <- args } arg match {
      case s"-h"    => printHelp()
      case _        => genargs = arg::genargs
    }
    val mainargs =  genargs.reverse.toArray   

    bootstrap.Generator.main(mainargs) 
  }
}

