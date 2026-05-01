//> using scala 2.13
//> using jar ROOT/bootstrap/target/bootstrap-0.8.0.jar
//> using jar ROOT/shared/target/shared-0.8.0.jar
//> using jar ROOT/stage2/target/stage2-0.8.0.jar 
//> using jar ROOT/utilities/target/utilities-0.8.0.jar
//> using jar ROOT/logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0


/**
 * BUILDING A RUNNABLE ASSEMBLY
 *
 *   scala-cli --power package slab.scala -o slab --assembly -f
 *  
 * AD-HOC RUN
 *
 *   scala-cli run slab.scala -- [source files]
 * 
 */
package org.sufrin.scalalr
package stage2

object main {
  def printHelp(): Unit = {
    val signature = {
      import scalalr.stage2.DialectInformation._
      s"Notation \"$name\" (for $notation) $scalalr"
    }
    println(
      s"""$signature
        |Usage: stage2 [--output=<outputpath> (default STAGE2OUTPUT)] [ <file> ...]
        | *** EVENTUALLY **** Generate parser tables from notation source files 
        |""".stripMargin
    )
  }
  def main(args: Array[String]): Unit = {
    var genargs: List[String] = List("--output=STAGE2OUTPUT")
    var boot = false
    for { arg <- args } arg match {
      case s"-h"        => printHelp()
      case s"--o=$path" => genargs = List(s"--output=$path")
      case _            => genargs = arg::genargs
    }
    val mainargs =  genargs.reverse.toArray
    try
      Generator.main(mainargs)
    catch { case scalalr.stage2.Tables.ErroneousGoto(state, symbol) =>
                   println(s"Erroneous GOTO from state $state at ${scalalr.stage2.Scanner.symbolName(symbol)}")
            case exn: Throwable =>
                   println(exn.toString)
          }
  }
}

