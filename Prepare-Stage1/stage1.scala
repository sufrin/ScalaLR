//> using scala 2.13
//> using jar ROOT/bootstrap/target/bootstrap-0.8.0.jar
//> using jar ROOT/shared/target/shared-0.8.0.jar
//> using jar ROOT/stage1/target/stage1-0.8.0.jar
//> using jar ROOT/utilities/target/utilities-0.8.0.jar
//> using jar ROOT/logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0


/**
 * BUILDING A RUNNABLE ASSEMBLY
 *
 *   scala-cli --power package stage1a.scala -o slab --assembly -f
 *  
 * AD-HOC RUN
 *
 *   scala-cli run stage1a.scala -- [source files]
 * 
 */
package org.sufrin.scalalr
package stage1

object main {
  def printHelp(): Unit = {
    val signature = {
      import org.sufrin.scalalr.stage1.ScalaLR.DialectInformation._
      s"$name: $notation ($scalalr)"
    }
    println(
      s"""$signature
        |Usage: stage1 [--output=<outputpath> (default STAGE1OUTPUT) | -o <outputpath>] [ <file> ...]cleandir""".stripMargin
    )
  }
  def main(args: Array[String]): Unit = {
    var genargs: List[String] = List("--output=STAGE1OUTPUT")
    var boot = false
    for { arg <- args } arg match {
      case s"-h"        => printHelp()
      case _            => genargs = arg::genargs
    }
    val mainargs =  genargs.reverse.toArray
    try
      org.sufrin.scalalr.stage1.Generator.main(mainargs)
    catch { //case org.sufrin.scalalr.ErroneousGoto(state, symbol) =>
            //   println(s"Erroneous GOTO from state $state at ${(symbol)}")
            case exn: Throwable => println(exn)
          }
  }
}

