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
    import scalalr.stage2.NotationInformation._
    println(s"ScalaLR ($signature) of $generated.")
  }
  def main(args: Array[String]): Unit = {
    try {
      printHelp()
      Generator.main(args)
    } catch { case org.sufrin.scalalr.ErroneousGoto(state, symbol) =>
                   println(s"Erroneous GOTO from state $state at ${scalalr.stage2.Scanner.symbolName(symbol)}")
            case exn: Throwable =>
                   exn.printStackTrace()
          }
  }
}

