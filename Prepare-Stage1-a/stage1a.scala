//> using scala 2.13
//> using jar ../bootstrap/target/bootstrap-0.8.0.jar
//> using jar ../shared/target/shared-0.8.0.jar
//> using jar ../slab/target/slab-0.8.0.jar
//> using jar ../utilities/target/utilities-0.8.0.jar
//> using jar ../logging-api/lib/Logging.jar
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

object slabMain {
  def printHelp(): Unit = {
    val signature = {
      import scalalr.slab.parser.DialectInformation._
      s"Notation \"$name\" (for $notation) $scalalr"
    }
    println(
      s"""$signature
        |Usage: slab [--output=<outputpath> (default SLABOUTPUT)] [ <file> ...]
        | Generate parser tables from scalalr source files using the bootstrap generator
        | and the slab parser.
        |""".stripMargin
    )
  }
  def main(args: Array[String]): Unit = {
    var genargs: List[String] = List("--output=SLABOUTPUT")
    var boot = false
    for { arg <- args } arg match {
      case s"-h"        => printHelp()
      case s"--o=$path" => genargs = List(s"--output=$path")
      case _            => genargs = arg::genargs
    }
    val mainargs =  genargs.reverse.toArray
    slab.Generator.main(mainargs)
  }
}

