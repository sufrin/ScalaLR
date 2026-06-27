package org.sufrin.scalalr
package stage2

object Messages {
  var fatalErrors: Int = 0
  def inform(what: String): Unit  = System.err.println(s" INFORM  $what")
  def warning(what: String): Unit = System.err.println(s" WARNING $what")
  def fatal(message: String): Unit = {
    System.err.println(s"*ERROR   $message")
    fatalErrors += 1
  }
  def halt(): Unit = {
    System.err.println(s"*HALTED: No code generation")
    System.exit(1)
  }
  def reset(): Unit = fatalErrors = 0
  def noneFatal: Boolean = fatalErrors == 0

}