package org.sufrin.scalalr
package stage2


import org.sufrin.SourceLocation._
import org.sufrin.utility._

/**
 * An App subclass to simplify construction of `scalalr` tests. For example:
 * {{{
 *   object test1 extends Test("")("""
 *    %tables    ielr
 *    %notation  stage2test
 *    %package   scalalr.stage2test
 *    %path      "parser"
 *
 *    %rules
 *    Rule1: Unit = S1 S2 S3 { Code };
 *    Rule2: Unit = a: S4 b: S5 { () }
 * """)
 *
 * }}}
 *
 * @param counterExamples generate detailed report on conflicts
 * @param html    generate grammar report as html as well as text
 * @param log     log the parse
 * @param pretty  pretty print the tree
 * @param output  prefix files directory
 * @param source  text of the source
 * @param loc     IMPLICIT location of the Test in Scala source
 *
 * @see GeneratorTests for longer examples.
 */
class Test(args: String="")(source: String)(implicit loc: SourceLocation) extends App {
  val effectiveArgs = List("--output=TEST-GENERATED")++args.split(' ').toList ++ List("-#", (loc.line).toString, "-##", (loc.offset).toString, "-s", source)
  Generator.main(effectiveArgs.toArray)
}


