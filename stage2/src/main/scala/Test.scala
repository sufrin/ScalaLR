package org.sufrin.scalalr
package stage2


import org.sufrin.SourceLocation._
import org.sufrin.utility._

/**
 * An App subclass to support simple scalalr tests.
 *
 * @see GeneratorTests
 *
 * @param counterExamples generate detailed report on conflicts
 * @param html    generate grammar report as html as well as text
 * @param log     log the parse
 * @param pretty  pretty print the tree
 * @param output  prefix files directory
 * @param source  text of the source
 * @param loc     IMPLICIT location of the Test in Scala source
 */
class Test(args: String="")(source: String)(implicit loc: SourceLocation) extends App {
  val effectiveArgs = List("--output=TEST-GENERATED")++args.split(' ').toList ++ List("-#", (loc.line-1).toString, "-s", source)
  Generator.main(effectiveArgs.toArray)
}

