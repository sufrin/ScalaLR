# First Language Atop Bootstrap

The `flab` module defines a generator that maps the bootstrap scalalr 
language (as defined in `flab-notation.scalalr`) into the usual Scalalr 
components in Tables.scala, Scanner.scala, and Reduction.scala. 

Before it can be  compiled, suitable copies of these 
components must be prepared and placed in 

     flab/scala/scalalr/parser

There are two ways to regenerate these after `flab-notation.scalalr` has been edited:

  1. `./makeflabgeneratorwithboot` -- this uses the hand-written bootstrap
     parser to generate the bootstrap "abstract syntax" that is then
     translated by the bootstrap generator into these files. 

  2. `./makeflabgeneratorwithflab` -- this uses the LR 
     parser generated from `flab-notation` to generate the 
     flab abstract syntax that is first translated to the
     bootstrap abstract syntax and thence
     translated by the bootstrap generator into these files.

If all goes well (ie if this stage of the bootstrap hasn't gone wrong) then
these two ways should generate functionally identical files; and you will
be invited to copy them to the appropriate directory in the
`flab` module. Decide which you want; make the appropriate copy;
then recompile/rebuild the `flab` module.

## Flab *vs* Bootstrap Syntax
Although the hand-written bootstrap parser accepts a slightly more general notation
than the Scalalr-generated parser for Flab, there is no practically-useable
difference between them. 