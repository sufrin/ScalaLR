# First Language atop Bootstrap

The `flab` module defines a generator that maps the bootstrap scalalr 
language (as defined in `flab-notation.scalalr`) into the usual Scalalr 
components in Components.scala, Tables.scala, Scanner.scala, and Reduction.scala. 

Before it can be  compiled, suitable copies of these 
components must be prepared and placed in 

     flab/scala/scalalr/parser

The best way to do this is to run one of the following (short) commands from the 
ScalaLR project `root/PREPAREFLAB`

     ./prepareflab.sh -boot
     ./prepareflab.sh -flab

If all goes well (ie if this stage of the bootstrap hasn't gone wrong) then
these should generate functionally identical files; and you will
be invited to copy the generated material to the appropriate directory in the
`flab` module, and if you accept the invitation you will be
invited to have the `flab` module recompiled.

## Flab *vs* Bootstrap Syntax
Although the handwritten bootstrap parser accepts a slightly more general notation
than the Scalalr-generated parser for Flab, there is no practically-useable
difference between them. 

