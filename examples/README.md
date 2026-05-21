### ScalaLR Examples



#### Preparation for running examples:
Each example, and much of the build process for `scalalr`, depends on the presence
of `scala-cli`

Ensure that the core libraries have been packaged, and the
code-generator stages that you are interested in are in `$ROOT/scripts`.

All the `testXXX.sh` scripts are prefixed by the following, to establish
a symbolic link to the ROOT of the distribution tree: 
````
#!/bin/bash
ROOT=../..
PATH=$ROOT/scripts:$PATH
[ ! -e ROOT  ] && ln -s $ROOT ROOT
````

This enables the use of `scala-cli` dependency and option descriptions at the head of
the `runXXX.scala` files. Like this
```scala
//> using scala 2.13
//> using jar ROOT/shared/target/shared-0.8.0.jar
//> using jar ROOT/utilities/target/utilities-0.8.0.jar
//> using jar ROOT/logging-api/lib/Logging.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0
//> using options -deprecation -feature -unchecked

package org.sufrin.scalalr

object runexpr  { ... }

```

## Expr
Exercises the core scalalr code generator(s) and parsing automata on a tiny
and incomplete arithmetic-expression notation. Useful during the initial bootstrapping
as a sanity check.

    cd examples/Expr
    sh testexpr.sh

~~## Small~~
Exercises the core scalalr code generator(s) and parsing automata on an even smaller
notation than Expr. Useful during the initial bootstrapping
as a sanity check.

    cd examples/small
    sh testsmall.sh


## RoseTree-Small
Exercises the core scalalr (stage2) code generator and parsing automata with the `small`
notation description, but each reduction now results in a rose-tree that
describes it, and the "normal" reduction expressions are bypassed. 

    cd examples/rosetree-small
    sh testrosetreesmall.sh

## TinyFun
A small "interactive" language of arithmetic expressions and assignments. 
Uses the stage2 code generators.

    cd examples/TinyFun
    sh maketinyfun.sh           # makes a complete application: runtinyfun

    ./runtinyfun [options]      # runs the application

Options are:

    -h -- print help text
    -l -- show the parsing automaton steps
    -p -- use the "push" automaton


