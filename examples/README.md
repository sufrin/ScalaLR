### ScalaLR Examples



#### Preparation for running examples:
Each example, and much of the build process for `scalalr,` depends on the presence
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

or if you have acquired a distribution with `scalalr.jar` and `scalalrruntime.jar` in
the `ROOT`
````scala
//> using scala 2.13
//> using jar ROOT/scalalrruntime.jar
//> using dep org.scala-lang.modules::scala-xml::2.4.0
//> using options -deprecation -feature -unchecked
````

## Expr
Exercises the core scalalr code generator(s) and parsing automata on a tiny
and incomplete arithmetic-expression notation. Useful during the initial bootstrapping
as a sanity check.

    cd examples/Expr
    sh testexpr.sh

## Small
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

### Read-eval-print
An important point of interest of TinyFun is the way the "read-eval-print" loop is
implemented in the grammar itself.
````scala
  loop: Unit =  %empty          { () }
             |  loop command NL { () }


  command: Unit = expressions   { run($expressions, "> ")  }
                | "QUIT"        { System.exit(0) }
````

The `command` production is a hook that is parsed by parsing an expr sequence, then
reduced when the NL appears to its right (as the lookahead symbol in `loop`).
It is at the reduction that the parsed list of expressions is run, its value is
printed, and the user is re-prompted.

### Top-level error-recovery
The top-level "runner" method is designed to recover from parsing errors by
abandoning the parse. For the moment this is the most expedient way of
building an interactive interface. The variable store persists across
such error reports.
````scala
      import LRParser._
      var state: ParseState = RUNNING
      while (state == RUNNING) {
        val scanner = Scanner(SourceTextCursor(Paths.get(file)))
        val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
        parser.logState = log
        parser.attemptRecovery = recover
        state = parser.run(scanner.next)
        state match {
          case ERRONEOUS(message) =>
            println(message)
            state = RUNNING
          case _ => state = RUNNING
        }
      }
````