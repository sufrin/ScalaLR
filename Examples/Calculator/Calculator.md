### ScalaLR Example: Calculator

This is an example of the front end of a potentially interactive functional 
calculator. It illustrates the following techniques, and the many of details alluded
to below can be found in the file `calculator.scalalr`

1. How to embed a lexical scanner in the description of a notation. See
the first `%include` directive for detail.
The scanner there is a specialization of the generic `ScannerCore` provided
in the runtime library. Although it is not *necessary* to use the library scanner 
core definitions, it can be very helpful to do so when one is working on parsing
a more-or-less conventional language; and the scanner cores provided in
the library provide (efficient) support for "say it once" definitions of
lexical symbols. 


2. How to include support for aspects of the concrete syntax that might
otherwise lead to shift/reduce conflicts in a ScalaLR-defined parser.
For detail, see:

   1. the function `MakeEquation` in the second "%include" directive, and
   1. the definitions in the `%rules` section of 
```scala
        whereExpr
        equation
        equation
```             
   
3. How to define a grammar that, in concert with the driver program for
the resulting parser, can be used interactively. The detail is in
the definitions below, but needs some explanation. The importat thing to
understand is that the Scala expression denoting the result of parsing
a production is evaluated at the moment the production is recognised by the
parse. It may (but usually does not) have side-effects, and its value
normally becomes the result of the production. 
        
   1. A session starts with an optional `NL` (newline), then continues
   with one or more `anAction` productions, each followed by an NL. When
   the NL is seen, the appropriate result expression of `anAction` is
   evaluated.
   2. If the `anAction` was `onePhrase`, then it is passed first to `layout,`
   and then to `translate.` The final expression `Instruct(())` yields a special
   `Shortcut` value that causes the parsing automaton to terminate the 
   current instance of the parser  (`parser.run`) running in the
   main loop of the program, but (`state=RUNNING`) to
   restart the session with a new parser that will proceed from the current
   position in the input reached by `scanner`.
   3. If the `anAction` was a `syntaxDef,` then its result is passed
   `(Instruct($syntaxDef))`
   "up" to the main loop of the program, that invokes the appropriate 
   "lexical extensibility" feature of the scanner before restarting  `(state=RUNNING)`
   the session (with a new parser ...) 
   4. If an `ENDOFFILE` is reached, then the result `(Accept(()))` of
   the corresponding production is passed up to the main loop of the program --
   has the effect of terminating it (after possibly expressing a valediction) -- because `state=/=RUNNING`
   5. Each single phrase `(onePhrase)` described by the grammar yields the corresponding
   abstract syntax tree: this will be either an `Expr` or a `Definition`, and
   the methods `layout` and `translate` are defined to do the right thing with the
   tree. 
```scala
        //
        // From calculator.scalalr
        //
        session:   Unit  = (NL)? (anAction NL)+ ENDOFFILE  { () }       §i
        
        
        anAction: Shortcut   = 
           | onePhrase { layout($onePhrase); 
                         translate($onePhrase).prettyPrint(); 
                         Instruct(()) 
                       }                                                §ii
           | syntaxDef { Instruct($syntaxDef) }                         §iii                          
           | ENDOFFILE { Accept(()) }                                   §iv                                                         
        
        onePhrase: TREE = expr                    => $expr              §v
                        | equation                => $equation
                        | typeDef                 => $typeDef
                        | guardedequation         => $guardedequation
```
        
```
        //
        // From Calculator.scala: the main loop of the interactive calculator
        // Each iteration starts with a fresh parser.run that uses the same scanner
        // and yields a state to be acted on.
        //
        val scanner = makeScanner(input)
        while (state == RUNNING) {
          val parser = LRParser.Pull[Token](Components)(scanner.sourceLocation)
        
                parser.logState         = log
                parser.attemptRecovery  = recover
                parser.logRecovery      = recover
                parser.locateError      = true
        
                state = parser.run(scanner.next)
        
                state match {
                  case ERRONEOUS(message) =>
                    println(message)
                    if (interactive) state = RUNNING
                  case INSTRUCTED(message) =>                           §ii, iii
                    message match {
                      §iii
                      case List("syntax", newSym: String, oldSym: calculator.syntax.Named) =>
                           calculator.Calc.Scanner.Extensibility.extend(scanner, newSym, oldSym.name)
                           state = RUNNING
                      §ii
                      case () =>
                           state = RUNNING
                    }
                  case ACCEPTED(()) =>                                  §iv
                    if (path=="/dev/console") println("Bye")
                  case _ =>
                    state = RUNNING
                }
              }
         }
```