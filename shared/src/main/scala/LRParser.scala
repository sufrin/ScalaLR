package org.sufrin.scalalr
/**
 * Symbols have `symbol` codes as well as (often ignorable) values
 */
trait Lexeme { val value: Any ; val symbol: Int }
case class SourceLocation(line: Int, col: Int) { override def toString: String = s"@$line.$col"}

/**
 * This module provides two methods of parsing material according to a grammar specified
 * in the `scalalr` notation. The `Pull` algorithm parses until the material is accepted or the first
 * error is found. The `Push` algorithm is invoked step-by-step, with a lexeme at a time, until
 * a step yields acceptance or an error.
 *
 */

object LRParser {

  type State = Int
  type Terminal = Int
  type NonTerminal = Int
  type Symbol = Int

  import Action._

  sealed trait ParseState
  case object RUNNING extends ParseState
  case object NEXTSTEP extends ParseState
  case class  ERRONEOUS(diagnosis: String) extends ParseState
  case class  ACCEPTED(values: Any) extends ParseState // state of the value stack

  /**
   * Core of both the Pull and Push parser automata.
   * @param action maps (current) state to (current) lookahead terminal to an action
   * @param goto  maps (current) state to (current top-of-stack) nonterminal to next state
   * @param reduction maps each production number to the function that maps the values on its rhs to the single value that will replace them
   * @param symbolName maps symbols to their (string) names -- used in logging
   * @param sourceLocation generates a representation of the current source location when it is called
   * @tparam Lex representation of symbols as returned by the scanner
   */
  class LRParser[Lex <: Lexeme](
                 val action: State=>Terminal=>Action,
                 val goto: State => NonTerminal => State,
                 val reduction: (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any],
                 val symbolName: Map[Symbol, String],
                 val sourceLocation: ()=>SourceLocation) {

    import collection.mutable.Stack

    // Invariant: symbols and values have the same length, one less than states.
    // (actually it doesn't matter if you keep all stacks the same length)
    val symbols = new Stack[Symbol]
    val values  = new Stack[Any]
    val states  = new Stack[State]
    val locations = new Stack[SourceLocation] //
    var parseState: ParseState = RUNNING
    var logState: Boolean = false
    var currentState = 0

    val errorSymbol: Int = 1 // MUST BE IDENTICAL TO THE BISON CODE FOR THE VIRTUAL TERMINAL SYMBOL "error"

    /**
     * Find a state on the stack of states that will shift "error"
     */
    def findRecoveryState(): Boolean = {
      println(s"Recovering from:\n${mkString}")
      def cannotShiftError(state: State): Boolean = !(action(state)(errorSymbol).isInstanceOf[SHIFT])
      while (states.nonEmpty && cannotShiftError(states.top)) {
        states.pop()
        values.pop()
        symbols.pop()
        locations.pop()
      }
      states.nonEmpty
    }

    /**
     * A rudimentary diagnostic message for use when a parse error is discovered
     * @param currentInput
     * @param currentState
     * @return
     */
    def diagnosis(currentInput: Lex, currentState: State): String = {
      val acceptable = for {(symbol, name) <- symbolName if action(currentState)(symbol) != ERROR } yield name
      val oneOf = if (acceptable.size>1) " one of: " else ": "
      s"Parse error at ${sourceLocation()} ${currentInput} when expecting$oneOf ${acceptable.mkString(" ")}"
    }

    /**
     * A 3-line string describing the symbol, value, and state stacks. The latter
     * is invariantly of length one more than the others -- which are of equal length.
     * @return
     */
    def mkString: String = {
        val sys = symbols.toSeq.map(symbolName).mkString("Symbols: ", ", ", "")
        val sts = states.toSeq.map(_.toString).mkString ("States:  ", ", ", "")
        val vls = values.toSeq.map(_.toString).mkString ("Values:  ", ", ", "")
        val lox = locations.toSeq.map(_.toString).mkString ("Locations:  ", ", ", "")
        s"$sts\n$sys\n$vls"
    }
  }



  /**
   * Parse-until-accept algorithm. The protocol is exemplified by
   * {{{
   *     val scanner = Scanner(SourceTextCursor(source))
   *     def next(): Token = if (scanner.hasNext) scanner.next() else SmallparseScanner.$end
   *     val parser  = new LRParser.Pull[Token](ACTIONTABLE, GOTOTABLE, reduction(_), ...)
   *     println(parser.run(next(_)))
   * }}}
   *
   * If the parser run terminates with `ACCEPTED(value)` then the parse was successful, and
   * the value is the parse result as specified by the action expressions.
   *
   */
  class Pull[Lex <: Lexeme](action: State=>Terminal=>Action,
                            goto: State => NonTerminal => State,
                            reduction: (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any],
                            symbolName: Map[Symbol,String],
                            sourceLocation: ()=>SourceLocation)
    extends LRParser[Lex](action, goto, reduction, symbolName, sourceLocation)  {

    import collection.mutable.Stack

    def run(next: () => Lex): ParseState = {
      //symbols.push(0)
      //values.push(())
      states.push(0)
      var location = sourceLocation()
      var input = next()

      while (parseState == RUNNING) {
        var currentState = states.top
        val act = action(currentState)(input.symbol)
        if (logState) println(s"$currentState: ${symbolName(input.symbol)}=«$input»  $act")
        act match {
          case _: GOTO => //TODO: Not really an action
          case SHIFT(newState) =>
            states.push(newState)
            symbols.push(input.symbol)
            values.push(input.value)
            locations.push(location)
            input = next()
            location = sourceLocation()
          case ACCEPT =>
            parseState = ACCEPTED(values(1))
          case ERROR =>
            println(s"Syntax error: ${sourceLocation()}")
            throw new Error (diagnosis(input, currentState))
            parseState = ERRONEOUS(diagnosis(input, currentState))

          case REDUCE(lhsSymbol, production, size) =>
            var reduced: List[Any] = Nil
            var right = location
            var left = location
            // pop the top "frame"
            for {i <- 1 to size} {
              states.pop()
              symbols.pop()
              reduced = values.pop() :: reduced
              left = locations.pop()
            }
            // calculate the reduced "frame"
            val result = reduction(left, right, production)(reduced)
            // push its reduction and symbol type
            currentState = states.top
            symbols.push(lhsSymbol)
            values.push(result)
            states.push(goto(currentState)(lhsSymbol))
            locations.push(left)
        }
      }
      parseState
    }
  }

  /**
   *
   * Step-by-step parse, driven externally. The protocol is exemplified here:
   * {{{
   *     val pushParser = new LRParser.Push[Token](ACTIONTABLE, GOTOTABLE, reduction(_), ...)
   *     val pushscanner = Scanner(SourceTextCursor(source))
   *     var state = pushParser.start()
   *     while (state==LRParser.NEXTSTEP) {
   *       val input = if (pushscanner.hasNext) pushscanner.next() else SmallparseScanner.$end
   *       state = pushParser.step(input)
   *       println(state)
   *     }
   * }}}
   *
   * Each parse `step` is invoked with the next input symbol of the material to be parsed. If
   * it terminates with `NEXTSTEP` then the parse can be continued (by invoking `step` again).
   * If `step` terminates with ACCEPTED(value)` then the parse has been successful, and
   * the  `value` is the parse result as specified by the action expressions.
   *
   */
  class Push[Lex <: Lexeme](action:     State=>Terminal=>Action,
                            goto:       State => NonTerminal => State,
                            reduction:  (SourceLocation, SourceLocation, State) => PartialFunction[List[Any], Any],
                            symbolName: Map[Symbol,String],
                            sourceLocation: ()=> SourceLocation)
    extends LRParser[Lex](action, goto, reduction, symbolName, sourceLocation)  {

    import collection.mutable.Stack

    /**
     * Initial automaton state
     */
    def start(): ParseState = {
      symbols.push(0)
      values.push(())
      states.push(0)
      locations.push(SourceLocation(0,0))
      NEXTSTEP
    }

    /**
     * Move automaton to the next state by inputting `input`
     *
     *
     * @param input
     */
    def step(input: Lex, location: SourceLocation=sourceLocation()): ParseState = {
      var stepUnfinished = true
      while (stepUnfinished) {
        var currentState = states.top
        var act = action(currentState)(input.symbol)
        stepUnfinished = false
        if (logState) Console.println(s"$currentState: «$input» $act")
        act match {
          case _: GOTO => //TODO: Not really an action
          case SHIFT(newState) =>
            states.push(newState)
            symbols.push(input.symbol)
            values.push(input.value)
            locations.push(location)
            parseState = NEXTSTEP
          case ACCEPT =>
            parseState = ACCEPTED(values(1))
          case ERROR =>
            if (findRecoveryState()) {
              val SHIFT(newState) = action(states.top)(errorSymbol)
              states.push(newState)
              symbols.push(errorSymbol)
              values.push(None)
              println(s"error SHIFT($newState)")
              parseState = NEXTSTEP
            } else  {
              parseState = ERRONEOUS(diagnosis(input, currentState))
            }

          case REDUCE(lhsSymbol, production, size) =>
            var reduced: List[Any] = Nil
            var left, right: SourceLocation = location
            // pop the top "frame"
            for {i <- 1 to size} {
              states.pop()
              symbols.pop()
              reduced = values.pop() :: reduced
              left = locations.pop()
            }
            // calculate the reduced "frame"
            val result = reduction(left, right, production)(reduced)
            // push its reduction and symbol type
            currentState = states.top
            symbols.push(lhsSymbol)
            values.push(result)
            locations.push(left)
            val newState = goto(currentState)(lhsSymbol)
            states.push(newState)
            stepUnfinished = true
        }
      }
      parseState
    }
  }
}
