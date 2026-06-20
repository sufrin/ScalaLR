object ExpressionEvaluator {


    import expr.Expression.Components
    import expr.Expression.Scanner._
    import org.sufrin.utility.PrettyPrint._
    import org.sufrin.utility._
    import org.sufrin.scalalr._

    class ExpressionScanner(chars: SourceTextCursor) extends SimpleScannerCore[Token](chars) {
      val symbols = expr.Expression.Scanner
      override val LONG       = symbols.LONG
      override val DOUBLE     = symbols.DOUBLE
      override val NEWLINE    = Some(symbols.NL)
      override val IDENTIFIER = symbols.ID
      override val STRING     = symbols.QUOTE
      override def TOKENMAP   = TokenMap
  }

    def main(args: Array[String]): Unit = {
      val scanner = new ExpressionScanner(SourceTextCursor.console.withPrompt("> "))
      print("Welcome\n> ")
      val parser  = LRParser.Pull[Components.Token](Components)(scanner.sourceLocation)
      parser.run(scanner.next).prettyPrint()
  }



}
