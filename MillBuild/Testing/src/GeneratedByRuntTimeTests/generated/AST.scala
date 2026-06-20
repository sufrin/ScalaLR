  package expr.AST
       trait Expr
       case class Bin(op: String, l: Expr, r: Expr) extends Expr
       case class Bra(e: Expr)                      extends Expr
       case class Quoted(string: String)            extends Expr { override val toString = s"\"$string\"" }
       case class Id(string: String)                extends Expr
       case class Num(double: Double)               extends Expr
