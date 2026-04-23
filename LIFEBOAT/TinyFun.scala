
//> using scala "2.13"
//> using jar "scalalr.jar"

package tinyfun

import scala.collection.mutable

object TinyFun {

  import org.sufrin.scalalr.SourceLocation

  val store = new mutable.LinkedHashMap[String, Double]

  trait Expr { def value: Double }

  case class Num(value: Double, loc: SourceLocation) extends Expr

  case class Id(id: String, loc: SourceLocation) extends Expr {
    val value=store.getOrElse(id, 0.0)
  }

  case class Assign(id: String, expr: Expr, loc: SourceLocation) extends Expr {
    def value: Double = {
      val v = expr.value
      store(id) = v
      v
    }
  }

  case class Binop(op: String, l: Expr, r: Expr, loc: SourceLocation) extends Expr {
    def value: Double = {
      val lv = l.value
      val rv = r.value
      op match {
        case "+" => lv + rv
        case "-" => lv - rv
        case "*" => lv * rv
        case "/" => lv / rv
        case "^" => Math.pow(lv, rv)
        case _ => 0.0
      }
    }
  }

  case class Apply(op: String, exprs: Seq[Expr], loc: SourceLocation) extends Expr {
    def value: Double = {
      val values = exprs.map(_.value)
      op match {
        case "sum"  => values.sum
        case "prod" => values.product
        case "min"  => values.min
        case "max"  => values.max
        case "sin"  => (values map Math.sin).head
        case "cos"  => (values map Math.cos).head
        case "tan"  => (values map Math.tan).head
        case _      => values.sum
      }
    }
  }



  def run(exprs: Seq[Expr]): Unit = {
    println(exprs.map(_.value).mkString(" "));
    print("> ")
    System.console.flush
  }
}

