package tinyfun

import scala.collection.mutable

object TinyFun {

  var prompt: String = ""

  import org.sufrin.scalalr.SourceLocation

  val store = new mutable.LinkedHashMap[String, Double]

  /**
   *   TinyFun is a little calculator
   */

  trait Expr {
    def value: Double
  }

  case class Num(value: Double, loc: SourceLocation) extends Expr

  case class Id(id: String, loc: SourceLocation) extends Expr {
    val value = store.getOrElse(id, 0.0)
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

  case class Neg(expr: Expr, loc: SourceLocation) extends Expr {
    def value: Double = -expr.value
  }


  case class Apply(op: String, exprs: Seq[Expr], loc: SourceLocation) extends Expr {
    def value: Double = {
      val values = exprs.map(_.value)
      op match {
        case "sum" => values.sum
        case "prod" => values.product
        case "min" => values.min
        case "max" => values.max
        case "sin" => (values map Math.sin).head
        case "cos" => (values map Math.cos).head
        case "tan" => (values map Math.tan).head
        case "*" => values.foldLeft(1.0)((l, r) => l * r)
        case "+" => values.foldLeft(0.0)((l, r) => l + r)
        case "/" => values.foldLeft(1.0)((l, r) => l / r)
        case "-" => values.foldLeft(0.0)((l, r) => l - r)
        case _ => 1.0 / 0.0
      }
    }
  }

  def run(exprs: Seq[Expr]): Unit = {
    println(exprs.map(_.value).mkString(" "))
    print(prompt)
    System.out.flush()
  }
}

