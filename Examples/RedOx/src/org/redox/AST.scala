package org.redox
object AST

import org.sufrin.scalalr.SourceLocation
import org.sufrin.utility.PrettyPrint.PrettyPrintable

/**
 * An `AST` extension is an Abstract Syntax Type.
 *
 * Objects of such types are constructed as usual.
 * But if (for some reason) it's necessary for them to
 * be associated with a textual span, they can be applied,
 * after construction, to a `(START, END)` pair.
 *
 * Here is an extract from `redox.scalalr` demonstrating how
 * ASTypes can be associated with textual spans.
 *
 * {{{
 * prim: Proc =
 *   lexprs: (',' expr)+  ':=' exprs   { Assign($lexprs, $exprs)($START, $END) }
 * | chan: expr '?' lexpr: lexpr       { Read($chan, $lexpr) }
 * | chan: expr '!' value: expr        { Write($chan, $value) }
 * | `skip`                            { SKIP}
 * | `delay`                           { DELAY}
 * | `stop`                            { STOP }
 * | "alt"         body: alts          { PriAlt($body)($START, $END) }
 * | "if"          body: ifs           { If($body)($START, $END) }
 * | "case" expr   body: cases         { Case($expr, $body)($START, $END) }
 * | "while" expr  body: procs         { mkWhile($expr, $body) }
 * | "until" expr  body: procs         { mkUntil($expr, $body) }
 * | "seq"         body: procs         { Sequence($body.map(flatten))($START, $END) }
 * | "{"  body: (proc ';')... "}"      { Sequence($body.map(flatten))($START, $END) }
 * }}}
 */

trait AST {
  def start: SourceLocation = _start
  def end:   SourceLocation = _end
  private var _start: SourceLocation = null
  private var _end:   SourceLocation = null
  /** Associate this object with the text between START and END, then yield the object */
  def apply(START: SourceLocation, END: SourceLocation): this.type = {
    _start = START
    _end = END
    this
  }
}

case class CaseBranch(labels: Seq[Expr], body: Proc)
case class AltBranch(guard: Expr, channel: String, lvalue: LExpr, body: Proc)
case class IfBranch(guard: Expr, body: Proc)

trait Proc extends AST
case class Parallel(procs: Seq[Proc]) extends Proc
case class Sequence(procs: Seq[Proc])  extends Proc
case class PAR(l: Proc, r: Proc) extends Proc
case class SEQ(l: Proc, r: Proc) extends Proc
case class Write(chan: Expr, value: Expr)  extends Proc
case class Read(chan: Expr,  lvalue: Expr)  extends Proc
case class Block(decls: Seq[Decl], body: Seq[Proc])  extends Proc
case class While(guard: Expr, body: Proc)  extends Proc
case class Until(guard: Expr, body: Proc)  extends Proc
case class Repeat( body: Proc)  extends Proc
case class If(branches: Seq[IfBranch]) extends Proc
case class Do(branches: Seq[IfBranch]) extends Proc
case class Case(subject: Expr, branches: Seq[CaseBranch])  extends Proc
case class Assign(lhss: Seq[Expr], rhss: Seq[Expr]) extends Proc
case class Declare(declarations: Seq[Decl], body: Proc) extends Proc
case class Call(procName: String) extends Proc
case class PriAlt(branches: Seq[AltBranch]) extends Proc
case object SKIP extends Proc
case object DELAY extends Proc
case object STOP extends Proc

trait Decl extends AST
case class Integer(id: String, width: Expr,  init: Seq[Expr]) extends Decl
case class Unsigned(id: String, width: Expr, init: Seq[Expr]) extends Decl
case class Channel(id: String, width: Expr) extends Decl
case class Ram(id: String, dataWidth: Expr, addrWidth: Expr) extends Decl
case class Rom(id: String, dataWidth: Expr, data: Seq[Expr]) extends Decl
case class Macro(id: String, params: Seq[Id], body: Expr) extends Decl

trait Expr extends AST
trait LExpr extends Expr
case object Anything extends LExpr
case class Id(name: String) extends LExpr
case class Index(array: Expr, offset: Expr) extends LExpr
case class Num(value: Long, bits: Long) extends Expr {
  override val toString: String = if (bits==0) value.toHexString else s"${value.toHexString}:$bits"
}
case class Unop(op: String, expr: Expr) extends Expr
case class Binop(op: String, l: Expr, r: Expr) extends Expr
case class Width( value: Expr, width: Expr) extends Expr
case class Apply(fun: String, args: Seq[Expr]) extends Expr
case class Cond(guard: Expr, tt: Expr, ff: Expr) extends Expr

trait Type
case object Byte extends Type
case object Int  extends Type



