package org.sufrin.scalalr

object Action {
  sealed trait Action
  case class  SHIFT(state: Int)       extends Action
  case class  REDUCE(symbol: Int, production: Int, size: Int) extends Action
  case object ACCEPT                  extends Action
  case object ERROR                   extends Action
  case class  GOTO(inState: Int, toState: Int) extends Action // not really an action
}
