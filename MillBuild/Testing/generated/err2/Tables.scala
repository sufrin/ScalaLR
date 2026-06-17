
package scalalr.err2
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 5 => 2;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 0 => REDUCE(5,1,1);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case _ => { case _ => ERROR }
  }
}
