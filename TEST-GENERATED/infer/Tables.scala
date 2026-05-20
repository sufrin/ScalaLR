
package infer.Infer
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 7 => 2;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(3);  case _ => REDUCE(7,1,1);  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(7,2,2);  }
  case 4 => { case _ => ACCEPT;  }
  case _ => { case _ => ERROR }
  }
}
