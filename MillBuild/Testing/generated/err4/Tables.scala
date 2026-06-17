
package scalalr.err4
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 5 => 2;  case 6 => 3;  case 7 => 4;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case _ => REDUCE(6,3,1);  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case 3 => SHIFT(6);  case _ => REDUCE(7,4,1);  }
  case 4 => { case _ => REDUCE(5,1,1);  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case _ => REDUCE(6,2,2);  }
  case _ => { case _ => ERROR }
  }
}
