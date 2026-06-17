
package scalalr.err5
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 6 => 2;  case 8 => 3;  case 9 => 4;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case _ => REDUCE(8,3,2);  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case 3 => SHIFT(6);  case _ => REDUCE(8,4,1);  }
  case 4 => { case _ => REDUCE(6,1,1);  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case _ => REDUCE(7,2,1);  }
  case _ => { case _ => ERROR }
  }
}
