
package small.Small
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 7 => 2;  case 8 => 3;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case _ => REDUCE(8,2,1);  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 4 => SHIFT(5);  case _ => REDUCE(7,1,1);  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 3 => SHIFT(6);  case _ => ERROR;  }
  case 6 => { case _ => REDUCE(8,3,3);  }
  case _ => { case _ => ERROR }
  }
}
