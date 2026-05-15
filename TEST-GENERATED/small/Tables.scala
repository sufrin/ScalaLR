
package small.Small
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 7 => 2;  case 8 => 3;  case 9 => 4;  case 10 => 5;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case _ => REDUCE(9,3,1);  }
  case 2 => { case 0 => SHIFT(6);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(7,1,1);  }
  case 4 => { case 4 => SHIFT(7);  case _ => REDUCE(10,5,1);  }
  case 5 => { case _ => REDUCE(8,2,1);  }
  case 6 => { case _ => ACCEPT;  }
  case 7 => { case 3 => SHIFT(8);  case _ => ERROR;  }
  case 8 => { case _ => REDUCE(9,4,3);  }
  case _ => { case _ => ERROR }
  }
}
