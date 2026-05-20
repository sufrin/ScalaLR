
package infer.Infer
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 9 => 3;  }
  case 4 => { case 10 => 8;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 1 => { case 0 => REDUCE(9,1,1);  case _ => REDUCE(9,1,1);  }
  case 2 => { case 3 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 4 => { case 3 => SHIFT(6);  case 7 => SHIFT(7);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case _ => REDUCE(10,5,1);  }
  case 7 => { case _ => REDUCE(9,3,3);  }
  case 8 => { case 7 => SHIFT(9);  case _ => ERROR;  }
  case 9 => { case _ => REDUCE(9,4,4);  }
  case _ => { case _ => ERROR }
  }
}
