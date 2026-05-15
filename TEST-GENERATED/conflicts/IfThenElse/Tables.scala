
package IfThenElse
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 9 => 3;  }
  case 1 => { case 9 => 4;  }
  case 7 => { case 9 => 9;  }
  case 10 => { case 9 => 11;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 2 => { case _ => REDUCE(9,1,1);  }
  case 3 => { case 0 => SHIFT(5);  case 7 => SHIFT(6);  case _ => ERROR;  }
  case 4 => { case 4 => SHIFT(7);  case 7 => SHIFT(6);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case 6 => SHIFT(8);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 8 => { case _ => REDUCE(9,2,3);  }
  case 9 => { case 5 => SHIFT(10);  case 7 => SHIFT(6);  case _ => REDUCE(9,3,4);  }
  case 10 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 11 => { case 7 => SHIFT(6);  case _ => REDUCE(9,4,6);  }
  case _ => { case _ => ERROR }
  }
}
