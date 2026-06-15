
package lists.Lists
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 13 => 1;  }
  case 1 => { case 14 => 4;  case 15 => 5;  case 16 => 6;  case 17 => 7;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case _ => REDUCE(13,1,0);  }
  case 1 => { case 0 => SHIFT(2);  case 5 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case _ => REDUCE(16,6,1);  }
  case 4 => { case 10 => SHIFT(8);  case _ => ERROR;  }
  case 5 => { case _ => REDUCE(14,3,1);  }
  case 6 => { case 11 => SHIFT(9);  case _ => REDUCE(17,8,1);  }
  case 7 => { case _ => REDUCE(15,4,1);  }
  case 8 => { case _ => REDUCE(13,2,3);  }
  case 9 => { case 5 => SHIFT(10);  case _ => REDUCE(16,7,2);  }
  case 10 => { case _ => REDUCE(16,5,3);  }
  case _ => { case _ => ERROR }
  }
}
