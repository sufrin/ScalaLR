
package lists.Lists
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 7 => 2;  case 9 => 3;  }
  case 3 => { case 8 => 6;  case 10 => 7;  case 11 => 8;  case 12 => 9;  case 13 => 10;  }
  case 11 => { case 8 => 13;  case 12 => 9;  case 13 => 10;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 4 => SHIFT(1);  case _ => REDUCE(9,3,0);  }
  case 1 => { case _ => REDUCE(9,4,1);  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 3 => SHIFT(5);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case _ => REDUCE(12,10,1);  }
  case 6 => { case _ => REDUCE(10,6,1);  }
  case 7 => { case 4 => SHIFT(11);  case _ => REDUCE(11,8,1);  }
  case 8 => { case _ => REDUCE(7,1,2);  }
  case 9 => { case 5 => SHIFT(12);  case _ => REDUCE(13,12,1);  }
  case 10 => { case _ => REDUCE(8,2,1);  }
  case 11 => { case 3 => SHIFT(5);  case _ => REDUCE(10,7,2);  }
  case 12 => { case 3 => SHIFT(14);  case _ => REDUCE(12,11,2);  }
  case 13 => { case _ => REDUCE(10,5,3);  }
  case 14 => { case _ => REDUCE(12,9,3);  }
  case _ => { case _ => ERROR }
  }
}
