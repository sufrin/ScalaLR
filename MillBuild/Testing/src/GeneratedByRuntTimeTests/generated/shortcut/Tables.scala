
package shortcut.Lists
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 8 => 2;  case 11 => 3;  }
  case 3 => { case 9 => 7;  case 10 => 8;  case 12 => 9;  case 13 => 10;  case 14 => 11;  case 15 => 12;  }
  case 13 => { case 9 => 15;  case 10 => 8;  case 14 => 11;  case 15 => 12;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 4 => SHIFT(1);  case _ => REDUCE(11,5,0);  }
  case 1 => { case _ => REDUCE(11,6,1);  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 5 => SHIFT(5);  case 3 => SHIFT(6);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case _ => REDUCE(9,3,1);  }
  case 6 => { case _ => REDUCE(14,11,1);  }
  case 7 => { case _ => REDUCE(12,8,1);  }
  case 8 => { case _ => REDUCE(9,2,1);  }
  case 9 => { case 4 => SHIFT(13);  case _ => REDUCE(13,10,1);  }
  case 10 => { case _ => REDUCE(8,1,2);  }
  case 11 => { case 6 => SHIFT(14);  case _ => REDUCE(15,13,1);  }
  case 12 => { case _ => REDUCE(10,4,1);  }
  case 13 => { case 5 => SHIFT(5);  case 3 => SHIFT(6);  case _ => REDUCE(12,9,2);  }
  case 14 => { case 3 => SHIFT(16);  case _ => ERROR;  }
  case 15 => { case _ => REDUCE(12,7,3);  }
  case 16 => { case _ => REDUCE(14,12,3);  }
  case _ => { case _ => ERROR }
  }
}
