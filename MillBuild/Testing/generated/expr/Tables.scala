
package expr.Expr
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 13 => 4;  case 14 => 5;  }
  case 2 => { case 14 => 6;  }
  case 3 => { case 14 => 7;  }
  case 9 => { case 14 => 14;  }
  case 10 => { case 14 => 15;  }
  case 11 => { case 14 => 16;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 6 => SHIFT(3);  case _ => ERROR;  }
  case 1 => { case _ => REDUCE(14,3,1);  }
  case 2 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 6 => SHIFT(3);  case _ => ERROR;  }
  case 3 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 6 => SHIFT(3);  case _ => ERROR;  }
  case 4 => { case 0 => SHIFT(8);  case 8 => SHIFT(9);  case _ => ERROR;  }
  case 5 => { case 10 => SHIFT(10);  case 11 => SHIFT(11);  case _ => REDUCE(13,1,1);  }
  case 6 => { case 5 => SHIFT(12);  case 10 => SHIFT(10);  case 11 => SHIFT(11);  case _ => ERROR;  }
  case 7 => { case 7 => SHIFT(13);  case 10 => SHIFT(10);  case 11 => SHIFT(11);  case _ => ERROR;  }
  case 8 => { case _ => ACCEPT;  }
  case 9 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 6 => SHIFT(3);  case _ => ERROR;  }
  case 10 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 6 => SHIFT(3);  case _ => ERROR;  }
  case 11 => { case 3 => SHIFT(1);  case 4 => SHIFT(2);  case 6 => SHIFT(3);  case _ => ERROR;  }
  case 12 => { case _ => REDUCE(14,6,3);  }
  case 13 => { case _ => REDUCE(14,7,3);  }
  case 14 => { case 10 => SHIFT(10);  case 11 => SHIFT(11);  case _ => REDUCE(13,2,3);  }
  case 15 => { case 11 => SHIFT(11);  case _ => REDUCE(14,5,3);  }
  case 16 => { case _ => REDUCE(14,4,3);  }
  case _ => { case _ => ERROR }
  }
}
