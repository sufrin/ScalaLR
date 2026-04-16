
package expr.Expr
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 13 => 5;  case 14 => 6;  }
  case 3 => { case 14 => 7;  }
  case 4 => { case 14 => 8;  }
  case 10 => { case 14 => 15;  }
  case 11 => { case 14 => 16;  }
  case 12 => { case 14 => 17;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 1 => SHIFT(1);  case 3 => SHIFT(2);  case 4 => SHIFT(3);  case 6 => SHIFT(4);  case _ => ERROR;  }
  case 1 => { case _ => REDUCE(13,3,1);  }
  case 2 => { case _ => REDUCE(14,4,1);  }
  case 3 => { case 3 => SHIFT(2);  case 4 => SHIFT(3);  case 6 => SHIFT(4);  case _ => ERROR;  }
  case 4 => { case 3 => SHIFT(2);  case 4 => SHIFT(3);  case 6 => SHIFT(4);  case _ => ERROR;  }
  case 5 => { case 0 => SHIFT(9);  case 8 => SHIFT(10);  case _ => ERROR;  }
  case 6 => { case 10 => SHIFT(11);  case 11 => SHIFT(12);  case _ => REDUCE(13,1,1);  }
  case 7 => { case 5 => SHIFT(13);  case 10 => SHIFT(11);  case 11 => SHIFT(12);  case _ => ERROR;  }
  case 8 => { case 7 => SHIFT(14);  case 10 => SHIFT(11);  case 11 => SHIFT(12);  case _ => ERROR;  }
  case 9 => { case _ => ACCEPT;  }
  case 10 => { case 3 => SHIFT(2);  case 4 => SHIFT(3);  case 6 => SHIFT(4);  case _ => ERROR;  }
  case 11 => { case 3 => SHIFT(2);  case 4 => SHIFT(3);  case 6 => SHIFT(4);  case _ => ERROR;  }
  case 12 => { case 3 => SHIFT(2);  case 4 => SHIFT(3);  case 6 => SHIFT(4);  case _ => ERROR;  }
  case 13 => { case _ => REDUCE(14,7,3);  }
  case 14 => { case _ => REDUCE(14,8,3);  }
  case 15 => { case 10 => SHIFT(11);  case 11 => SHIFT(12);  case _ => REDUCE(13,2,3);  }
  case 16 => { case 11 => SHIFT(12);  case _ => REDUCE(14,6,3);  }
  case 17 => { case _ => REDUCE(14,5,3);  }
  case _ => { case _ => ERROR }
  }
}
