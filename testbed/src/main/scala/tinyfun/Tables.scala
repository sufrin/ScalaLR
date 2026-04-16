
package tinyfun
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 17 => 2;  }
  case 2 => { case 18 => 8;  case 19 => 9;  }
  case 7 => { case 19 => 11;  }
  case 10 => { case 19 => 17;  case 20 => 18;  }
  case 13 => { case 19 => 20;  }
  case 14 => { case 19 => 21;  }
  case 15 => { case 19 => 22;  }
  case 16 => { case 19 => 23;  }
  case 25 => { case 19 => 26;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 1 => SHIFT(1);  case 0 => REDUCE(17,1,0);  case 3 => REDUCE(17,1,0);  case 4 => REDUCE(17,1,0);  case 5 => REDUCE(17,1,0);  case _ => ERROR;  }
  case 1 => { case 11 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(4);  case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(17,3,2);  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case _ => REDUCE(19,6,1);  }
  case 6 => { case 5 => SHIFT(10);  case _ => REDUCE(19,5,1);  }
  case 7 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 8 => { case 11 => SHIFT(12);  case _ => ERROR;  }
  case 9 => { case 12 => SHIFT(13);  case 13 => SHIFT(14);  case 14 => SHIFT(15);  case 15 => SHIFT(16);  case _ => REDUCE(18,4,1);  }
  case 10 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 11 => { case 6 => SHIFT(19);  case 12 => SHIFT(13);  case 13 => SHIFT(14);  case 14 => SHIFT(15);  case 15 => SHIFT(16);  case _ => ERROR;  }
  case 12 => { case _ => REDUCE(17,2,3);  }
  case 13 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 17 => { case 12 => SHIFT(13);  case 13 => SHIFT(14);  case 14 => SHIFT(15);  case 15 => SHIFT(16);  case _ => REDUCE(20,13,1);  }
  case 18 => { case 6 => SHIFT(24);  case 9 => SHIFT(25);  case _ => ERROR;  }
  case 19 => { case _ => REDUCE(19,11,3);  }
  case 20 => { case 14 => SHIFT(15);  case 15 => SHIFT(16);  case _ => REDUCE(19,8,3);  }
  case 21 => { case 14 => SHIFT(15);  case 15 => SHIFT(16);  case _ => REDUCE(19,10,3);  }
  case 22 => { case _ => REDUCE(19,7,3);  }
  case 23 => { case _ => REDUCE(19,9,3);  }
  case 24 => { case _ => REDUCE(19,12,4);  }
  case 25 => { case 3 => SHIFT(5);  case 4 => SHIFT(6);  case 5 => SHIFT(7);  case _ => ERROR;  }
  case 26 => { case 12 => SHIFT(13);  case 13 => SHIFT(14);  case 14 => SHIFT(15);  case 15 => SHIFT(16);  case _ => REDUCE(20,14,3);  }
  case _ => { case _ => ERROR }
  }
}
