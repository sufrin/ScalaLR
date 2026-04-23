
package tinyfun
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 20 => 1;  }
  case 1 => { case 21 => 8;  case 22 => 9;  case 23 => 10;  }
  case 6 => { case 22 => 14;  }
  case 12 => { case 22 => 9;  case 23 => 22;  }
  case 13 => { case 22 => 23;  }
  case 16 => { case 22 => 25;  }
  case 17 => { case 22 => 26;  }
  case 18 => { case 22 => 27;  }
  case 19 => { case 22 => 28;  }
  case 20 => { case 22 => 29;  }
  case 21 => { case 22 => 30;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(20,1,0);  }
  case 1 => { case 0 => SHIFT(2);  case 1 => SHIFT(3);  case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case 12 => SHIFT(7);  case 11 => REDUCE(21,6,0);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case 11 => SHIFT(11);  case _ => ERROR;  }
  case 4 => { case _ => REDUCE(22,8,1);  }
  case 5 => { case 5 => SHIFT(12);  case 13 => SHIFT(13);  case _ => REDUCE(22,7,1);  }
  case 6 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 7 => { case _ => REDUCE(21,5,1);  }
  case 8 => { case 11 => SHIFT(15);  case _ => ERROR;  }
  case 9 => { case 14 => SHIFT(16);  case 15 => SHIFT(17);  case 16 => SHIFT(18);  case 17 => SHIFT(19);  case 18 => SHIFT(20);  case _ => REDUCE(23,17,1);  }
  case 10 => { case 9 => SHIFT(21);  case _ => REDUCE(21,4,1);  }
  case 11 => { case _ => REDUCE(20,3,3);  }
  case 12 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 14 => { case 6 => SHIFT(24);  case 14 => SHIFT(16);  case 15 => SHIFT(17);  case 16 => SHIFT(18);  case 17 => SHIFT(19);  case 18 => SHIFT(20);  case _ => ERROR;  }
  case 15 => { case _ => REDUCE(20,2,3);  }
  case 16 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 20 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 21 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 22 => { case 6 => SHIFT(31);  case 9 => SHIFT(21);  case _ => ERROR;  }
  case 23 => { case 14 => SHIFT(16);  case 15 => SHIFT(17);  case 16 => SHIFT(18);  case 17 => SHIFT(19);  case 18 => SHIFT(20);  case _ => REDUCE(22,9,3);  }
  case 24 => { case _ => REDUCE(22,15,3);  }
  case 25 => { case 16 => SHIFT(18);  case 17 => SHIFT(19);  case 18 => SHIFT(20);  case _ => REDUCE(22,12,3);  }
  case 26 => { case 16 => SHIFT(18);  case 17 => SHIFT(19);  case 18 => SHIFT(20);  case _ => REDUCE(22,14,3);  }
  case 27 => { case 18 => SHIFT(20);  case _ => REDUCE(22,11,3);  }
  case 28 => { case 18 => SHIFT(20);  case _ => REDUCE(22,13,3);  }
  case 29 => { case 18 => SHIFT(20);  case _ => REDUCE(22,10,3);  }
  case 30 => { case 14 => SHIFT(16);  case 15 => SHIFT(17);  case 16 => SHIFT(18);  case 17 => SHIFT(19);  case 18 => SHIFT(20);  case _ => REDUCE(23,18,3);  }
  case 31 => { case _ => REDUCE(22,16,4);  }
  case _ => { case _ => ERROR }
  }
}
