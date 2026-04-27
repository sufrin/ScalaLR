
package IfThenElse
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 9 => 3;  }
  case 1 => { case 9 => 4;  }
  case 7 => { case 9 => 9;  }
  case 10 => { case 9 => 11;  }
  case 12 => { case 9 => 18;  }
  case 15 => { case 9 => 20;  }
  case 22 => { case 9 => 26;  }
  case 23 => { case 9 => 27;  }
  case 24 => { case 9 => 28;  }
  case 29 => { case 9 => 33;  }
  case 31 => { case 9 => 35;  }
  case 32 => { case 9 => 36;  }
  case 37 => { case 9 => 38;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(12);  case 6 => SHIFT(13);  case _ => ERROR;  }
  case 2 => { case 0 => REDUCE(9,1,1);  case 7 => REDUCE(9,1,1);  case _ => ERROR;  }
  case 3 => { case 0 => SHIFT(5);  case 7 => SHIFT(6);  case _ => ERROR;  }
  case 4 => { case 4 => SHIFT(7);  case 7 => SHIFT(14);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case 6 => SHIFT(8);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(15);  case 6 => SHIFT(16);  case _ => ERROR;  }
  case 8 => { case 0 => REDUCE(9,2,3);  case 7 => REDUCE(9,2,3);  case _ => ERROR;  }
  case 9 => { case 5 => SHIFT(10);  case 7 => SHIFT(17);  case 0 => REDUCE(9,3,4);  case _ => ERROR;  }
  case 10 => { case 3 => SHIFT(1);  case 6 => SHIFT(2);  case _ => ERROR;  }
  case 11 => { case 7 => SHIFT(6);  case 0 => REDUCE(9,4,6);  case _ => ERROR;  }
  case 12 => { case 3 => SHIFT(12);  case 6 => SHIFT(13);  case _ => ERROR;  }
  case 13 => { case 4 => REDUCE(9,1,1);  case 7 => REDUCE(9,1,1);  case _ => ERROR;  }
  case 14 => { case 6 => SHIFT(19);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(12);  case 6 => SHIFT(13);  case _ => ERROR;  }
  case 16 => { case 0 => REDUCE(9,1,1);  case 5 => REDUCE(9,1,1);  case 7 => REDUCE(9,1,1);  case _ => ERROR;  }
  case 17 => { case 6 => SHIFT(21);  case _ => ERROR;  }
  case 18 => { case 4 => SHIFT(22);  case 7 => SHIFT(14);  case _ => ERROR;  }
  case 19 => { case 4 => REDUCE(9,2,3);  case 7 => REDUCE(9,2,3);  case _ => ERROR;  }
  case 20 => { case 4 => SHIFT(23);  case 7 => SHIFT(14);  case _ => ERROR;  }
  case 21 => { case 0 => REDUCE(9,2,3);  case 5 => REDUCE(9,2,3);  case 7 => REDUCE(9,2,3);  case _ => ERROR;  }
  case 22 => { case 3 => SHIFT(24);  case 6 => SHIFT(25);  case _ => ERROR;  }
  case 23 => { case 3 => SHIFT(15);  case 6 => SHIFT(16);  case _ => ERROR;  }
  case 24 => { case 3 => SHIFT(12);  case 6 => SHIFT(13);  case _ => ERROR;  }
  case 25 => { case 4 => REDUCE(9,1,1);  case 5 => REDUCE(9,1,1);  case 7 => REDUCE(9,1,1);  case _ => ERROR;  }
  case 26 => { case 5 => SHIFT(29);  case 7 => SHIFT(30);  case 4 => REDUCE(9,3,4);  case _ => ERROR;  }
  case 27 => { case 5 => SHIFT(31);  case 7 => SHIFT(17);  case 0 => REDUCE(9,3,4);  case _ => ERROR;  }
  case 28 => { case 4 => SHIFT(32);  case 7 => SHIFT(14);  case _ => ERROR;  }
  case 29 => { case 3 => SHIFT(12);  case 6 => SHIFT(13);  case _ => ERROR;  }
  case 30 => { case 6 => SHIFT(34);  case _ => ERROR;  }
  case 31 => { case 3 => SHIFT(15);  case 6 => SHIFT(16);  case _ => ERROR;  }
  case 32 => { case 3 => SHIFT(24);  case 6 => SHIFT(25);  case _ => ERROR;  }
  case 33 => { case 7 => SHIFT(14);  case 4 => REDUCE(9,4,6);  case _ => ERROR;  }
  case 34 => { case 4 => REDUCE(9,2,3);  case 5 => REDUCE(9,2,3);  case 7 => REDUCE(9,2,3);  case _ => ERROR;  }
  case 35 => { case 7 => SHIFT(17);  case 0 => REDUCE(9,4,6);  case 5 => REDUCE(9,4,6);  case _ => ERROR;  }
  case 36 => { case 5 => SHIFT(37);  case 7 => SHIFT(30);  case 4 => REDUCE(9,3,4);  case _ => ERROR;  }
  case 37 => { case 3 => SHIFT(24);  case 6 => SHIFT(25);  case _ => ERROR;  }
  case 38 => { case 7 => SHIFT(30);  case 4 => REDUCE(9,4,6);  case 5 => REDUCE(9,4,6);  case _ => ERROR;  }
  case _ => { case _ => ERROR }
  }
}
