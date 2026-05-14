
package tinyfun
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 21 => 1;  }
  case 1 => { case 22 => 9;  case 23 => 10;  case 24 => 11;  }
  case 6 => { case 23 => 16;  }
  case 13 => { case 23 => 10;  case 24 => 24;  }
  case 14 => { case 23 => 25;  }
  case 18 => { case 23 => 28;  }
  case 19 => { case 23 => 29;  }
  case 20 => { case 23 => 30;  }
  case 21 => { case 23 => 31;  }
  case 22 => { case 23 => 32;  }
  case 23 => { case 23 => 33;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(21,1,0);  }
  case 1 => { case 0 => SHIFT(2);  case 1 => SHIFT(3);  case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case 12 => SHIFT(7);  case 13 => SHIFT(8);  case 11 => REDUCE(22,7,0);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case 11 => SHIFT(12);  case _ => ERROR;  }
  case 4 => { case _ => REDUCE(23,9,1);  }
  case 5 => { case 5 => SHIFT(13);  case 14 => SHIFT(14);  case _ => REDUCE(23,8,1);  }
  case 6 => { case 1 => SHIFT(15);  case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 7 => { case _ => REDUCE(22,5,1);  }
  case 8 => { case _ => REDUCE(22,6,1);  }
  case 9 => { case 11 => SHIFT(17);  case _ => ERROR;  }
  case 10 => { case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case _ => REDUCE(24,19,1);  }
  case 11 => { case 9 => SHIFT(23);  case _ => REDUCE(22,4,1);  }
  case 12 => { case _ => REDUCE(21,3,3);  }
  case 13 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 15 => { case 6 => SHIFT(26);  case _ => ERROR;  }
  case 16 => { case 6 => SHIFT(27);  case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case _ => ERROR;  }
  case 17 => { case _ => REDUCE(21,2,3);  }
  case 18 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 20 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 21 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 22 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 23 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 24 => { case 6 => SHIFT(34);  case 9 => SHIFT(23);  case _ => ERROR;  }
  case 25 => { case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case _ => REDUCE(23,10,3);  }
  case 26 => { case _ => REDUCE(23,17,3);  }
  case 27 => { case _ => REDUCE(23,16,3);  }
  case 28 => { case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case _ => REDUCE(23,13,3);  }
  case 29 => { case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case _ => REDUCE(23,15,3);  }
  case 30 => { case 19 => SHIFT(22);  case _ => REDUCE(23,12,3);  }
  case 31 => { case 19 => SHIFT(22);  case _ => REDUCE(23,14,3);  }
  case 32 => { case 19 => SHIFT(22);  case _ => REDUCE(23,11,3);  }
  case 33 => { case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case _ => REDUCE(24,20,3);  }
  case 34 => { case _ => REDUCE(23,18,4);  }
  case _ => { case _ => ERROR }
  }
}
