
package tinyfun
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 19 => 1;  }
  case 1 => { case 20 => 7;  case 21 => 8;  }
  case 5 => { case 21 => 11;  }
  case 9 => { case 21 => 17;  case 22 => 18;  }
  case 10 => { case 21 => 19;  }
  case 13 => { case 21 => 21;  }
  case 14 => { case 21 => 22;  }
  case 15 => { case 21 => 23;  }
  case 16 => { case 21 => 24;  }
  case 26 => { case 21 => 27;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(19,1,0);  }
  case 1 => { case 0 => SHIFT(2);  case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 12 => SHIFT(6);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case _ => REDUCE(21,6,1);  }
  case 4 => { case 5 => SHIFT(9);  case 13 => SHIFT(10);  case _ => REDUCE(21,5,1);  }
  case 5 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 6 => { case _ => REDUCE(20,4,1);  }
  case 7 => { case 11 => SHIFT(12);  case _ => ERROR;  }
  case 8 => { case 14 => SHIFT(13);  case 15 => SHIFT(14);  case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => REDUCE(20,3,1);  }
  case 9 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 10 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 11 => { case 6 => SHIFT(20);  case 14 => SHIFT(13);  case 15 => SHIFT(14);  case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => ERROR;  }
  case 12 => { case _ => REDUCE(19,2,3);  }
  case 13 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 17 => { case 14 => SHIFT(13);  case 15 => SHIFT(14);  case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => REDUCE(22,14,1);  }
  case 18 => { case 6 => SHIFT(25);  case 9 => SHIFT(26);  case _ => ERROR;  }
  case 19 => { case 14 => SHIFT(13);  case 15 => SHIFT(14);  case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => REDUCE(21,7,3);  }
  case 20 => { case _ => REDUCE(21,12,3);  }
  case 21 => { case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => REDUCE(21,9,3);  }
  case 22 => { case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => REDUCE(21,11,3);  }
  case 23 => { case _ => REDUCE(21,8,3);  }
  case 24 => { case _ => REDUCE(21,10,3);  }
  case 25 => { case _ => REDUCE(21,13,4);  }
  case 26 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case _ => ERROR;  }
  case 27 => { case 14 => SHIFT(13);  case 15 => SHIFT(14);  case 16 => SHIFT(15);  case 17 => SHIFT(16);  case _ => REDUCE(22,15,3);  }
  case _ => { case _ => ERROR }
  }
}
