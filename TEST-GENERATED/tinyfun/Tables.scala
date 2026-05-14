
package tinyfun
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 21 => 1;  }
  case 1 => { case 22 => 8;  case 23 => 9;  case 25 => 10;  }
  case 5 => { case 23 => 13;  case 25 => 10;  }
  case 11 => { case 23 => 20;  case 24 => 21;  case 25 => 10;  }
  case 12 => { case 23 => 22;  case 25 => 10;  }
  case 16 => { case 23 => 24;  case 25 => 10;  }
  case 17 => { case 23 => 25;  case 25 => 10;  }
  case 18 => { case 23 => 26;  case 25 => 10;  }
  case 19 => { case 23 => 27;  case 25 => 10;  }
  case 29 => { case 23 => 30;  case 25 => 10;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case _ => REDUCE(21,1,0);  }
  case 1 => { case 0 => SHIFT(2);  case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 12 => SHIFT(6);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case _ => REDUCE(25,16,1);  }
  case 4 => { case 5 => SHIFT(11);  case 15 => SHIFT(12);  case _ => REDUCE(23,5,1);  }
  case 5 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 6 => { case _ => REDUCE(22,4,1);  }
  case 7 => { case 3 => SHIFT(14);  case _ => ERROR;  }
  case 8 => { case 11 => SHIFT(15);  case _ => ERROR;  }
  case 9 => { case 16 => SHIFT(16);  case 17 => SHIFT(17);  case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => REDUCE(22,3,1);  }
  case 10 => { case _ => REDUCE(23,6,1);  }
  case 11 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 12 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 13 => { case 6 => SHIFT(23);  case 16 => SHIFT(16);  case 17 => SHIFT(17);  case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => ERROR;  }
  case 14 => { case _ => REDUCE(25,17,2);  }
  case 15 => { case _ => REDUCE(21,2,3);  }
  case 16 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 20 => { case 16 => SHIFT(16);  case 17 => SHIFT(17);  case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => REDUCE(24,14,1);  }
  case 21 => { case 6 => SHIFT(28);  case 9 => SHIFT(29);  case _ => ERROR;  }
  case 22 => { case 16 => SHIFT(16);  case 17 => SHIFT(17);  case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => REDUCE(23,7,3);  }
  case 23 => { case _ => REDUCE(23,12,3);  }
  case 24 => { case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => REDUCE(23,9,3);  }
  case 25 => { case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => REDUCE(23,11,3);  }
  case 26 => { case _ => REDUCE(23,8,3);  }
  case 27 => { case _ => REDUCE(23,10,3);  }
  case 28 => { case _ => REDUCE(23,13,4);  }
  case 29 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 13 => SHIFT(7);  case _ => ERROR;  }
  case 30 => { case 16 => SHIFT(16);  case 17 => SHIFT(17);  case 18 => SHIFT(18);  case 19 => SHIFT(19);  case _ => REDUCE(24,15,3);  }
  case _ => { case _ => ERROR }
  }
}
