
package expr.Expression
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 17 => 2;  case 22 => 3;  }
  case 3 => { case 18 => 11;  case 19 => 12;  case 20 => 13;  case 21 => 14;  case 23 => 15;  case 24 => 16;  }
  case 5 => { case 19 => 17;  case 20 => 13;  case 21 => 14;  }
  case 18 => { case 19 => 24;  case 20 => 13;  case 21 => 14;  }
  case 19 => { case 19 => 25;  case 20 => 13;  case 21 => 14;  }
  case 20 => { case 19 => 26;  case 20 => 13;  case 21 => 14;  }
  case 21 => { case 19 => 27;  case 20 => 13;  case 21 => 14;  }
  case 22 => { case 18 => 28;  case 19 => 12;  case 20 => 13;  case 21 => 14;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 9 => SHIFT(1);  case _ => REDUCE(22,16,0);  }
  case 1 => { case _ => REDUCE(22,17,1);  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => REDUCE(24,20,0);  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => ERROR;  }
  case 6 => { case _ => REDUCE(21,11,1);  }
  case 7 => { case _ => REDUCE(21,12,1);  }
  case 8 => { case _ => REDUCE(21,14,1);  }
  case 9 => { case 0 => REDUCE(21,13,1);  case 15 => REDUCE(21,13,1);  case 9 => REDUCE(21,13,1);  case 10 => REDUCE(21,13,1);  case 11 => REDUCE(21,13,1);  case 12 => REDUCE(21,13,1);  case 13 => REDUCE(21,13,1);  case _ => REDUCE(21,13,1);  }
  case 10 => { case _ => REDUCE(20,10,1);  }
  case 11 => { case _ => REDUCE(23,18,1);  }
  case 12 => { case 10 => SHIFT(18);  case 11 => SHIFT(19);  case 12 => SHIFT(20);  case 13 => SHIFT(21);  case _ => REDUCE(18,2,1);  }
  case 13 => { case _ => REDUCE(19,3,1);  }
  case 14 => { case _ => REDUCE(20,9,1);  }
  case 15 => { case 9 => SHIFT(22);  case _ => REDUCE(24,21,1);  }
  case 16 => { case _ => REDUCE(17,1,2);  }
  case 17 => { case 15 => SHIFT(23);  case 10 => SHIFT(18);  case 11 => SHIFT(19);  case 12 => SHIFT(20);  case 13 => SHIFT(21);  case _ => ERROR;  }
  case 18 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => ERROR;  }
  case 19 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => ERROR;  }
  case 20 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => ERROR;  }
  case 21 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => ERROR;  }
  case 22 => { case 14 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case 7 => SHIFT(10);  case _ => ERROR;  }
  case 23 => { case _ => REDUCE(19,8,3);  }
  case 24 => { case 12 => SHIFT(20);  case 13 => SHIFT(21);  case _ => REDUCE(19,4,3);  }
  case 25 => { case 12 => SHIFT(20);  case 13 => SHIFT(21);  case _ => REDUCE(19,6,3);  }
  case 26 => { case _ => REDUCE(19,5,3);  }
  case 27 => { case _ => REDUCE(19,7,3);  }
  case 28 => { case _ => REDUCE(23,19,3);  }
  case _ => { case _ => ERROR }
  }
}
