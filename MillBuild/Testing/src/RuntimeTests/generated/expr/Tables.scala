
package expr.Expression
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 15 => 2;  case 19 => 3;  }
  case 3 => { case 16 => 10;  case 17 => 11;  case 18 => 12;  case 20 => 13;  case 21 => 14;  }
  case 5 => { case 17 => 15;  case 18 => 12;  }
  case 16 => { case 17 => 22;  case 18 => 12;  }
  case 17 => { case 17 => 23;  case 18 => 12;  }
  case 18 => { case 17 => 24;  case 18 => 12;  }
  case 19 => { case 17 => 25;  case 18 => 12;  }
  case 20 => { case 16 => 26;  case 17 => 11;  case 18 => 12;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 7 => SHIFT(1);  case _ => REDUCE(19,13,0);  }
  case 1 => { case _ => REDUCE(19,14,1);  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => ERROR;  }
  case 6 => { case _ => REDUCE(18,10,1);  }
  case 7 => { case _ => REDUCE(18,11,1);  }
  case 8 => { case _ => REDUCE(18,9,1);  }
  case 9 => { case _ => REDUCE(18,12,1);  }
  case 10 => { case _ => REDUCE(20,16,1);  }
  case 11 => { case 8 => SHIFT(16);  case 9 => SHIFT(17);  case 10 => SHIFT(18);  case 11 => SHIFT(19);  case _ => REDUCE(16,2,1);  }
  case 12 => { case _ => REDUCE(17,3,1);  }
  case 13 => { case 7 => SHIFT(20);  case _ => REDUCE(21,18,1);  }
  case 14 => { case _ => REDUCE(15,1,2);  }
  case 15 => { case 13 => SHIFT(21);  case 8 => SHIFT(16);  case 9 => SHIFT(17);  case 10 => SHIFT(18);  case 11 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => ERROR;  }
  case 17 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => ERROR;  }
  case 18 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => ERROR;  }
  case 19 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => ERROR;  }
  case 20 => { case 12 => SHIFT(5);  case 3 => SHIFT(6);  case 4 => SHIFT(7);  case 5 => SHIFT(8);  case 6 => SHIFT(9);  case _ => REDUCE(20,17,2);  }
  case 21 => { case _ => REDUCE(17,8,3);  }
  case 22 => { case 10 => SHIFT(18);  case 11 => SHIFT(19);  case _ => REDUCE(17,4,3);  }
  case 23 => { case 10 => SHIFT(18);  case 11 => SHIFT(19);  case _ => REDUCE(17,6,3);  }
  case 24 => { case _ => REDUCE(17,5,3);  }
  case 25 => { case _ => REDUCE(17,7,3);  }
  case 26 => { case _ => REDUCE(20,15,3);  }
  case _ => { case _ => ERROR }
  }
}
