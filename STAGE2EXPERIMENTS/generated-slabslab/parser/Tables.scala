
package scalalr.stage2
object Tables {case class ErroneousGoto(state: Int, symbol: Int) extends Throwable
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 36 => 1;  case 37 => 2;  case 38 => 3;  }
  case 3 => { case 39 => 11;  }
  case 11 => { case 40 => 18;  }
  case 18 => { case 41 => 21;  case 42 => 22;  case 44 => 23;  }
  case 25 => { case 51 => 30;  }
  case 26 => { case 42 => 31;  case 44 => 23;  }
  case 27 => { case 43 => 33;  }
  case 29 => { case 51 => 36;  case 52 => 37;  }
  case 33 => { case 45 => 40;  case 46 => 41;  case 47 => 42;  case 48 => 43;  }
  case 34 => { case 51 => 36;  case 52 => 44;  }
  case 42 => { case 49 => 50;  }
  case 43 => { case 47 => 51;  case 48 => 43;  }
  case 45 => { case 51 => 36;  case 52 => 53;  }
  case 48 => { case 45 => 55;  case 46 => 41;  case 47 => 42;  case 48 => 43;  }
  case 50 => { case 50 => 57;  }
  case state => { case symbol => throw ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(38,3,0);  }
  case 1 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 2 => { case _ => REDUCE(36,1,1);  }
  case 3 => { case 20 => SHIFT(5);  case 23 => SHIFT(6);  case 24 => SHIFT(7);  case 29 => SHIFT(8);  case 30 => SHIFT(9);  case 32 => SHIFT(10);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 3 => SHIFT(12);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(13);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(14);  case _ => ERROR;  }
  case 8 => { case _ => REDUCE(39,9,1);  }
  case 9 => { case 5 => SHIFT(15);  case _ => ERROR;  }
  case 10 => { case 3 => SHIFT(16);  case _ => ERROR;  }
  case 11 => { case 30 => SHIFT(17);  case _ => REDUCE(40,11,0);  }
  case 12 => { case _ => REDUCE(38,6,3);  }
  case 13 => { case _ => REDUCE(38,4,3);  }
  case 14 => { case _ => REDUCE(38,5,3);  }
  case 15 => { case _ => REDUCE(38,8,3);  }
  case 16 => { case _ => REDUCE(38,7,3);  }
  case 17 => { case 5 => SHIFT(19);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 19 => { case 19 => SHIFT(24);  case _ => ERROR;  }
  case 20 => { case 13 => SHIFT(25);  case _ => REDUCE(44,18,1);  }
  case 21 => { case 19 => SHIFT(26);  case _ => REDUCE(37,2,4);  }
  case 22 => { case _ => REDUCE(41,12,1);  }
  case 23 => { case 11 => SHIFT(27);  case _ => ERROR;  }
  case 24 => { case _ => REDUCE(40,10,3);  }
  case 25 => { case 3 => SHIFT(28);  case 16 => SHIFT(29);  case _ => ERROR;  }
  case 26 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 27 => { case 12 => SHIFT(32);  case _ => REDUCE(43,16,0);  }
  case 28 => { case 8 => SHIFT(34);  case _ => REDUCE(51,31,1);  }
  case 29 => { case 3 => SHIFT(28);  case 16 => SHIFT(29);  case 17 => SHIFT(35);  case _ => ERROR;  }
  case 30 => { case _ => REDUCE(44,17,3);  }
  case 31 => { case _ => REDUCE(41,13,3);  }
  case 32 => { case _ => REDUCE(43,15,1);  }
  case 33 => { case 3 => SHIFT(38);  case 22 => SHIFT(39);  case _ => ERROR;  }
  case 34 => { case 3 => SHIFT(28);  case 16 => SHIFT(29);  case _ => ERROR;  }
  case 35 => { case _ => REDUCE(51,34,2);  }
  case 36 => { case 18 => SHIFT(45);  case _ => REDUCE(52,35,1);  }
  case 37 => { case 17 => SHIFT(46);  case _ => ERROR;  }
  case 38 => { case 13 => SHIFT(47);  case _ => REDUCE(48,25,1);  }
  case 39 => { case _ => REDUCE(47,22,1);  }
  case 40 => { case _ => REDUCE(42,14,4);  }
  case 41 => { case 12 => SHIFT(48);  case _ => REDUCE(45,19,1);  }
  case 42 => { case 5 => SHIFT(49);  case _ => REDUCE(49,27,0);  }
  case 43 => { case 3 => SHIFT(38);  case 22 => SHIFT(39);  case _ => REDUCE(47,23,1);  }
  case 44 => { case 9 => SHIFT(52);  case _ => ERROR;  }
  case 45 => { case 3 => SHIFT(28);  case 16 => SHIFT(29);  case _ => ERROR;  }
  case 46 => { case _ => REDUCE(51,33,3);  }
  case 47 => { case 3 => SHIFT(54);  case _ => ERROR;  }
  case 48 => { case 3 => SHIFT(38);  case 22 => SHIFT(39);  case _ => ERROR;  }
  case 49 => { case _ => REDUCE(49,28,1);  }
  case 50 => { case 31 => SHIFT(56);  case _ => REDUCE(50,29,0);  }
  case 51 => { case _ => REDUCE(47,24,2);  }
  case 52 => { case _ => REDUCE(51,32,4);  }
  case 53 => { case _ => REDUCE(52,36,3);  }
  case 54 => { case _ => REDUCE(48,26,3);  }
  case 55 => { case _ => REDUCE(45,20,3);  }
  case 56 => { case 3 => SHIFT(58);  case _ => ERROR;  }
  case 57 => { case _ => REDUCE(46,21,3);  }
  case 58 => { case _ => REDUCE(50,30,2);  }
  case _ => { case _ => ERROR }
  }
}
