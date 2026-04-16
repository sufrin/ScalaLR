
package scalalr.parser.ScalaLR
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 33 => 2;  case 34 => 3;  }
  case 9 => { case 35 => 12;  }
  case 12 => { case 36 => 18;  case 37 => 19;  }
  case 14 => { case 38 => 21;  case 39 => 22;  }
  case 15 => { case 38 => 23;  case 39 => 22;  }
  case 16 => { case 38 => 24;  case 39 => 22;  }
  case 17 => { case 38 => 25;  case 39 => 22;  }
  case 19 => { case 36 => 27;  case 37 => 19;  }
  case 22 => { case 38 => 29;  case 39 => 22;  }
  case 26 => { case 35 => 30;  }
  case 28 => { case 50 => 33;  }
  case 30 => { case 40 => 35;  case 41 => 36;  case 43 => 37;  }
  case 32 => { case 50 => 40;  case 51 => 41;  }
  case 35 => { case 42 => 44;  }
  case 38 => { case 50 => 40;  case 51 => 46;  }
  case 42 => { case 50 => 49;  }
  case 43 => { case 41 => 50;  case 43 => 37;  }
  case 45 => { case 44 => 53;  case 45 => 54;  case 46 => 55;  case 47 => 56;  }
  case 47 => { case 50 => 40;  case 51 => 58;  }
  case 55 => { case 48 => 62;  }
  case 56 => { case 46 => 63;  case 47 => 56;  }
  case 60 => { case 44 => 65;  case 45 => 54;  case 46 => 55;  case 47 => 56;  }
  case 62 => { case 49 => 67;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 23 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(4);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(33,1,1);  }
  case 4 => { case 24 => SHIFT(6);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case 3 => SHIFT(7);  case _ => ERROR;  }
  case 7 => { case 20 => SHIFT(8);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(9);  case _ => ERROR;  }
  case 9 => { case 22 => SHIFT(10);  case 30 => SHIFT(11);  case _ => ERROR;  }
  case 10 => { case _ => REDUCE(35,3,1);  }
  case 11 => { case 5 => SHIFT(13);  case _ => ERROR;  }
  case 12 => { case 25 => SHIFT(14);  case 26 => SHIFT(15);  case 27 => SHIFT(16);  case 28 => SHIFT(17);  case _ => REDUCE(36,5,0);  }
  case 13 => { case _ => REDUCE(35,4,2);  }
  case 14 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 18 => { case 29 => SHIFT(26);  case _ => ERROR;  }
  case 19 => { case 25 => SHIFT(14);  case 26 => SHIFT(15);  case 27 => SHIFT(16);  case 28 => SHIFT(17);  case _ => REDUCE(36,5,0);  }
  case 20 => { case 13 => SHIFT(28);  case _ => REDUCE(39,14,1);  }
  case 21 => { case _ => REDUCE(37,10,2);  }
  case 22 => { case 3 => SHIFT(20);  case _ => REDUCE(38,11,1);  }
  case 23 => { case _ => REDUCE(37,7,2);  }
  case 24 => { case _ => REDUCE(37,8,2);  }
  case 25 => { case _ => REDUCE(37,9,2);  }
  case 26 => { case 22 => SHIFT(10);  case 30 => SHIFT(11);  case _ => ERROR;  }
  case 27 => { case _ => REDUCE(36,6,2);  }
  case 28 => { case 3 => SHIFT(31);  case 16 => SHIFT(32);  case _ => ERROR;  }
  case 29 => { case _ => REDUCE(38,12,2);  }
  case 30 => { case 3 => SHIFT(34);  case _ => ERROR;  }
  case 31 => { case 8 => SHIFT(38);  case _ => REDUCE(50,34,1);  }
  case 32 => { case 3 => SHIFT(31);  case 16 => SHIFT(32);  case 17 => SHIFT(39);  case _ => ERROR;  }
  case 33 => { case _ => REDUCE(39,13,3);  }
  case 34 => { case 13 => SHIFT(42);  case _ => REDUCE(43,21,1);  }
  case 35 => { case 10 => SHIFT(43);  case _ => REDUCE(42,18,0);  }
  case 36 => { case _ => REDUCE(40,15,1);  }
  case 37 => { case 11 => SHIFT(45);  case _ => ERROR;  }
  case 38 => { case 3 => SHIFT(31);  case 16 => SHIFT(32);  case _ => ERROR;  }
  case 39 => { case _ => REDUCE(50,37,2);  }
  case 40 => { case 18 => SHIFT(47);  case _ => REDUCE(51,38,1);  }
  case 41 => { case 17 => SHIFT(48);  case _ => ERROR;  }
  case 42 => { case 3 => SHIFT(31);  case 16 => SHIFT(32);  case _ => ERROR;  }
  case 43 => { case 3 => SHIFT(34);  case _ => REDUCE(42,19,1);  }
  case 44 => { case _ => REDUCE(34,2,12);  }
  case 45 => { case 3 => SHIFT(51);  case 22 => SHIFT(52);  case _ => ERROR;  }
  case 46 => { case 9 => SHIFT(57);  case _ => ERROR;  }
  case 47 => { case 3 => SHIFT(31);  case 16 => SHIFT(32);  case _ => ERROR;  }
  case 48 => { case _ => REDUCE(50,36,3);  }
  case 49 => { case _ => REDUCE(43,20,3);  }
  case 50 => { case _ => REDUCE(40,16,3);  }
  case 51 => { case 13 => SHIFT(59);  case _ => REDUCE(47,28,1);  }
  case 52 => { case _ => REDUCE(46,25,1);  }
  case 53 => { case _ => REDUCE(41,17,3);  }
  case 54 => { case 12 => SHIFT(60);  case _ => REDUCE(44,22,1);  }
  case 55 => { case 5 => SHIFT(61);  case _ => REDUCE(48,30,0);  }
  case 56 => { case 3 => SHIFT(51);  case 22 => SHIFT(52);  case _ => REDUCE(46,26,1);  }
  case 57 => { case _ => REDUCE(50,35,4);  }
  case 58 => { case _ => REDUCE(51,39,3);  }
  case 59 => { case 3 => SHIFT(64);  case _ => ERROR;  }
  case 60 => { case 3 => SHIFT(51);  case 22 => SHIFT(52);  case _ => ERROR;  }
  case 61 => { case _ => REDUCE(48,31,1);  }
  case 62 => { case 31 => SHIFT(66);  case _ => REDUCE(49,32,0);  }
  case 63 => { case _ => REDUCE(46,27,2);  }
  case 64 => { case _ => REDUCE(47,29,3);  }
  case 65 => { case _ => REDUCE(44,23,3);  }
  case 66 => { case 3 => SHIFT(68);  case _ => ERROR;  }
  case 67 => { case _ => REDUCE(45,24,3);  }
  case 68 => { case _ => REDUCE(49,33,2);  }
  case _ => { case _ => ERROR }
  }
}
