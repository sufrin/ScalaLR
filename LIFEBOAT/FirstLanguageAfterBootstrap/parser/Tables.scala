
package scalalr.parser.ScalaLR
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 34 => 2;  case 35 => 3;  }
  case 9 => { case 41 => 11;  }
  case 11 => { case 36 => 15;  }
  case 15 => { case 37 => 21;  case 38 => 22;  }
  case 17 => { case 39 => 24;  case 40 => 25;  }
  case 18 => { case 39 => 26;  case 40 => 25;  }
  case 19 => { case 39 => 27;  case 40 => 25;  }
  case 20 => { case 39 => 28;  case 40 => 25;  }
  case 22 => { case 37 => 30;  case 38 => 22;  }
  case 25 => { case 39 => 33;  case 40 => 25;  }
  case 29 => { case 36 => 34;  }
  case 31 => { case 52 => 37;  }
  case 32 => { case 52 => 38;  }
  case 34 => { case 42 => 40;  case 43 => 41;  case 45 => 42;  }
  case 36 => { case 52 => 45;  case 53 => 46;  }
  case 40 => { case 44 => 50;  }
  case 43 => { case 52 => 45;  case 53 => 52;  }
  case 48 => { case 52 => 55;  }
  case 49 => { case 43 => 56;  case 45 => 42;  }
  case 51 => { case 46 => 59;  case 47 => 60;  case 48 => 61;  case 49 => 62;  }
  case 53 => { case 52 => 45;  case 53 => 64;  }
  case 61 => { case 50 => 68;  }
  case 62 => { case 48 => 69;  case 49 => 62;  }
  case 66 => { case 46 => 71;  case 47 => 60;  case 48 => 61;  case 49 => 62;  }
  case 68 => { case 51 => 73;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 23 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(4);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(34,1,1);  }
  case 4 => { case 24 => SHIFT(6);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case 3 => SHIFT(7);  case _ => ERROR;  }
  case 7 => { case 20 => SHIFT(8);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(9);  case _ => ERROR;  }
  case 9 => { case 32 => SHIFT(10);  case _ => REDUCE(41,16,0);  }
  case 10 => { case 3 => SHIFT(12);  case _ => ERROR;  }
  case 11 => { case 22 => SHIFT(13);  case 30 => SHIFT(14);  case _ => ERROR;  }
  case 12 => { case _ => REDUCE(41,17,2);  }
  case 13 => { case _ => REDUCE(36,3,1);  }
  case 14 => { case 5 => SHIFT(16);  case _ => ERROR;  }
  case 15 => { case 25 => SHIFT(17);  case 26 => SHIFT(18);  case 27 => SHIFT(19);  case 28 => SHIFT(20);  case _ => REDUCE(37,5,0);  }
  case 16 => { case _ => REDUCE(36,4,2);  }
  case 17 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 20 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 21 => { case 29 => SHIFT(29);  case _ => ERROR;  }
  case 22 => { case 25 => SHIFT(17);  case 26 => SHIFT(18);  case 27 => SHIFT(19);  case 28 => SHIFT(20);  case _ => REDUCE(37,5,0);  }
  case 23 => { case 13 => SHIFT(31);  case 16 => SHIFT(32);  case _ => REDUCE(40,15,1);  }
  case 24 => { case _ => REDUCE(38,10,2);  }
  case 25 => { case 3 => SHIFT(23);  case _ => REDUCE(39,11,1);  }
  case 26 => { case _ => REDUCE(38,7,2);  }
  case 27 => { case _ => REDUCE(38,8,2);  }
  case 28 => { case _ => REDUCE(38,9,2);  }
  case 29 => { case 22 => SHIFT(13);  case 30 => SHIFT(14);  case _ => ERROR;  }
  case 30 => { case _ => REDUCE(37,6,2);  }
  case 31 => { case 3 => SHIFT(35);  case 16 => SHIFT(36);  case _ => ERROR;  }
  case 32 => { case 3 => SHIFT(35);  case 16 => SHIFT(36);  case _ => ERROR;  }
  case 33 => { case _ => REDUCE(39,12,2);  }
  case 34 => { case 3 => SHIFT(39);  case _ => ERROR;  }
  case 35 => { case 8 => SHIFT(43);  case _ => REDUCE(52,37,1);  }
  case 36 => { case 3 => SHIFT(35);  case 16 => SHIFT(36);  case 17 => SHIFT(44);  case _ => ERROR;  }
  case 37 => { case _ => REDUCE(40,13,3);  }
  case 38 => { case 17 => SHIFT(47);  case _ => ERROR;  }
  case 39 => { case 13 => SHIFT(48);  case _ => REDUCE(45,24,1);  }
  case 40 => { case 10 => SHIFT(49);  case _ => REDUCE(44,21,0);  }
  case 41 => { case _ => REDUCE(42,18,1);  }
  case 42 => { case 11 => SHIFT(51);  case _ => ERROR;  }
  case 43 => { case 3 => SHIFT(35);  case 16 => SHIFT(36);  case _ => ERROR;  }
  case 44 => { case _ => REDUCE(52,40,2);  }
  case 45 => { case 18 => SHIFT(53);  case _ => REDUCE(53,41,1);  }
  case 46 => { case 17 => SHIFT(54);  case _ => ERROR;  }
  case 47 => { case _ => REDUCE(40,14,4);  }
  case 48 => { case 3 => SHIFT(35);  case 16 => SHIFT(36);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(39);  case _ => REDUCE(44,22,1);  }
  case 50 => { case _ => REDUCE(35,2,13);  }
  case 51 => { case 3 => SHIFT(57);  case 22 => SHIFT(58);  case _ => ERROR;  }
  case 52 => { case 9 => SHIFT(63);  case _ => ERROR;  }
  case 53 => { case 3 => SHIFT(35);  case 16 => SHIFT(36);  case _ => ERROR;  }
  case 54 => { case _ => REDUCE(52,39,3);  }
  case 55 => { case _ => REDUCE(45,23,3);  }
  case 56 => { case _ => REDUCE(42,19,3);  }
  case 57 => { case 13 => SHIFT(65);  case _ => REDUCE(49,31,1);  }
  case 58 => { case _ => REDUCE(48,28,1);  }
  case 59 => { case _ => REDUCE(43,20,3);  }
  case 60 => { case 12 => SHIFT(66);  case _ => REDUCE(46,25,1);  }
  case 61 => { case 5 => SHIFT(67);  case _ => REDUCE(50,33,0);  }
  case 62 => { case 3 => SHIFT(57);  case 22 => SHIFT(58);  case _ => REDUCE(48,29,1);  }
  case 63 => { case _ => REDUCE(52,38,4);  }
  case 64 => { case _ => REDUCE(53,42,3);  }
  case 65 => { case 3 => SHIFT(70);  case _ => ERROR;  }
  case 66 => { case 3 => SHIFT(57);  case 22 => SHIFT(58);  case _ => ERROR;  }
  case 67 => { case _ => REDUCE(50,34,1);  }
  case 68 => { case 31 => SHIFT(72);  case _ => REDUCE(51,35,0);  }
  case 69 => { case _ => REDUCE(48,30,2);  }
  case 70 => { case _ => REDUCE(49,32,3);  }
  case 71 => { case _ => REDUCE(46,26,3);  }
  case 72 => { case 3 => SHIFT(74);  case _ => ERROR;  }
  case 73 => { case _ => REDUCE(47,27,3);  }
  case 74 => { case _ => REDUCE(51,36,2);  }
  case _ => { case _ => ERROR }
  }
}
