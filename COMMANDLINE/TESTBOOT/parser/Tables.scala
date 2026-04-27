
package scalalr.parser.ScalaLR
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 36 => 2;  case 37 => 3;  }
  case 9 => { case 39 => 12;  }
  case 12 => { case 44 => 16;  }
  case 16 => { case 38 => 21;  }
  case 21 => { case 40 => 29;  case 41 => 30;  }
  case 25 => { case 42 => 32;  case 43 => 33;  }
  case 26 => { case 42 => 34;  case 43 => 33;  }
  case 27 => { case 42 => 35;  case 43 => 33;  }
  case 28 => { case 42 => 36;  case 43 => 33;  }
  case 30 => { case 40 => 38;  case 41 => 30;  }
  case 33 => { case 42 => 41;  case 43 => 33;  }
  case 37 => { case 38 => 42;  }
  case 39 => { case 55 => 45;  }
  case 40 => { case 55 => 46;  }
  case 42 => { case 45 => 48;  case 46 => 49;  case 48 => 50;  }
  case 44 => { case 55 => 53;  case 56 => 54;  }
  case 48 => { case 47 => 58;  }
  case 51 => { case 55 => 53;  case 56 => 60;  }
  case 56 => { case 55 => 63;  }
  case 57 => { case 46 => 64;  case 48 => 50;  }
  case 59 => { case 49 => 67;  case 50 => 68;  case 51 => 69;  case 52 => 70;  }
  case 61 => { case 55 => 53;  case 56 => 72;  }
  case 69 => { case 53 => 76;  }
  case 70 => { case 51 => 77;  case 52 => 70;  }
  case 74 => { case 49 => 79;  case 50 => 68;  case 51 => 69;  case 52 => 70;  }
  case 76 => { case 54 => 81;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 23 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(4);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(36,1,1);  }
  case 4 => { case 24 => SHIFT(6);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case 3 => SHIFT(7);  case _ => ERROR;  }
  case 7 => { case 20 => SHIFT(8);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(9);  case _ => ERROR;  }
  case 9 => { case 33 => SHIFT(10);  case 34 => SHIFT(11);  case _ => REDUCE(39,7,0);  }
  case 10 => { case 3 => SHIFT(13);  case _ => ERROR;  }
  case 11 => { case 3 => SHIFT(14);  case _ => ERROR;  }
  case 12 => { case 32 => SHIFT(15);  case _ => REDUCE(44,19,0);  }
  case 13 => { case 34 => SHIFT(17);  case _ => ERROR;  }
  case 14 => { case 33 => SHIFT(18);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 30 => SHIFT(20);  case _ => REDUCE(38,3,0);  }
  case 17 => { case 3 => SHIFT(22);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 19 => { case _ => REDUCE(44,20,2);  }
  case 20 => { case 5 => SHIFT(24);  case _ => ERROR;  }
  case 21 => { case 25 => SHIFT(25);  case 26 => SHIFT(26);  case 27 => SHIFT(27);  case 28 => SHIFT(28);  case _ => REDUCE(40,8,0);  }
  case 22 => { case _ => REDUCE(39,5,4);  }
  case 23 => { case _ => REDUCE(39,6,4);  }
  case 24 => { case _ => REDUCE(38,4,2);  }
  case 25 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 26 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 29 => { case 29 => SHIFT(37);  case _ => ERROR;  }
  case 30 => { case 25 => SHIFT(25);  case 26 => SHIFT(26);  case 27 => SHIFT(27);  case 28 => SHIFT(28);  case _ => REDUCE(40,8,0);  }
  case 31 => { case 13 => SHIFT(39);  case 16 => SHIFT(40);  case _ => REDUCE(43,18,1);  }
  case 32 => { case _ => REDUCE(41,13,2);  }
  case 33 => { case 3 => SHIFT(31);  case _ => REDUCE(42,14,1);  }
  case 34 => { case _ => REDUCE(41,10,2);  }
  case 35 => { case _ => REDUCE(41,11,2);  }
  case 36 => { case _ => REDUCE(41,12,2);  }
  case 37 => { case 30 => SHIFT(20);  case _ => REDUCE(38,3,0);  }
  case 38 => { case _ => REDUCE(40,9,2);  }
  case 39 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 40 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 41 => { case _ => REDUCE(42,15,2);  }
  case 42 => { case 3 => SHIFT(47);  case _ => ERROR;  }
  case 43 => { case 8 => SHIFT(51);  case _ => REDUCE(55,40,1);  }
  case 44 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case 17 => SHIFT(52);  case _ => ERROR;  }
  case 45 => { case _ => REDUCE(43,16,3);  }
  case 46 => { case 17 => SHIFT(55);  case _ => ERROR;  }
  case 47 => { case 13 => SHIFT(56);  case _ => REDUCE(48,27,1);  }
  case 48 => { case 10 => SHIFT(57);  case _ => REDUCE(47,24,0);  }
  case 49 => { case _ => REDUCE(45,21,1);  }
  case 50 => { case 11 => SHIFT(59);  case _ => ERROR;  }
  case 51 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 52 => { case _ => REDUCE(55,43,2);  }
  case 53 => { case 18 => SHIFT(61);  case _ => REDUCE(56,44,1);  }
  case 54 => { case 17 => SHIFT(62);  case _ => ERROR;  }
  case 55 => { case _ => REDUCE(43,17,4);  }
  case 56 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 57 => { case 3 => SHIFT(47);  case _ => REDUCE(47,25,1);  }
  case 58 => { case _ => REDUCE(37,2,14);  }
  case 59 => { case 3 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 60 => { case 9 => SHIFT(71);  case _ => ERROR;  }
  case 61 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 62 => { case _ => REDUCE(55,42,3);  }
  case 63 => { case _ => REDUCE(48,26,3);  }
  case 64 => { case _ => REDUCE(45,22,3);  }
  case 65 => { case 13 => SHIFT(73);  case _ => REDUCE(52,34,1);  }
  case 66 => { case _ => REDUCE(51,31,1);  }
  case 67 => { case _ => REDUCE(46,23,3);  }
  case 68 => { case 12 => SHIFT(74);  case _ => REDUCE(49,28,1);  }
  case 69 => { case 5 => SHIFT(75);  case _ => REDUCE(53,36,0);  }
  case 70 => { case 3 => SHIFT(65);  case 22 => SHIFT(66);  case _ => REDUCE(51,32,1);  }
  case 71 => { case _ => REDUCE(55,41,4);  }
  case 72 => { case _ => REDUCE(56,45,3);  }
  case 73 => { case 3 => SHIFT(78);  case _ => ERROR;  }
  case 74 => { case 3 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 75 => { case _ => REDUCE(53,37,1);  }
  case 76 => { case 31 => SHIFT(80);  case _ => REDUCE(54,38,0);  }
  case 77 => { case _ => REDUCE(51,33,2);  }
  case 78 => { case _ => REDUCE(52,35,3);  }
  case 79 => { case _ => REDUCE(49,29,3);  }
  case 80 => { case 3 => SHIFT(82);  case _ => ERROR;  }
  case 81 => { case _ => REDUCE(50,30,3);  }
  case 82 => { case _ => REDUCE(54,39,2);  }
  case _ => { case _ => ERROR }
  }
}
