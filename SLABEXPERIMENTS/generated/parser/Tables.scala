
package scalalr.slab.parser
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 36 => 2;  case 37 => 3;  }
  case 4 => { case 48 => 7;  }
  case 9 => { case 48 => 10;  }
  case 10 => { case 38 => 12;  }
  case 12 => { case 40 => 16;  }
  case 13 => { case 48 => 17;  }
  case 16 => { case 45 => 21;  }
  case 21 => { case 39 => 26;  }
  case 26 => { case 41 => 34;  case 42 => 35;  }
  case 27 => { case 48 => 36;  }
  case 28 => { case 48 => 37;  }
  case 30 => { case 43 => 39;  case 44 => 40;  }
  case 31 => { case 43 => 41;  case 44 => 40;  }
  case 32 => { case 43 => 42;  case 44 => 40;  }
  case 33 => { case 43 => 43;  case 44 => 40;  }
  case 35 => { case 41 => 45;  case 42 => 35;  }
  case 40 => { case 43 => 48;  case 44 => 40;  }
  case 44 => { case 39 => 49;  }
  case 46 => { case 56 => 52;  }
  case 47 => { case 56 => 53;  }
  case 49 => { case 46 => 55;  case 47 => 56;  case 49 => 57;  }
  case 51 => { case 56 => 60;  case 57 => 61;  }
  case 55 => { case 48 => 65;  }
  case 58 => { case 56 => 60;  case 57 => 67;  }
  case 63 => { case 56 => 70;  }
  case 64 => { case 47 => 71;  case 49 => 57;  }
  case 66 => { case 50 => 74;  case 51 => 75;  case 52 => 76;  case 53 => 77;  }
  case 68 => { case 56 => 60;  case 57 => 79;  }
  case 76 => { case 54 => 83;  }
  case 77 => { case 52 => 84;  case 53 => 77;  }
  case 81 => { case 50 => 86;  case 51 => 75;  case 52 => 76;  case 53 => 77;  }
  case 83 => { case 55 => 88;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 23 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(4);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case _ => REDUCE(36,1,1);  }
  case 4 => { case 10 => SHIFT(6);  case _ => REDUCE(48,26,0);  }
  case 5 => { case _ => ACCEPT;  }
  case 6 => { case _ => REDUCE(48,27,1);  }
  case 7 => { case 24 => SHIFT(8);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(9);  case _ => ERROR;  }
  case 9 => { case 10 => SHIFT(6);  case _ => REDUCE(48,26,0);  }
  case 10 => { case 20 => SHIFT(11);  case _ => REDUCE(38,3,0);  }
  case 11 => { case 3 => SHIFT(13);  case _ => ERROR;  }
  case 12 => { case 33 => SHIFT(14);  case 34 => SHIFT(15);  case _ => REDUCE(40,9,0);  }
  case 13 => { case 10 => SHIFT(6);  case _ => REDUCE(48,26,0);  }
  case 14 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 32 => SHIFT(20);  case _ => REDUCE(45,21,0);  }
  case 17 => { case _ => REDUCE(38,4,3);  }
  case 18 => { case 34 => SHIFT(22);  case _ => ERROR;  }
  case 19 => { case 33 => SHIFT(23);  case _ => ERROR;  }
  case 20 => { case 3 => SHIFT(24);  case _ => ERROR;  }
  case 21 => { case 30 => SHIFT(25);  case _ => REDUCE(39,5,0);  }
  case 22 => { case 3 => SHIFT(27);  case _ => ERROR;  }
  case 23 => { case 3 => SHIFT(28);  case _ => ERROR;  }
  case 24 => { case _ => REDUCE(45,22,2);  }
  case 25 => { case 5 => SHIFT(29);  case _ => ERROR;  }
  case 26 => { case 25 => SHIFT(30);  case 26 => SHIFT(31);  case 27 => SHIFT(32);  case 28 => SHIFT(33);  case _ => REDUCE(41,10,0);  }
  case 27 => { case 10 => SHIFT(6);  case _ => REDUCE(48,26,0);  }
  case 28 => { case 10 => SHIFT(6);  case _ => REDUCE(48,26,0);  }
  case 29 => { case _ => REDUCE(39,6,2);  }
  case 30 => { case 3 => SHIFT(38);  case _ => ERROR;  }
  case 31 => { case 3 => SHIFT(38);  case _ => ERROR;  }
  case 32 => { case 3 => SHIFT(38);  case _ => ERROR;  }
  case 33 => { case 3 => SHIFT(38);  case _ => ERROR;  }
  case 34 => { case 29 => SHIFT(44);  case _ => ERROR;  }
  case 35 => { case 25 => SHIFT(30);  case 26 => SHIFT(31);  case 27 => SHIFT(32);  case 28 => SHIFT(33);  case _ => REDUCE(41,10,0);  }
  case 36 => { case _ => REDUCE(40,7,5);  }
  case 37 => { case _ => REDUCE(40,8,5);  }
  case 38 => { case 13 => SHIFT(46);  case 16 => SHIFT(47);  case _ => REDUCE(44,20,1);  }
  case 39 => { case _ => REDUCE(42,15,2);  }
  case 40 => { case 3 => SHIFT(38);  case _ => REDUCE(43,16,1);  }
  case 41 => { case _ => REDUCE(42,12,2);  }
  case 42 => { case _ => REDUCE(42,13,2);  }
  case 43 => { case _ => REDUCE(42,14,2);  }
  case 44 => { case 30 => SHIFT(25);  case _ => REDUCE(39,5,0);  }
  case 45 => { case _ => REDUCE(41,11,2);  }
  case 46 => { case 3 => SHIFT(50);  case 16 => SHIFT(51);  case _ => ERROR;  }
  case 47 => { case 3 => SHIFT(50);  case 16 => SHIFT(51);  case _ => ERROR;  }
  case 48 => { case _ => REDUCE(43,17,2);  }
  case 49 => { case 3 => SHIFT(54);  case _ => ERROR;  }
  case 50 => { case 8 => SHIFT(58);  case _ => REDUCE(56,42,1);  }
  case 51 => { case 3 => SHIFT(50);  case 16 => SHIFT(51);  case 17 => SHIFT(59);  case _ => ERROR;  }
  case 52 => { case _ => REDUCE(44,18,3);  }
  case 53 => { case 17 => SHIFT(62);  case _ => ERROR;  }
  case 54 => { case 13 => SHIFT(63);  case _ => REDUCE(49,29,1);  }
  case 55 => { case 10 => SHIFT(64);  case _ => REDUCE(48,26,0);  }
  case 56 => { case _ => REDUCE(46,23,1);  }
  case 57 => { case 11 => SHIFT(66);  case _ => ERROR;  }
  case 58 => { case 3 => SHIFT(50);  case 16 => SHIFT(51);  case _ => ERROR;  }
  case 59 => { case _ => REDUCE(56,45,2);  }
  case 60 => { case 18 => SHIFT(68);  case _ => REDUCE(57,46,1);  }
  case 61 => { case 17 => SHIFT(69);  case _ => ERROR;  }
  case 62 => { case _ => REDUCE(44,19,4);  }
  case 63 => { case 3 => SHIFT(50);  case 16 => SHIFT(51);  case _ => ERROR;  }
  case 64 => { case 3 => SHIFT(54);  case _ => REDUCE(48,27,1);  }
  case 65 => { case _ => REDUCE(37,2,15);  }
  case 66 => { case 3 => SHIFT(72);  case 22 => SHIFT(73);  case _ => ERROR;  }
  case 67 => { case 9 => SHIFT(78);  case _ => ERROR;  }
  case 68 => { case 3 => SHIFT(50);  case 16 => SHIFT(51);  case _ => ERROR;  }
  case 69 => { case _ => REDUCE(56,44,3);  }
  case 70 => { case _ => REDUCE(49,28,3);  }
  case 71 => { case _ => REDUCE(46,24,3);  }
  case 72 => { case 13 => SHIFT(80);  case _ => REDUCE(53,36,1);  }
  case 73 => { case _ => REDUCE(52,33,1);  }
  case 74 => { case _ => REDUCE(47,25,3);  }
  case 75 => { case 12 => SHIFT(81);  case _ => REDUCE(50,30,1);  }
  case 76 => { case 5 => SHIFT(82);  case _ => REDUCE(54,38,0);  }
  case 77 => { case 3 => SHIFT(72);  case 22 => SHIFT(73);  case _ => REDUCE(52,34,1);  }
  case 78 => { case _ => REDUCE(56,43,4);  }
  case 79 => { case _ => REDUCE(57,47,3);  }
  case 80 => { case 3 => SHIFT(85);  case _ => ERROR;  }
  case 81 => { case 3 => SHIFT(72);  case 22 => SHIFT(73);  case _ => ERROR;  }
  case 82 => { case _ => REDUCE(54,39,1);  }
  case 83 => { case 31 => SHIFT(87);  case _ => REDUCE(55,40,0);  }
  case 84 => { case _ => REDUCE(52,35,2);  }
  case 85 => { case _ => REDUCE(53,37,3);  }
  case 86 => { case _ => REDUCE(50,31,3);  }
  case 87 => { case 3 => SHIFT(89);  case _ => ERROR;  }
  case 88 => { case _ => REDUCE(51,32,3);  }
  case 89 => { case _ => REDUCE(55,41,2);  }
  case _ => { case _ => ERROR }
  }
}
