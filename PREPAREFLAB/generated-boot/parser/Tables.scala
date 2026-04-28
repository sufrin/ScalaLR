
package scalalr.parser.ScalaLR
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 36 => 2;  case 37 => 3;  }
  case 7 => { case 38 => 9;  }
  case 9 => { case 40 => 13;  }
  case 13 => { case 45 => 17;  }
  case 17 => { case 39 => 22;  }
  case 22 => { case 41 => 30;  case 42 => 31;  }
  case 26 => { case 43 => 33;  case 44 => 34;  }
  case 27 => { case 43 => 35;  case 44 => 34;  }
  case 28 => { case 43 => 36;  case 44 => 34;  }
  case 29 => { case 43 => 37;  case 44 => 34;  }
  case 31 => { case 41 => 39;  case 42 => 31;  }
  case 34 => { case 43 => 42;  case 44 => 34;  }
  case 38 => { case 39 => 43;  }
  case 40 => { case 56 => 46;  }
  case 41 => { case 56 => 47;  }
  case 43 => { case 46 => 49;  case 47 => 50;  case 49 => 51;  }
  case 45 => { case 56 => 54;  case 57 => 55;  }
  case 49 => { case 48 => 59;  }
  case 52 => { case 56 => 54;  case 57 => 61;  }
  case 57 => { case 56 => 64;  }
  case 58 => { case 47 => 65;  case 49 => 51;  }
  case 60 => { case 50 => 68;  case 51 => 69;  case 52 => 70;  case 53 => 71;  }
  case 62 => { case 56 => 54;  case 57 => 73;  }
  case 70 => { case 54 => 77;  }
  case 71 => { case 52 => 78;  case 53 => 71;  }
  case 75 => { case 50 => 80;  case 51 => 69;  case 52 => 70;  case 53 => 71;  }
  case 77 => { case 55 => 82;  }
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
  case 7 => { case 20 => SHIFT(8);  case _ => REDUCE(38,3,0);  }
  case 8 => { case 3 => SHIFT(10);  case _ => ERROR;  }
  case 9 => { case 33 => SHIFT(11);  case 34 => SHIFT(12);  case _ => REDUCE(40,9,0);  }
  case 10 => { case _ => REDUCE(38,4,2);  }
  case 11 => { case 3 => SHIFT(14);  case _ => ERROR;  }
  case 12 => { case 3 => SHIFT(15);  case _ => ERROR;  }
  case 13 => { case 32 => SHIFT(16);  case _ => REDUCE(45,21,0);  }
  case 14 => { case 34 => SHIFT(18);  case _ => ERROR;  }
  case 15 => { case 33 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 17 => { case 30 => SHIFT(21);  case _ => REDUCE(39,5,0);  }
  case 18 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(24);  case _ => ERROR;  }
  case 20 => { case _ => REDUCE(45,22,2);  }
  case 21 => { case 5 => SHIFT(25);  case _ => ERROR;  }
  case 22 => { case 25 => SHIFT(26);  case 26 => SHIFT(27);  case 27 => SHIFT(28);  case 28 => SHIFT(29);  case _ => REDUCE(41,10,0);  }
  case 23 => { case _ => REDUCE(40,7,4);  }
  case 24 => { case _ => REDUCE(40,8,4);  }
  case 25 => { case _ => REDUCE(39,6,2);  }
  case 26 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 29 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 30 => { case 29 => SHIFT(38);  case _ => ERROR;  }
  case 31 => { case 25 => SHIFT(26);  case 26 => SHIFT(27);  case 27 => SHIFT(28);  case 28 => SHIFT(29);  case _ => REDUCE(41,10,0);  }
  case 32 => { case 13 => SHIFT(40);  case 16 => SHIFT(41);  case _ => REDUCE(44,20,1);  }
  case 33 => { case _ => REDUCE(42,15,2);  }
  case 34 => { case 3 => SHIFT(32);  case _ => REDUCE(43,16,1);  }
  case 35 => { case _ => REDUCE(42,12,2);  }
  case 36 => { case _ => REDUCE(42,13,2);  }
  case 37 => { case _ => REDUCE(42,14,2);  }
  case 38 => { case 30 => SHIFT(21);  case _ => REDUCE(39,5,0);  }
  case 39 => { case _ => REDUCE(41,11,2);  }
  case 40 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 41 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 42 => { case _ => REDUCE(43,17,2);  }
  case 43 => { case 3 => SHIFT(48);  case _ => ERROR;  }
  case 44 => { case 8 => SHIFT(52);  case _ => REDUCE(56,42,1);  }
  case 45 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case 17 => SHIFT(53);  case _ => ERROR;  }
  case 46 => { case _ => REDUCE(44,18,3);  }
  case 47 => { case 17 => SHIFT(56);  case _ => ERROR;  }
  case 48 => { case 13 => SHIFT(57);  case _ => REDUCE(49,29,1);  }
  case 49 => { case 10 => SHIFT(58);  case _ => REDUCE(48,26,0);  }
  case 50 => { case _ => REDUCE(46,23,1);  }
  case 51 => { case 11 => SHIFT(60);  case _ => ERROR;  }
  case 52 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 53 => { case _ => REDUCE(56,45,2);  }
  case 54 => { case 18 => SHIFT(62);  case _ => REDUCE(57,46,1);  }
  case 55 => { case 17 => SHIFT(63);  case _ => ERROR;  }
  case 56 => { case _ => REDUCE(44,19,4);  }
  case 57 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 58 => { case 3 => SHIFT(48);  case _ => REDUCE(48,27,1);  }
  case 59 => { case _ => REDUCE(37,2,13);  }
  case 60 => { case 3 => SHIFT(66);  case 22 => SHIFT(67);  case _ => ERROR;  }
  case 61 => { case 9 => SHIFT(72);  case _ => ERROR;  }
  case 62 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 63 => { case _ => REDUCE(56,44,3);  }
  case 64 => { case _ => REDUCE(49,28,3);  }
  case 65 => { case _ => REDUCE(46,24,3);  }
  case 66 => { case 13 => SHIFT(74);  case _ => REDUCE(53,36,1);  }
  case 67 => { case _ => REDUCE(52,33,1);  }
  case 68 => { case _ => REDUCE(47,25,3);  }
  case 69 => { case 12 => SHIFT(75);  case _ => REDUCE(50,30,1);  }
  case 70 => { case 5 => SHIFT(76);  case _ => REDUCE(54,38,0);  }
  case 71 => { case 3 => SHIFT(66);  case 22 => SHIFT(67);  case _ => REDUCE(52,34,1);  }
  case 72 => { case _ => REDUCE(56,43,4);  }
  case 73 => { case _ => REDUCE(57,47,3);  }
  case 74 => { case 3 => SHIFT(79);  case _ => ERROR;  }
  case 75 => { case 3 => SHIFT(66);  case 22 => SHIFT(67);  case _ => ERROR;  }
  case 76 => { case _ => REDUCE(54,39,1);  }
  case 77 => { case 31 => SHIFT(81);  case _ => REDUCE(55,40,0);  }
  case 78 => { case _ => REDUCE(52,35,2);  }
  case 79 => { case _ => REDUCE(53,37,3);  }
  case 80 => { case _ => REDUCE(50,31,3);  }
  case 81 => { case 3 => SHIFT(83);  case _ => ERROR;  }
  case 82 => { case _ => REDUCE(51,32,3);  }
  case 83 => { case _ => REDUCE(55,41,2);  }
  case _ => { case _ => ERROR }
  }
}
