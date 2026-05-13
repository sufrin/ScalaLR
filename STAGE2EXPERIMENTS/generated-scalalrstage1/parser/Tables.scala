
package scalalr.stage2
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 37 => 1;  case 38 => 2;  }
  case 7 => { case 41 => 21;  case 42 => 22;  }
  case 8 => { case 41 => 23;  case 42 => 22;  }
  case 9 => { case 41 => 24;  case 42 => 22;  }
  case 10 => { case 41 => 25;  case 42 => 22;  }
  case 11 => { case 39 => 27;  }
  case 22 => { case 41 => 35;  case 42 => 22;  }
  case 27 => { case 43 => 38;  case 44 => 39;  case 46 => 40;  }
  case 33 => { case 53 => 43;  }
  case 34 => { case 53 => 44;  }
  case 38 => { case 40 => 48;  }
  case 42 => { case 53 => 52;  case 54 => 53;  }
  case 46 => { case 53 => 55;  }
  case 47 => { case 44 => 56;  case 46 => 40;  }
  case 49 => { case 45 => 58;  }
  case 50 => { case 53 => 52;  case 54 => 59;  }
  case 58 => { case 47 => 64;  case 48 => 65;  case 49 => 66;  case 50 => 67;  }
  case 60 => { case 53 => 52;  case 54 => 69;  }
  case 66 => { case 51 => 73;  }
  case 67 => { case 49 => 74;  case 50 => 67;  }
  case 71 => { case 47 => 76;  case 48 => 65;  case 49 => 66;  case 50 => 67;  }
  case 73 => { case 52 => 78;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(38,2,0);  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 20 => SHIFT(4);  case 23 => SHIFT(5);  case 24 => SHIFT(6);  case 25 => SHIFT(7);  case 26 => SHIFT(8);  case 27 => SHIFT(9);  case 28 => SHIFT(10);  case 29 => SHIFT(11);  case 30 => SHIFT(12);  case 32 => SHIFT(13);  case 33 => SHIFT(14);  case 34 => SHIFT(15);  case 35 => SHIFT(16);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(17);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(20);  case _ => REDUCE(41,19,0);  }
  case 8 => { case 3 => SHIFT(20);  case _ => REDUCE(41,19,0);  }
  case 9 => { case 3 => SHIFT(20);  case _ => REDUCE(41,19,0);  }
  case 10 => { case 3 => SHIFT(20);  case _ => REDUCE(41,19,0);  }
  case 11 => { case 30 => SHIFT(26);  case _ => REDUCE(39,16,0);  }
  case 12 => { case 5 => SHIFT(28);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(29);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(30);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 17 => { case _ => REDUCE(38,5,3);  }
  case 18 => { case _ => REDUCE(38,3,3);  }
  case 19 => { case _ => REDUCE(38,4,3);  }
  case 20 => { case 13 => SHIFT(33);  case 16 => SHIFT(34);  case _ => REDUCE(42,23,1);  }
  case 21 => { case _ => REDUCE(38,8,3);  }
  case 22 => { case 3 => SHIFT(20);  case _ => REDUCE(41,19,0);  }
  case 23 => { case _ => REDUCE(38,9,3);  }
  case 24 => { case _ => REDUCE(38,10,3);  }
  case 25 => { case _ => REDUCE(38,11,3);  }
  case 26 => { case 5 => SHIFT(36);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(37);  case _ => ERROR;  }
  case 28 => { case _ => REDUCE(38,7,3);  }
  case 29 => { case _ => REDUCE(38,6,3);  }
  case 30 => { case _ => REDUCE(38,12,3);  }
  case 31 => { case _ => REDUCE(38,13,3);  }
  case 32 => { case _ => REDUCE(38,14,3);  }
  case 33 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 34 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 35 => { case _ => REDUCE(41,20,2);  }
  case 36 => { case 19 => SHIFT(45);  case _ => ERROR;  }
  case 37 => { case 13 => SHIFT(46);  case _ => REDUCE(46,30,1);  }
  case 38 => { case 19 => SHIFT(47);  case _ => REDUCE(40,17,0);  }
  case 39 => { case _ => REDUCE(43,24,1);  }
  case 40 => { case 11 => SHIFT(49);  case _ => ERROR;  }
  case 41 => { case 8 => SHIFT(50);  case _ => REDUCE(53,43,1);  }
  case 42 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case 17 => SHIFT(51);  case _ => ERROR;  }
  case 43 => { case _ => REDUCE(42,21,3);  }
  case 44 => { case 17 => SHIFT(54);  case _ => ERROR;  }
  case 45 => { case _ => REDUCE(39,15,3);  }
  case 46 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 47 => { case 3 => SHIFT(37);  case _ => REDUCE(40,18,1);  }
  case 48 => { case _ => REDUCE(37,1,5);  }
  case 49 => { case 12 => SHIFT(57);  case _ => REDUCE(45,28,0);  }
  case 50 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 51 => { case _ => REDUCE(53,46,2);  }
  case 52 => { case 18 => SHIFT(60);  case _ => REDUCE(54,47,1);  }
  case 53 => { case 17 => SHIFT(61);  case _ => ERROR;  }
  case 54 => { case _ => REDUCE(42,22,4);  }
  case 55 => { case _ => REDUCE(46,29,3);  }
  case 56 => { case _ => REDUCE(43,25,3);  }
  case 57 => { case _ => REDUCE(45,27,1);  }
  case 58 => { case 3 => SHIFT(62);  case 22 => SHIFT(63);  case _ => ERROR;  }
  case 59 => { case 9 => SHIFT(68);  case _ => ERROR;  }
  case 60 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 61 => { case _ => REDUCE(53,45,3);  }
  case 62 => { case 13 => SHIFT(70);  case _ => REDUCE(50,37,1);  }
  case 63 => { case _ => REDUCE(49,34,1);  }
  case 64 => { case _ => REDUCE(44,26,4);  }
  case 65 => { case 12 => SHIFT(71);  case _ => REDUCE(47,31,1);  }
  case 66 => { case 5 => SHIFT(72);  case _ => REDUCE(51,39,0);  }
  case 67 => { case 3 => SHIFT(62);  case 22 => SHIFT(63);  case _ => REDUCE(49,35,1);  }
  case 68 => { case _ => REDUCE(53,44,4);  }
  case 69 => { case _ => REDUCE(54,48,3);  }
  case 70 => { case 3 => SHIFT(75);  case _ => ERROR;  }
  case 71 => { case 3 => SHIFT(62);  case 22 => SHIFT(63);  case _ => ERROR;  }
  case 72 => { case _ => REDUCE(51,40,1);  }
  case 73 => { case 31 => SHIFT(77);  case _ => REDUCE(52,41,0);  }
  case 74 => { case _ => REDUCE(49,36,2);  }
  case 75 => { case _ => REDUCE(50,38,3);  }
  case 76 => { case _ => REDUCE(47,32,3);  }
  case 77 => { case 3 => SHIFT(79);  case _ => ERROR;  }
  case 78 => { case _ => REDUCE(48,33,3);  }
  case 79 => { case _ => REDUCE(52,42,2);  }
  case _ => { case _ => ERROR }
  }
}
