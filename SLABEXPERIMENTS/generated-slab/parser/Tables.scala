
package scalalr.slab.parser
object Tables {case class ErroneousGoto(state: Int, symbol: Int) extends Throwable
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 36 => 2;  case 37 => 3;  }
  case 7 => { case 40 => 9;  }
  case 9 => { case 42 => 13;  }
  case 13 => { case 47 => 17;  }
  case 17 => { case 41 => 22;  }
  case 22 => { case 43 => 30;  case 44 => 31;  }
  case 26 => { case 45 => 33;  case 46 => 34;  }
  case 27 => { case 45 => 35;  case 46 => 34;  }
  case 28 => { case 45 => 36;  case 46 => 34;  }
  case 29 => { case 45 => 37;  case 46 => 34;  }
  case 30 => { case 38 => 39;  }
  case 31 => { case 43 => 40;  case 44 => 31;  }
  case 34 => { case 45 => 43;  case 46 => 34;  }
  case 39 => { case 41 => 44;  }
  case 41 => { case 58 => 47;  }
  case 42 => { case 58 => 48;  }
  case 46 => { case 58 => 52;  case 59 => 53;  }
  case 49 => { case 48 => 56;  case 49 => 57;  case 51 => 58;  }
  case 50 => { case 58 => 52;  case 59 => 59;  }
  case 56 => { case 39 => 64;  }
  case 60 => { case 58 => 52;  case 59 => 67;  }
  case 62 => { case 58 => 68;  }
  case 63 => { case 49 => 69;  case 51 => 58;  }
  case 65 => { case 50 => 71;  }
  case 71 => { case 52 => 74;  case 53 => 75;  case 54 => 76;  case 55 => 77;  }
  case 76 => { case 56 => 81;  }
  case 77 => { case 54 => 82;  case 55 => 77;  }
  case 79 => { case 52 => 84;  case 53 => 75;  case 54 => 76;  case 55 => 77;  }
  case 81 => { case 57 => 86;  }
  case state => { case symbol => throw ErroneousGoto(state, symbol)}
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
  case 7 => { case 20 => SHIFT(8);  case _ => REDUCE(40,6,0);  }
  case 8 => { case 3 => SHIFT(10);  case _ => ERROR;  }
  case 9 => { case 33 => SHIFT(11);  case 34 => SHIFT(12);  case _ => REDUCE(42,12,0);  }
  case 10 => { case _ => REDUCE(40,7,2);  }
  case 11 => { case 3 => SHIFT(14);  case _ => ERROR;  }
  case 12 => { case 3 => SHIFT(15);  case _ => ERROR;  }
  case 13 => { case 32 => SHIFT(16);  case _ => REDUCE(47,24,0);  }
  case 14 => { case 34 => SHIFT(18);  case _ => ERROR;  }
  case 15 => { case 33 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 17 => { case 30 => SHIFT(21);  case _ => REDUCE(41,8,0);  }
  case 18 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(24);  case _ => ERROR;  }
  case 20 => { case _ => REDUCE(47,25,2);  }
  case 21 => { case 5 => SHIFT(25);  case _ => ERROR;  }
  case 22 => { case 25 => SHIFT(26);  case 26 => SHIFT(27);  case 27 => SHIFT(28);  case 28 => SHIFT(29);  case _ => REDUCE(43,13,0);  }
  case 23 => { case _ => REDUCE(42,10,4);  }
  case 24 => { case _ => REDUCE(42,11,4);  }
  case 25 => { case _ => REDUCE(41,9,2);  }
  case 26 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 29 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 30 => { case 29 => SHIFT(38);  case _ => ERROR;  }
  case 31 => { case 25 => SHIFT(26);  case 26 => SHIFT(27);  case 27 => SHIFT(28);  case 28 => SHIFT(29);  case _ => REDUCE(43,13,0);  }
  case 32 => { case 13 => SHIFT(41);  case 16 => SHIFT(42);  case _ => REDUCE(46,23,1);  }
  case 33 => { case _ => REDUCE(44,18,2);  }
  case 34 => { case 3 => SHIFT(32);  case _ => REDUCE(45,19,1);  }
  case 35 => { case _ => REDUCE(44,15,2);  }
  case 36 => { case _ => REDUCE(44,16,2);  }
  case 37 => { case _ => REDUCE(44,17,2);  }
  case 38 => { case _ => REDUCE(38,3,1);  }
  case 39 => { case 30 => SHIFT(21);  case _ => REDUCE(41,8,0);  }
  case 40 => { case _ => REDUCE(43,14,2);  }
  case 41 => { case 3 => SHIFT(45);  case 16 => SHIFT(46);  case _ => ERROR;  }
  case 42 => { case 3 => SHIFT(45);  case 16 => SHIFT(46);  case _ => ERROR;  }
  case 43 => { case _ => REDUCE(45,20,2);  }
  case 44 => { case 19 => SHIFT(49);  case _ => ERROR;  }
  case 45 => { case 8 => SHIFT(50);  case _ => REDUCE(58,45,1);  }
  case 46 => { case 3 => SHIFT(45);  case 16 => SHIFT(46);  case 17 => SHIFT(51);  case _ => ERROR;  }
  case 47 => { case _ => REDUCE(46,21,3);  }
  case 48 => { case 17 => SHIFT(54);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(55);  case _ => ERROR;  }
  case 50 => { case 3 => SHIFT(45);  case 16 => SHIFT(46);  case _ => ERROR;  }
  case 51 => { case _ => REDUCE(58,48,2);  }
  case 52 => { case 18 => SHIFT(60);  case _ => REDUCE(59,49,1);  }
  case 53 => { case 17 => SHIFT(61);  case _ => ERROR;  }
  case 54 => { case _ => REDUCE(46,22,4);  }
  case 55 => { case 13 => SHIFT(62);  case _ => REDUCE(51,32,1);  }
  case 56 => { case 19 => SHIFT(63);  case _ => REDUCE(39,4,0);  }
  case 57 => { case _ => REDUCE(48,26,1);  }
  case 58 => { case 11 => SHIFT(65);  case _ => ERROR;  }
  case 59 => { case 9 => SHIFT(66);  case _ => ERROR;  }
  case 60 => { case 3 => SHIFT(45);  case 16 => SHIFT(46);  case _ => ERROR;  }
  case 61 => { case _ => REDUCE(58,47,3);  }
  case 62 => { case 3 => SHIFT(45);  case 16 => SHIFT(46);  case _ => ERROR;  }
  case 63 => { case 3 => SHIFT(55);  case _ => REDUCE(39,5,1);  }
  case 64 => { case _ => REDUCE(37,2,14);  }
  case 65 => { case 12 => SHIFT(70);  case _ => REDUCE(50,30,0);  }
  case 66 => { case _ => REDUCE(58,46,4);  }
  case 67 => { case _ => REDUCE(59,50,3);  }
  case 68 => { case _ => REDUCE(51,31,3);  }
  case 69 => { case _ => REDUCE(48,27,3);  }
  case 70 => { case _ => REDUCE(50,29,1);  }
  case 71 => { case 3 => SHIFT(72);  case 22 => SHIFT(73);  case _ => ERROR;  }
  case 72 => { case 13 => SHIFT(78);  case _ => REDUCE(55,39,1);  }
  case 73 => { case _ => REDUCE(54,36,1);  }
  case 74 => { case _ => REDUCE(49,28,4);  }
  case 75 => { case 12 => SHIFT(79);  case _ => REDUCE(52,33,1);  }
  case 76 => { case 5 => SHIFT(80);  case _ => REDUCE(56,41,0);  }
  case 77 => { case 3 => SHIFT(72);  case 22 => SHIFT(73);  case _ => REDUCE(54,37,1);  }
  case 78 => { case 3 => SHIFT(83);  case _ => ERROR;  }
  case 79 => { case 3 => SHIFT(72);  case 22 => SHIFT(73);  case _ => ERROR;  }
  case 80 => { case _ => REDUCE(56,42,1);  }
  case 81 => { case 31 => SHIFT(85);  case _ => REDUCE(57,43,0);  }
  case 82 => { case _ => REDUCE(54,38,2);  }
  case 83 => { case _ => REDUCE(55,40,3);  }
  case 84 => { case _ => REDUCE(52,34,3);  }
  case 85 => { case 3 => SHIFT(87);  case _ => ERROR;  }
  case 86 => { case _ => REDUCE(53,35,3);  }
  case 87 => { case _ => REDUCE(57,44,2);  }
  case _ => { case _ => ERROR }
  }
}
