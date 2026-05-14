
package scalalr.stage2
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 37 => 1;  case 38 => 2;  case 39 => 3;  }
  case 8 => { case 42 => 23;  case 43 => 24;  }
  case 9 => { case 42 => 25;  case 43 => 24;  }
  case 10 => { case 42 => 26;  case 43 => 24;  }
  case 11 => { case 42 => 27;  case 43 => 24;  }
  case 12 => { case 40 => 29;  }
  case 14 => { case 42 => 31;  case 43 => 24;  }
  case 24 => { case 42 => 38;  case 43 => 24;  }
  case 29 => { case 44 => 41;  case 45 => 42;  case 47 => 43;  }
  case 36 => { case 54 => 46;  }
  case 37 => { case 54 => 47;  }
  case 41 => { case 41 => 51;  }
  case 45 => { case 54 => 55;  case 55 => 56;  }
  case 49 => { case 54 => 58;  }
  case 50 => { case 45 => 59;  case 47 => 43;  }
  case 52 => { case 46 => 61;  }
  case 53 => { case 54 => 55;  case 55 => 62;  }
  case 61 => { case 48 => 67;  case 49 => 68;  case 50 => 69;  case 51 => 70;  }
  case 63 => { case 54 => 55;  case 55 => 72;  }
  case 69 => { case 52 => 76;  }
  case 70 => { case 50 => 77;  case 51 => 70;  }
  case 74 => { case 48 => 79;  case 49 => 68;  case 50 => 69;  case 51 => 70;  }
  case 76 => { case 53 => 81;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case _ => REDUCE(39,3,0);  }
  case 1 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 2 => { case _ => REDUCE(37,1,1);  }
  case 3 => { case 20 => SHIFT(5);  case 23 => SHIFT(6);  case 24 => SHIFT(7);  case 25 => SHIFT(8);  case 26 => SHIFT(9);  case 27 => SHIFT(10);  case 28 => SHIFT(11);  case 29 => SHIFT(12);  case 30 => SHIFT(13);  case 31 => SHIFT(14);  case 32 => SHIFT(15);  case 33 => SHIFT(16);  case 34 => SHIFT(17);  case 35 => SHIFT(18);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(21);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(22);  case _ => REDUCE(42,21,0);  }
  case 9 => { case 3 => SHIFT(22);  case _ => REDUCE(42,21,0);  }
  case 10 => { case 3 => SHIFT(22);  case _ => REDUCE(42,21,0);  }
  case 11 => { case 3 => SHIFT(22);  case _ => REDUCE(42,21,0);  }
  case 12 => { case 30 => SHIFT(28);  case _ => REDUCE(40,18,0);  }
  case 13 => { case 5 => SHIFT(30);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(22);  case _ => REDUCE(42,21,0);  }
  case 15 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(33);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(34);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(35);  case _ => ERROR;  }
  case 19 => { case _ => REDUCE(39,6,3);  }
  case 20 => { case _ => REDUCE(39,4,3);  }
  case 21 => { case _ => REDUCE(39,5,3);  }
  case 22 => { case 13 => SHIFT(36);  case 16 => SHIFT(37);  case _ => REDUCE(43,25,1);  }
  case 23 => { case _ => REDUCE(39,9,3);  }
  case 24 => { case 3 => SHIFT(22);  case _ => REDUCE(42,21,0);  }
  case 25 => { case _ => REDUCE(39,10,3);  }
  case 26 => { case _ => REDUCE(39,11,3);  }
  case 27 => { case _ => REDUCE(39,12,3);  }
  case 28 => { case 5 => SHIFT(39);  case _ => ERROR;  }
  case 29 => { case 3 => SHIFT(40);  case _ => ERROR;  }
  case 30 => { case _ => REDUCE(39,8,3);  }
  case 31 => { case _ => REDUCE(39,13,3);  }
  case 32 => { case _ => REDUCE(39,7,3);  }
  case 33 => { case _ => REDUCE(39,14,3);  }
  case 34 => { case _ => REDUCE(39,15,3);  }
  case 35 => { case _ => REDUCE(39,16,3);  }
  case 36 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 37 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 38 => { case _ => REDUCE(42,22,2);  }
  case 39 => { case 19 => SHIFT(48);  case _ => ERROR;  }
  case 40 => { case 13 => SHIFT(49);  case _ => REDUCE(47,32,1);  }
  case 41 => { case 19 => SHIFT(50);  case _ => REDUCE(41,19,0);  }
  case 42 => { case _ => REDUCE(44,26,1);  }
  case 43 => { case 11 => SHIFT(52);  case _ => ERROR;  }
  case 44 => { case 8 => SHIFT(53);  case _ => REDUCE(54,45,1);  }
  case 45 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case 17 => SHIFT(54);  case _ => ERROR;  }
  case 46 => { case _ => REDUCE(43,23,3);  }
  case 47 => { case 17 => SHIFT(57);  case _ => ERROR;  }
  case 48 => { case _ => REDUCE(40,17,3);  }
  case 49 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 50 => { case 3 => SHIFT(40);  case _ => REDUCE(41,20,1);  }
  case 51 => { case _ => REDUCE(38,2,5);  }
  case 52 => { case 12 => SHIFT(60);  case _ => REDUCE(46,30,0);  }
  case 53 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 54 => { case _ => REDUCE(54,48,2);  }
  case 55 => { case 18 => SHIFT(63);  case _ => REDUCE(55,49,1);  }
  case 56 => { case 17 => SHIFT(64);  case _ => ERROR;  }
  case 57 => { case _ => REDUCE(43,24,4);  }
  case 58 => { case _ => REDUCE(47,31,3);  }
  case 59 => { case _ => REDUCE(44,27,3);  }
  case 60 => { case _ => REDUCE(46,29,1);  }
  case 61 => { case 3 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 62 => { case 9 => SHIFT(71);  case _ => ERROR;  }
  case 63 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 64 => { case _ => REDUCE(54,47,3);  }
  case 65 => { case 13 => SHIFT(73);  case _ => REDUCE(51,39,1);  }
  case 66 => { case _ => REDUCE(50,36,1);  }
  case 67 => { case _ => REDUCE(45,28,4);  }
  case 68 => { case 12 => SHIFT(74);  case _ => REDUCE(48,33,1);  }
  case 69 => { case 5 => SHIFT(75);  case _ => REDUCE(52,41,0);  }
  case 70 => { case 3 => SHIFT(65);  case 22 => SHIFT(66);  case _ => REDUCE(50,37,1);  }
  case 71 => { case _ => REDUCE(54,46,4);  }
  case 72 => { case _ => REDUCE(55,50,3);  }
  case 73 => { case 3 => SHIFT(78);  case _ => ERROR;  }
  case 74 => { case 3 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 75 => { case _ => REDUCE(52,42,1);  }
  case 76 => { case 31 => SHIFT(80);  case _ => REDUCE(53,43,0);  }
  case 77 => { case _ => REDUCE(50,38,2);  }
  case 78 => { case _ => REDUCE(51,40,3);  }
  case 79 => { case _ => REDUCE(48,34,3);  }
  case 80 => { case 3 => SHIFT(82);  case _ => ERROR;  }
  case 81 => { case _ => REDUCE(49,35,3);  }
  case 82 => { case _ => REDUCE(53,44,2);  }
  case _ => { case _ => ERROR }
  }
}
