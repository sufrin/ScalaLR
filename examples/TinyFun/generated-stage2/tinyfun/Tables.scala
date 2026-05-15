
package tinyfun
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 21 => 1;  }
  case 1 => { case 22 => 9;  case 23 => 10;  case 24 => 11;  }
  case 6 => { case 23 => 16;  }
  case 13 => { case 23 => 41;  case 24 => 24;  }
  case 14 => { case 23 => 25;  }
  case 18 => { case 23 => 28;  }
  case 19 => { case 23 => 29;  }
  case 20 => { case 23 => 30;  }
  case 21 => { case 23 => 31;  }
  case 22 => { case 23 => 32;  }
  case 23 => { case 23 => 33;  }
  case 37 => { case 23 => 51;  }
  case 40 => { case 23 => 55;  }
  case 42 => { case 23 => 61;  }
  case 43 => { case 23 => 62;  }
  case 44 => { case 23 => 63;  }
  case 45 => { case 23 => 64;  }
  case 46 => { case 23 => 65;  }
  case 47 => { case 23 => 66;  }
  case 48 => { case 23 => 41;  case 24 => 67;  }
  case 49 => { case 23 => 68;  }
  case 52 => { case 23 => 41;  case 24 => 71;  }
  case 53 => { case 23 => 72;  }
  case 56 => { case 23 => 75;  }
  case 57 => { case 23 => 76;  }
  case 58 => { case 23 => 77;  }
  case 59 => { case 23 => 78;  }
  case 60 => { case 23 => 79;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 0 => REDUCE(21,1,0);  case 1 => REDUCE(21,1,0);  case 3 => REDUCE(21,1,0);  case 4 => REDUCE(21,1,0);  case 5 => REDUCE(21,1,0);  case 11 => REDUCE(21,1,0);  case 12 => REDUCE(21,1,0);  case 13 => REDUCE(21,1,0);  case _ => ERROR;  }
  case 1 => { case 0 => SHIFT(2);  case 1 => SHIFT(3);  case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case 12 => SHIFT(7);  case 13 => SHIFT(8);  case 11 => REDUCE(22,7,0);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case 11 => SHIFT(12);  case _ => ERROR;  }
  case 4 => { case 9 => REDUCE(23,9,1);  case 11 => REDUCE(23,9,1);  case 15 => REDUCE(23,9,1);  case 16 => REDUCE(23,9,1);  case 17 => REDUCE(23,9,1);  case 18 => REDUCE(23,9,1);  case 19 => REDUCE(23,9,1);  case _ => ERROR;  }
  case 5 => { case 5 => SHIFT(13);  case 14 => SHIFT(14);  case 9 => REDUCE(23,8,1);  case 11 => REDUCE(23,8,1);  case 15 => REDUCE(23,8,1);  case 16 => REDUCE(23,8,1);  case 17 => REDUCE(23,8,1);  case 18 => REDUCE(23,8,1);  case 19 => REDUCE(23,8,1);  case _ => ERROR;  }
  case 6 => { case 1 => SHIFT(15);  case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 7 => { case 11 => REDUCE(22,5,1);  case _ => ERROR;  }
  case 8 => { case 11 => REDUCE(22,6,1);  case _ => ERROR;  }
  case 9 => { case 11 => SHIFT(17);  case _ => ERROR;  }
  case 10 => { case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case 9 => REDUCE(24,19,1);  case 11 => REDUCE(24,19,1);  case _ => ERROR;  }
  case 11 => { case 9 => SHIFT(23);  case 11 => REDUCE(22,4,1);  case _ => ERROR;  }
  case 12 => { case 0 => REDUCE(21,3,3);  case 1 => REDUCE(21,3,3);  case 3 => REDUCE(21,3,3);  case 4 => REDUCE(21,3,3);  case 5 => REDUCE(21,3,3);  case 11 => REDUCE(21,3,3);  case 12 => REDUCE(21,3,3);  case 13 => REDUCE(21,3,3);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 15 => { case 6 => SHIFT(26);  case _ => ERROR;  }
  case 16 => { case 6 => SHIFT(27);  case 15 => SHIFT(42);  case 16 => SHIFT(43);  case 17 => SHIFT(44);  case 18 => SHIFT(45);  case 19 => SHIFT(46);  case _ => ERROR;  }
  case 17 => { case 0 => REDUCE(21,2,3);  case 1 => REDUCE(21,2,3);  case 3 => REDUCE(21,2,3);  case 4 => REDUCE(21,2,3);  case 5 => REDUCE(21,2,3);  case 11 => REDUCE(21,2,3);  case 12 => REDUCE(21,2,3);  case 13 => REDUCE(21,2,3);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 19 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 20 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 21 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 22 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 23 => { case 3 => SHIFT(4);  case 4 => SHIFT(5);  case 5 => SHIFT(6);  case _ => ERROR;  }
  case 24 => { case 6 => SHIFT(34);  case 9 => SHIFT(47);  case _ => ERROR;  }
  case 25 => { case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case 9 => REDUCE(23,10,3);  case 11 => REDUCE(23,10,3);  case _ => ERROR;  }
  case 26 => { case 9 => REDUCE(23,17,3);  case 11 => REDUCE(23,17,3);  case 15 => REDUCE(23,17,3);  case 16 => REDUCE(23,17,3);  case 17 => REDUCE(23,17,3);  case 18 => REDUCE(23,17,3);  case 19 => REDUCE(23,17,3);  case _ => ERROR;  }
  case 27 => { case 9 => REDUCE(23,16,3);  case 11 => REDUCE(23,16,3);  case 15 => REDUCE(23,16,3);  case 16 => REDUCE(23,16,3);  case 17 => REDUCE(23,16,3);  case 18 => REDUCE(23,16,3);  case 19 => REDUCE(23,16,3);  case _ => ERROR;  }
  case 28 => { case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case 9 => REDUCE(23,13,3);  case 11 => REDUCE(23,13,3);  case 15 => REDUCE(23,13,3);  case 16 => REDUCE(23,13,3);  case _ => ERROR;  }
  case 29 => { case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case 9 => REDUCE(23,15,3);  case 11 => REDUCE(23,15,3);  case 15 => REDUCE(23,15,3);  case 16 => REDUCE(23,15,3);  case _ => ERROR;  }
  case 30 => { case 19 => SHIFT(22);  case 9 => REDUCE(23,12,3);  case 11 => REDUCE(23,12,3);  case 15 => REDUCE(23,12,3);  case 16 => REDUCE(23,12,3);  case 17 => REDUCE(23,12,3);  case 18 => REDUCE(23,12,3);  case _ => ERROR;  }
  case 31 => { case 19 => SHIFT(22);  case 9 => REDUCE(23,14,3);  case 11 => REDUCE(23,14,3);  case 15 => REDUCE(23,14,3);  case 16 => REDUCE(23,14,3);  case 17 => REDUCE(23,14,3);  case 18 => REDUCE(23,14,3);  case _ => ERROR;  }
  case 32 => { case 19 => SHIFT(22);  case 9 => REDUCE(23,11,3);  case 11 => REDUCE(23,11,3);  case 15 => REDUCE(23,11,3);  case 16 => REDUCE(23,11,3);  case 17 => REDUCE(23,11,3);  case 18 => REDUCE(23,11,3);  case _ => ERROR;  }
  case 33 => { case 15 => SHIFT(18);  case 16 => SHIFT(19);  case 17 => SHIFT(20);  case 18 => SHIFT(21);  case 19 => SHIFT(22);  case 9 => REDUCE(24,20,3);  case 11 => REDUCE(24,20,3);  case _ => ERROR;  }
  case 34 => { case 9 => REDUCE(23,18,4);  case 11 => REDUCE(23,18,4);  case 15 => REDUCE(23,18,4);  case 16 => REDUCE(23,18,4);  case 17 => REDUCE(23,18,4);  case 18 => REDUCE(23,18,4);  case 19 => REDUCE(23,18,4);  case _ => ERROR;  }
  case 35 => { case 6 => REDUCE(23,9,1);  case 15 => REDUCE(23,9,1);  case 16 => REDUCE(23,9,1);  case 17 => REDUCE(23,9,1);  case 18 => REDUCE(23,9,1);  case 19 => REDUCE(23,9,1);  case _ => ERROR;  }
  case 36 => { case 5 => SHIFT(48);  case 14 => SHIFT(49);  case 6 => REDUCE(23,8,1);  case 15 => REDUCE(23,8,1);  case 16 => REDUCE(23,8,1);  case 17 => REDUCE(23,8,1);  case 18 => REDUCE(23,8,1);  case 19 => REDUCE(23,8,1);  case _ => ERROR;  }
  case 37 => { case 1 => SHIFT(50);  case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 38 => { case 6 => REDUCE(23,9,1);  case 9 => REDUCE(23,9,1);  case 15 => REDUCE(23,9,1);  case 16 => REDUCE(23,9,1);  case 17 => REDUCE(23,9,1);  case 18 => REDUCE(23,9,1);  case 19 => REDUCE(23,9,1);  case _ => ERROR;  }
  case 39 => { case 5 => SHIFT(52);  case 14 => SHIFT(53);  case 6 => REDUCE(23,8,1);  case 9 => REDUCE(23,8,1);  case 15 => REDUCE(23,8,1);  case 16 => REDUCE(23,8,1);  case 17 => REDUCE(23,8,1);  case 18 => REDUCE(23,8,1);  case 19 => REDUCE(23,8,1);  case _ => ERROR;  }
  case 40 => { case 1 => SHIFT(54);  case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 41 => { case 15 => SHIFT(56);  case 16 => SHIFT(57);  case 17 => SHIFT(58);  case 18 => SHIFT(59);  case 19 => SHIFT(60);  case 6 => REDUCE(24,19,1);  case 9 => REDUCE(24,19,1);  case _ => ERROR;  }
  case 42 => { case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 43 => { case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 44 => { case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 45 => { case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 46 => { case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 47 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 48 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(35);  case 4 => SHIFT(36);  case 5 => SHIFT(37);  case _ => ERROR;  }
  case 50 => { case 6 => SHIFT(69);  case _ => ERROR;  }
  case 51 => { case 6 => SHIFT(70);  case 15 => SHIFT(42);  case 16 => SHIFT(43);  case 17 => SHIFT(44);  case 18 => SHIFT(45);  case 19 => SHIFT(46);  case _ => ERROR;  }
  case 52 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 53 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 54 => { case 6 => SHIFT(73);  case _ => ERROR;  }
  case 55 => { case 6 => SHIFT(74);  case 15 => SHIFT(42);  case 16 => SHIFT(43);  case 17 => SHIFT(44);  case 18 => SHIFT(45);  case 19 => SHIFT(46);  case _ => ERROR;  }
  case 56 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 57 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 58 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 59 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 60 => { case 3 => SHIFT(38);  case 4 => SHIFT(39);  case 5 => SHIFT(40);  case _ => ERROR;  }
  case 61 => { case 17 => SHIFT(44);  case 18 => SHIFT(45);  case 19 => SHIFT(46);  case 6 => REDUCE(23,13,3);  case 15 => REDUCE(23,13,3);  case 16 => REDUCE(23,13,3);  case _ => ERROR;  }
  case 62 => { case 17 => SHIFT(44);  case 18 => SHIFT(45);  case 19 => SHIFT(46);  case 6 => REDUCE(23,15,3);  case 15 => REDUCE(23,15,3);  case 16 => REDUCE(23,15,3);  case _ => ERROR;  }
  case 63 => { case 19 => SHIFT(46);  case 6 => REDUCE(23,12,3);  case 15 => REDUCE(23,12,3);  case 16 => REDUCE(23,12,3);  case 17 => REDUCE(23,12,3);  case 18 => REDUCE(23,12,3);  case _ => ERROR;  }
  case 64 => { case 19 => SHIFT(46);  case 6 => REDUCE(23,14,3);  case 15 => REDUCE(23,14,3);  case 16 => REDUCE(23,14,3);  case 17 => REDUCE(23,14,3);  case 18 => REDUCE(23,14,3);  case _ => ERROR;  }
  case 65 => { case 19 => SHIFT(46);  case 6 => REDUCE(23,11,3);  case 15 => REDUCE(23,11,3);  case 16 => REDUCE(23,11,3);  case 17 => REDUCE(23,11,3);  case 18 => REDUCE(23,11,3);  case _ => ERROR;  }
  case 66 => { case 15 => SHIFT(56);  case 16 => SHIFT(57);  case 17 => SHIFT(58);  case 18 => SHIFT(59);  case 19 => SHIFT(60);  case 6 => REDUCE(24,20,3);  case 9 => REDUCE(24,20,3);  case _ => ERROR;  }
  case 67 => { case 6 => SHIFT(80);  case 9 => SHIFT(47);  case _ => ERROR;  }
  case 68 => { case 15 => SHIFT(42);  case 16 => SHIFT(43);  case 17 => SHIFT(44);  case 18 => SHIFT(45);  case 19 => SHIFT(46);  case 6 => REDUCE(23,10,3);  case _ => ERROR;  }
  case 69 => { case 6 => REDUCE(23,17,3);  case 15 => REDUCE(23,17,3);  case 16 => REDUCE(23,17,3);  case 17 => REDUCE(23,17,3);  case 18 => REDUCE(23,17,3);  case 19 => REDUCE(23,17,3);  case _ => ERROR;  }
  case 70 => { case 6 => REDUCE(23,16,3);  case 15 => REDUCE(23,16,3);  case 16 => REDUCE(23,16,3);  case 17 => REDUCE(23,16,3);  case 18 => REDUCE(23,16,3);  case 19 => REDUCE(23,16,3);  case _ => ERROR;  }
  case 71 => { case 6 => SHIFT(81);  case 9 => SHIFT(47);  case _ => ERROR;  }
  case 72 => { case 15 => SHIFT(56);  case 16 => SHIFT(57);  case 17 => SHIFT(58);  case 18 => SHIFT(59);  case 19 => SHIFT(60);  case 6 => REDUCE(23,10,3);  case 9 => REDUCE(23,10,3);  case _ => ERROR;  }
  case 73 => { case 6 => REDUCE(23,17,3);  case 9 => REDUCE(23,17,3);  case 15 => REDUCE(23,17,3);  case 16 => REDUCE(23,17,3);  case 17 => REDUCE(23,17,3);  case 18 => REDUCE(23,17,3);  case 19 => REDUCE(23,17,3);  case _ => ERROR;  }
  case 74 => { case 6 => REDUCE(23,16,3);  case 9 => REDUCE(23,16,3);  case 15 => REDUCE(23,16,3);  case 16 => REDUCE(23,16,3);  case 17 => REDUCE(23,16,3);  case 18 => REDUCE(23,16,3);  case 19 => REDUCE(23,16,3);  case _ => ERROR;  }
  case 75 => { case 17 => SHIFT(58);  case 18 => SHIFT(59);  case 19 => SHIFT(60);  case 6 => REDUCE(23,13,3);  case 9 => REDUCE(23,13,3);  case 15 => REDUCE(23,13,3);  case 16 => REDUCE(23,13,3);  case _ => ERROR;  }
  case 76 => { case 17 => SHIFT(58);  case 18 => SHIFT(59);  case 19 => SHIFT(60);  case 6 => REDUCE(23,15,3);  case 9 => REDUCE(23,15,3);  case 15 => REDUCE(23,15,3);  case 16 => REDUCE(23,15,3);  case _ => ERROR;  }
  case 77 => { case 19 => SHIFT(60);  case 6 => REDUCE(23,12,3);  case 9 => REDUCE(23,12,3);  case 15 => REDUCE(23,12,3);  case 16 => REDUCE(23,12,3);  case 17 => REDUCE(23,12,3);  case 18 => REDUCE(23,12,3);  case _ => ERROR;  }
  case 78 => { case 19 => SHIFT(60);  case 6 => REDUCE(23,14,3);  case 9 => REDUCE(23,14,3);  case 15 => REDUCE(23,14,3);  case 16 => REDUCE(23,14,3);  case 17 => REDUCE(23,14,3);  case 18 => REDUCE(23,14,3);  case _ => ERROR;  }
  case 79 => { case 19 => SHIFT(60);  case 6 => REDUCE(23,11,3);  case 9 => REDUCE(23,11,3);  case 15 => REDUCE(23,11,3);  case 16 => REDUCE(23,11,3);  case 17 => REDUCE(23,11,3);  case 18 => REDUCE(23,11,3);  case _ => ERROR;  }
  case 80 => { case 6 => REDUCE(23,18,4);  case 15 => REDUCE(23,18,4);  case 16 => REDUCE(23,18,4);  case 17 => REDUCE(23,18,4);  case 18 => REDUCE(23,18,4);  case 19 => REDUCE(23,18,4);  case _ => ERROR;  }
  case 81 => { case 6 => REDUCE(23,18,4);  case 9 => REDUCE(23,18,4);  case 15 => REDUCE(23,18,4);  case 16 => REDUCE(23,18,4);  case 17 => REDUCE(23,18,4);  case 18 => REDUCE(23,18,4);  case 19 => REDUCE(23,18,4);  case _ => ERROR;  }
  case _ => { case _ => ERROR }
  }
}
