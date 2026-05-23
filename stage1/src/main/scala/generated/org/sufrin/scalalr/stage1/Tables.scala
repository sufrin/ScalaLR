
package org.sufrin.scalalr.stage1.ScalaLR
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 36 => 2;  }
  case 6 => { case 39 => 8;  }
  case 8 => { case 41 => 12;  }
  case 12 => { case 46 => 16;  }
  case 16 => { case 40 => 21;  }
  case 21 => { case 42 => 29;  case 43 => 30;  }
  case 25 => { case 44 => 32;  case 45 => 33;  }
  case 26 => { case 44 => 34;  case 45 => 33;  }
  case 27 => { case 44 => 35;  case 45 => 33;  }
  case 28 => { case 44 => 36;  case 45 => 33;  }
  case 29 => { case 37 => 38;  }
  case 30 => { case 42 => 39;  case 43 => 30;  }
  case 33 => { case 44 => 42;  case 45 => 33;  }
  case 38 => { case 40 => 43;  }
  case 40 => { case 57 => 46;  }
  case 41 => { case 57 => 47;  }
  case 45 => { case 57 => 51;  case 58 => 52;  }
  case 48 => { case 47 => 55;  case 48 => 56;  case 50 => 57;  }
  case 49 => { case 57 => 51;  case 58 => 58;  }
  case 55 => { case 38 => 63;  }
  case 59 => { case 57 => 51;  case 58 => 66;  }
  case 61 => { case 57 => 67;  }
  case 62 => { case 48 => 68;  case 50 => 57;  }
  case 64 => { case 49 => 70;  }
  case 70 => { case 51 => 73;  case 52 => 74;  case 53 => 75;  case 54 => 76;  }
  case 75 => { case 55 => 80;  }
  case 76 => { case 53 => 81;  case 54 => 76;  }
  case 78 => { case 51 => 83;  case 52 => 74;  case 53 => 75;  case 54 => 76;  }
  case 80 => { case 56 => 85;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 23 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 3 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 24 => SHIFT(5);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 3 => SHIFT(6);  case _ => ERROR;  }
  case 6 => { case 20 => SHIFT(7);  case _ => REDUCE(39,5,0);  }
  case 7 => { case 3 => SHIFT(9);  case _ => ERROR;  }
  case 8 => { case 33 => SHIFT(10);  case 34 => SHIFT(11);  case _ => REDUCE(41,11,0);  }
  case 9 => { case _ => REDUCE(39,6,2);  }
  case 10 => { case 3 => SHIFT(13);  case _ => ERROR;  }
  case 11 => { case 3 => SHIFT(14);  case _ => ERROR;  }
  case 12 => { case 32 => SHIFT(15);  case _ => REDUCE(46,23,0);  }
  case 13 => { case 34 => SHIFT(17);  case _ => ERROR;  }
  case 14 => { case 33 => SHIFT(18);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 30 => SHIFT(20);  case _ => REDUCE(40,7,0);  }
  case 17 => { case 3 => SHIFT(22);  case _ => ERROR;  }
  case 18 => { case 3 => SHIFT(23);  case _ => ERROR;  }
  case 19 => { case _ => REDUCE(46,24,2);  }
  case 20 => { case 5 => SHIFT(24);  case _ => ERROR;  }
  case 21 => { case 25 => SHIFT(25);  case 26 => SHIFT(26);  case 27 => SHIFT(27);  case 28 => SHIFT(28);  case _ => REDUCE(42,12,0);  }
  case 22 => { case _ => REDUCE(41,9,4);  }
  case 23 => { case _ => REDUCE(41,10,4);  }
  case 24 => { case _ => REDUCE(40,8,2);  }
  case 25 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 26 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 29 => { case 29 => SHIFT(37);  case _ => ERROR;  }
  case 30 => { case 25 => SHIFT(25);  case 26 => SHIFT(26);  case 27 => SHIFT(27);  case 28 => SHIFT(28);  case _ => REDUCE(42,12,0);  }
  case 31 => { case 13 => SHIFT(40);  case 16 => SHIFT(41);  case _ => REDUCE(45,22,1);  }
  case 32 => { case _ => REDUCE(43,17,2);  }
  case 33 => { case 3 => SHIFT(31);  case _ => REDUCE(44,18,1);  }
  case 34 => { case _ => REDUCE(43,14,2);  }
  case 35 => { case _ => REDUCE(43,15,2);  }
  case 36 => { case _ => REDUCE(43,16,2);  }
  case 37 => { case _ => REDUCE(37,2,1);  }
  case 38 => { case 30 => SHIFT(20);  case _ => REDUCE(40,7,0);  }
  case 39 => { case _ => REDUCE(42,13,2);  }
  case 40 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 41 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 42 => { case _ => REDUCE(44,19,2);  }
  case 43 => { case 19 => SHIFT(48);  case _ => ERROR;  }
  case 44 => { case 8 => SHIFT(49);  case _ => REDUCE(57,44,1);  }
  case 45 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case 17 => SHIFT(50);  case _ => ERROR;  }
  case 46 => { case _ => REDUCE(45,20,3);  }
  case 47 => { case 17 => SHIFT(53);  case _ => ERROR;  }
  case 48 => { case 3 => SHIFT(54);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 50 => { case _ => REDUCE(57,47,2);  }
  case 51 => { case 18 => SHIFT(59);  case _ => REDUCE(58,48,1);  }
  case 52 => { case 17 => SHIFT(60);  case _ => ERROR;  }
  case 53 => { case _ => REDUCE(45,21,4);  }
  case 54 => { case 13 => SHIFT(61);  case _ => REDUCE(50,31,1);  }
  case 55 => { case 19 => SHIFT(62);  case _ => REDUCE(38,3,0);  }
  case 56 => { case _ => REDUCE(47,25,1);  }
  case 57 => { case 11 => SHIFT(64);  case _ => ERROR;  }
  case 58 => { case 9 => SHIFT(65);  case _ => ERROR;  }
  case 59 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 60 => { case _ => REDUCE(57,46,3);  }
  case 61 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 62 => { case 3 => SHIFT(54);  case _ => REDUCE(38,4,1);  }
  case 63 => { case _ => REDUCE(36,1,14);  }
  case 64 => { case 12 => SHIFT(69);  case _ => REDUCE(49,29,0);  }
  case 65 => { case _ => REDUCE(57,45,4);  }
  case 66 => { case _ => REDUCE(58,49,3);  }
  case 67 => { case _ => REDUCE(50,30,3);  }
  case 68 => { case _ => REDUCE(47,26,3);  }
  case 69 => { case _ => REDUCE(49,28,1);  }
  case 70 => { case 3 => SHIFT(71);  case 22 => SHIFT(72);  case _ => ERROR;  }
  case 71 => { case 13 => SHIFT(77);  case _ => REDUCE(54,38,1);  }
  case 72 => { case _ => REDUCE(53,35,1);  }
  case 73 => { case _ => REDUCE(48,27,4);  }
  case 74 => { case 12 => SHIFT(78);  case _ => REDUCE(51,32,1);  }
  case 75 => { case 5 => SHIFT(79);  case _ => REDUCE(55,40,0);  }
  case 76 => { case 3 => SHIFT(71);  case 22 => SHIFT(72);  case _ => REDUCE(53,36,1);  }
  case 77 => { case 3 => SHIFT(82);  case _ => ERROR;  }
  case 78 => { case 3 => SHIFT(71);  case 22 => SHIFT(72);  case _ => ERROR;  }
  case 79 => { case _ => REDUCE(55,41,1);  }
  case 80 => { case 31 => SHIFT(84);  case _ => REDUCE(56,42,0);  }
  case 81 => { case _ => REDUCE(53,37,2);  }
  case 82 => { case _ => REDUCE(54,39,3);  }
  case 83 => { case _ => REDUCE(51,33,3);  }
  case 84 => { case 3 => SHIFT(86);  case _ => ERROR;  }
  case 85 => { case _ => REDUCE(52,34,3);  }
  case 86 => { case _ => REDUCE(56,43,2);  }
  case _ => { case _ => ERROR }
  }
}
