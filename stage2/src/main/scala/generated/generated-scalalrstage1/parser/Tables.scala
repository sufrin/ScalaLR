
package scalalr.stage2
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 40 => 1;  case 41 => 2;  }
  case 7 => { case 44 => 22;  case 45 => 23;  }
  case 8 => { case 44 => 24;  case 45 => 23;  }
  case 9 => { case 44 => 25;  case 45 => 23;  }
  case 10 => { case 44 => 26;  case 45 => 23;  }
  case 11 => { case 42 => 28;  }
  case 13 => { case 44 => 30;  case 45 => 23;  }
  case 23 => { case 44 => 37;  case 45 => 23;  }
  case 28 => { case 46 => 40;  case 47 => 41;  case 49 => 42;  }
  case 35 => { case 58 => 45;  }
  case 36 => { case 58 => 46;  }
  case 40 => { case 43 => 50;  }
  case 44 => { case 58 => 54;  case 59 => 55;  }
  case 48 => { case 58 => 57;  }
  case 49 => { case 47 => 58;  case 49 => 42;  }
  case 51 => { case 48 => 60;  }
  case 52 => { case 58 => 54;  case 59 => 61;  }
  case 60 => { case 50 => 67;  case 51 => 68;  case 52 => 69;  case 53 => 70;  case 54 => 71;  }
  case 62 => { case 58 => 54;  case 59 => 73;  }
  case 65 => { case 52 => 75;  case 53 => 70;  case 54 => 71;  }
  case 69 => { case 56 => 78;  }
  case 70 => { case 52 => 79;  case 53 => 70;  case 54 => 71;  }
  case 74 => { case 54 => 81;  }
  case 76 => { case 50 => 83;  case 51 => 68;  case 52 => 69;  case 53 => 70;  case 54 => 71;  }
  case 78 => { case 57 => 85;  }
  case 82 => { case 55 => 89;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(41,2,0);  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 20 => SHIFT(4);  case 23 => SHIFT(5);  case 24 => SHIFT(6);  case 25 => SHIFT(7);  case 26 => SHIFT(8);  case 27 => SHIFT(9);  case 28 => SHIFT(10);  case 29 => SHIFT(11);  case 30 => SHIFT(12);  case 31 => SHIFT(13);  case 32 => SHIFT(14);  case 33 => SHIFT(15);  case 34 => SHIFT(16);  case 35 => SHIFT(17);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(21);  case _ => REDUCE(44,20,0);  }
  case 8 => { case 3 => SHIFT(21);  case _ => REDUCE(44,20,0);  }
  case 9 => { case 3 => SHIFT(21);  case _ => REDUCE(44,20,0);  }
  case 10 => { case 3 => SHIFT(21);  case _ => REDUCE(44,20,0);  }
  case 11 => { case 30 => SHIFT(27);  case _ => REDUCE(42,17,0);  }
  case 12 => { case 5 => SHIFT(29);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(21);  case _ => REDUCE(44,20,0);  }
  case 14 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(33);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(34);  case _ => ERROR;  }
  case 18 => { case _ => REDUCE(41,5,3);  }
  case 19 => { case _ => REDUCE(41,3,3);  }
  case 20 => { case _ => REDUCE(41,4,3);  }
  case 21 => { case 13 => SHIFT(35);  case 16 => SHIFT(36);  case _ => REDUCE(45,24,1);  }
  case 22 => { case _ => REDUCE(41,8,3);  }
  case 23 => { case 3 => SHIFT(21);  case _ => REDUCE(44,20,0);  }
  case 24 => { case _ => REDUCE(41,9,3);  }
  case 25 => { case _ => REDUCE(41,10,3);  }
  case 26 => { case _ => REDUCE(41,11,3);  }
  case 27 => { case 5 => SHIFT(38);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(39);  case _ => ERROR;  }
  case 29 => { case _ => REDUCE(41,7,3);  }
  case 30 => { case _ => REDUCE(41,12,3);  }
  case 31 => { case _ => REDUCE(41,6,3);  }
  case 32 => { case _ => REDUCE(41,13,3);  }
  case 33 => { case _ => REDUCE(41,14,3);  }
  case 34 => { case _ => REDUCE(41,15,3);  }
  case 35 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 36 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 37 => { case _ => REDUCE(44,21,2);  }
  case 38 => { case 19 => SHIFT(47);  case _ => ERROR;  }
  case 39 => { case 13 => SHIFT(48);  case _ => REDUCE(49,31,1);  }
  case 40 => { case 19 => SHIFT(49);  case _ => REDUCE(43,18,0);  }
  case 41 => { case _ => REDUCE(46,25,1);  }
  case 42 => { case 11 => SHIFT(51);  case _ => ERROR;  }
  case 43 => { case 8 => SHIFT(52);  case _ => REDUCE(58,49,1);  }
  case 44 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case 17 => SHIFT(53);  case _ => ERROR;  }
  case 45 => { case _ => REDUCE(45,22,3);  }
  case 46 => { case 17 => SHIFT(56);  case _ => ERROR;  }
  case 47 => { case _ => REDUCE(42,16,3);  }
  case 48 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(39);  case _ => REDUCE(43,19,1);  }
  case 50 => { case _ => REDUCE(40,1,5);  }
  case 51 => { case 12 => SHIFT(59);  case _ => REDUCE(48,29,0);  }
  case 52 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 53 => { case _ => REDUCE(58,52,2);  }
  case 54 => { case 18 => SHIFT(62);  case _ => REDUCE(59,53,1);  }
  case 55 => { case 17 => SHIFT(63);  case _ => ERROR;  }
  case 56 => { case _ => REDUCE(45,23,4);  }
  case 57 => { case _ => REDUCE(49,30,3);  }
  case 58 => { case _ => REDUCE(46,26,3);  }
  case 59 => { case _ => REDUCE(48,28,1);  }
  case 60 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 61 => { case 9 => SHIFT(72);  case _ => ERROR;  }
  case 62 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 63 => { case _ => REDUCE(58,51,3);  }
  case 64 => { case 13 => SHIFT(74);  case _ => REDUCE(54,40,1);  }
  case 65 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 66 => { case _ => REDUCE(52,35,1);  }
  case 67 => { case _ => REDUCE(47,27,4);  }
  case 68 => { case 12 => SHIFT(76);  case _ => REDUCE(50,32,1);  }
  case 69 => { case 5 => SHIFT(77);  case _ => REDUCE(56,45,0);  }
  case 70 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 22 => SHIFT(66);  case _ => REDUCE(52,36,1);  }
  case 71 => { case _ => REDUCE(53,38,1);  }
  case 72 => { case _ => REDUCE(58,50,4);  }
  case 73 => { case _ => REDUCE(59,54,3);  }
  case 74 => { case 3 => SHIFT(80);  case 16 => SHIFT(65);  case _ => ERROR;  }
  case 75 => { case 17 => SHIFT(82);  case _ => ERROR;  }
  case 76 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 22 => SHIFT(66);  case _ => ERROR;  }
  case 77 => { case _ => REDUCE(56,46,1);  }
  case 78 => { case 31 => SHIFT(84);  case _ => REDUCE(57,47,0);  }
  case 79 => { case _ => REDUCE(52,37,2);  }
  case 80 => { case _ => REDUCE(54,40,1);  }
  case 81 => { case _ => REDUCE(53,39,3);  }
  case 82 => { case 36 => SHIFT(86);  case 37 => SHIFT(87);  case 38 => SHIFT(88);  case _ => ERROR;  }
  case 83 => { case _ => REDUCE(50,33,3);  }
  case 84 => { case 3 => SHIFT(90);  case _ => ERROR;  }
  case 85 => { case _ => REDUCE(51,34,3);  }
  case 86 => { case _ => REDUCE(55,42,1);  }
  case 87 => { case _ => REDUCE(55,43,1);  }
  case 88 => { case _ => REDUCE(55,44,1);  }
  case 89 => { case _ => REDUCE(54,41,4);  }
  case 90 => { case _ => REDUCE(57,48,2);  }
  case _ => { case _ => ERROR }
  }
}
