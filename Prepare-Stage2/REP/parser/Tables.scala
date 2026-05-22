
package scalalr.stage2
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 40 => 1;  case 41 => 2;  }
  case 7 => { case 43 => 21;  case 44 => 22;  case 61 => 23;  case 62 => 24;  }
  case 8 => { case 43 => 25;  case 44 => 22;  case 61 => 23;  case 62 => 24;  }
  case 9 => { case 43 => 26;  case 44 => 22;  case 61 => 23;  case 62 => 24;  }
  case 10 => { case 43 => 27;  case 44 => 22;  case 61 => 23;  case 62 => 24;  }
  case 11 => { case 42 => 29;  }
  case 23 => { case 44 => 37;  }
  case 29 => { case 45 => 40;  case 47 => 41;  case 58 => 42;  case 59 => 43;  }
  case 35 => { case 56 => 46;  }
  case 36 => { case 56 => 47;  }
  case 42 => { case 45 => 51;  case 47 => 41;  }
  case 43 => { case 60 => 53;  }
  case 45 => { case 56 => 56;  case 57 => 57;  }
  case 49 => { case 56 => 59;  }
  case 50 => { case 46 => 61;  }
  case 54 => { case 56 => 56;  case 57 => 62;  }
  case 61 => { case 48 => 68;  case 49 => 69;  case 50 => 70;  case 51 => 71;  case 52 => 72;  case 63 => 73;  case 64 => 74;  }
  case 63 => { case 56 => 56;  case 57 => 76;  }
  case 66 => { case 50 => 78;  case 51 => 71;  case 52 => 72;  }
  case 70 => { case 54 => 80;  }
  case 71 => { case 50 => 81;  case 51 => 71;  case 52 => 72;  }
  case 77 => { case 52 => 84;  }
  case 80 => { case 55 => 87;  }
  case 82 => { case 49 => 88;  case 50 => 70;  case 51 => 71;  case 52 => 72;  }
  case 85 => { case 53 => 92;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case _ => REDUCE(41,2,0);  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 20 => SHIFT(4);  case 23 => SHIFT(5);  case 24 => SHIFT(6);  case 25 => SHIFT(7);  case 26 => SHIFT(8);  case 27 => SHIFT(9);  case 28 => SHIFT(10);  case 29 => SHIFT(11);  case 30 => SHIFT(12);  case 32 => SHIFT(13);  case 33 => SHIFT(14);  case 34 => SHIFT(15);  case 35 => SHIFT(16);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(17);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(20);  case _ => REDUCE(62,56,0);  }
  case 8 => { case 3 => SHIFT(20);  case _ => REDUCE(62,56,0);  }
  case 9 => { case 3 => SHIFT(20);  case _ => REDUCE(62,56,0);  }
  case 10 => { case 3 => SHIFT(20);  case _ => REDUCE(62,56,0);  }
  case 11 => { case 30 => SHIFT(28);  case _ => REDUCE(42,16,0);  }
  case 12 => { case 5 => SHIFT(30);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(33);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(34);  case _ => ERROR;  }
  case 17 => { case _ => REDUCE(41,5,3);  }
  case 18 => { case _ => REDUCE(41,3,3);  }
  case 19 => { case _ => REDUCE(41,4,3);  }
  case 20 => { case 13 => SHIFT(35);  case 16 => SHIFT(36);  case _ => REDUCE(44,20,1);  }
  case 21 => { case _ => REDUCE(41,8,3);  }
  case 22 => { case _ => REDUCE(61,53,1);  }
  case 23 => { case 3 => SHIFT(20);  case _ => REDUCE(62,55,1);  }
  case 24 => { case _ => REDUCE(43,17,1);  }
  case 25 => { case _ => REDUCE(41,9,3);  }
  case 26 => { case _ => REDUCE(41,10,3);  }
  case 27 => { case _ => REDUCE(41,11,3);  }
  case 28 => { case 5 => SHIFT(38);  case _ => ERROR;  }
  case 29 => { case 3 => SHIFT(39);  case _ => ERROR;  }
  case 30 => { case _ => REDUCE(41,7,3);  }
  case 31 => { case _ => REDUCE(41,6,3);  }
  case 32 => { case _ => REDUCE(41,12,3);  }
  case 33 => { case _ => REDUCE(41,13,3);  }
  case 34 => { case _ => REDUCE(41,14,3);  }
  case 35 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 36 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 37 => { case _ => REDUCE(61,54,2);  }
  case 38 => { case 19 => SHIFT(48);  case _ => ERROR;  }
  case 39 => { case 13 => SHIFT(49);  case _ => REDUCE(47,25,1);  }
  case 40 => { case _ => REDUCE(58,48,1);  }
  case 41 => { case 11 => SHIFT(50);  case _ => ERROR;  }
  case 42 => { case 3 => SHIFT(39);  case _ => REDUCE(59,50,1);  }
  case 43 => { case 19 => SHIFT(52);  case _ => REDUCE(60,51,0);  }
  case 44 => { case 8 => SHIFT(54);  case _ => REDUCE(56,42,1);  }
  case 45 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case 17 => SHIFT(55);  case _ => ERROR;  }
  case 46 => { case _ => REDUCE(44,18,3);  }
  case 47 => { case 17 => SHIFT(58);  case _ => ERROR;  }
  case 48 => { case _ => REDUCE(42,15,3);  }
  case 49 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 50 => { case 12 => SHIFT(60);  case _ => REDUCE(46,23,0);  }
  case 51 => { case _ => REDUCE(58,49,2);  }
  case 52 => { case _ => REDUCE(60,52,1);  }
  case 53 => { case _ => REDUCE(40,1,5);  }
  case 54 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 55 => { case _ => REDUCE(56,45,2);  }
  case 56 => { case 18 => SHIFT(63);  case _ => REDUCE(57,46,1);  }
  case 57 => { case 17 => SHIFT(64);  case _ => ERROR;  }
  case 58 => { case _ => REDUCE(44,19,4);  }
  case 59 => { case _ => REDUCE(47,24,3);  }
  case 60 => { case _ => REDUCE(46,22,1);  }
  case 61 => { case 3 => SHIFT(65);  case 16 => SHIFT(66);  case 22 => SHIFT(67);  case _ => ERROR;  }
  case 62 => { case 9 => SHIFT(75);  case _ => ERROR;  }
  case 63 => { case 3 => SHIFT(44);  case 16 => SHIFT(45);  case _ => ERROR;  }
  case 64 => { case _ => REDUCE(56,44,3);  }
  case 65 => { case 13 => SHIFT(77);  case _ => REDUCE(52,33,1);  }
  case 66 => { case 3 => SHIFT(65);  case 16 => SHIFT(66);  case 22 => SHIFT(67);  case _ => ERROR;  }
  case 67 => { case _ => REDUCE(50,28,1);  }
  case 68 => { case _ => REDUCE(45,21,4);  }
  case 69 => { case _ => REDUCE(63,57,1);  }
  case 70 => { case 5 => SHIFT(79);  case _ => REDUCE(54,38,0);  }
  case 71 => { case 3 => SHIFT(65);  case 16 => SHIFT(66);  case 22 => SHIFT(67);  case _ => REDUCE(50,29,1);  }
  case 72 => { case _ => REDUCE(51,31,1);  }
  case 73 => { case 12 => SHIFT(82);  case _ => REDUCE(64,59,1);  }
  case 74 => { case _ => REDUCE(48,26,1);  }
  case 75 => { case _ => REDUCE(56,43,4);  }
  case 76 => { case _ => REDUCE(57,47,3);  }
  case 77 => { case 3 => SHIFT(83);  case 16 => SHIFT(66);  case _ => ERROR;  }
  case 78 => { case 17 => SHIFT(85);  case _ => ERROR;  }
  case 79 => { case _ => REDUCE(54,39,1);  }
  case 80 => { case 31 => SHIFT(86);  case _ => REDUCE(55,40,0);  }
  case 81 => { case _ => REDUCE(50,30,2);  }
  case 82 => { case 3 => SHIFT(65);  case 16 => SHIFT(66);  case 22 => SHIFT(67);  case _ => ERROR;  }
  case 83 => { case _ => REDUCE(52,33,1);  }
  case 84 => { case _ => REDUCE(51,32,3);  }
  case 85 => { case 36 => SHIFT(89);  case 37 => SHIFT(90);  case 38 => SHIFT(91);  case _ => ERROR;  }
  case 86 => { case 3 => SHIFT(93);  case _ => ERROR;  }
  case 87 => { case _ => REDUCE(49,27,3);  }
  case 88 => { case _ => REDUCE(63,58,3);  }
  case 89 => { case _ => REDUCE(53,35,1);  }
  case 90 => { case _ => REDUCE(53,36,1);  }
  case 91 => { case _ => REDUCE(53,37,1);  }
  case 92 => { case _ => REDUCE(52,34,4);  }
  case 93 => { case _ => REDUCE(55,41,2);  }
  case _ => { case _ => ERROR }
  }
}
