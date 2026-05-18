
package scalalr.stage2
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 40 => 1;  case 41 => 2;  }
  case 7 => { case 44 => 21;  case 45 => 22;  }
  case 8 => { case 44 => 23;  case 45 => 22;  }
  case 9 => { case 44 => 24;  case 45 => 22;  }
  case 10 => { case 44 => 25;  case 45 => 22;  }
  case 11 => { case 42 => 27;  }
  case 22 => { case 44 => 35;  case 45 => 22;  }
  case 27 => { case 46 => 38;  case 47 => 39;  case 49 => 40;  }
  case 33 => { case 58 => 43;  }
  case 34 => { case 58 => 44;  }
  case 38 => { case 43 => 48;  }
  case 42 => { case 58 => 52;  case 59 => 53;  }
  case 46 => { case 58 => 55;  }
  case 47 => { case 47 => 56;  case 49 => 40;  }
  case 49 => { case 48 => 58;  }
  case 50 => { case 58 => 52;  case 59 => 59;  }
  case 58 => { case 50 => 65;  case 51 => 66;  case 52 => 67;  case 53 => 68;  case 54 => 69;  }
  case 60 => { case 58 => 52;  case 59 => 71;  }
  case 63 => { case 52 => 73;  case 53 => 68;  case 54 => 69;  }
  case 67 => { case 56 => 76;  }
  case 68 => { case 52 => 77;  case 53 => 68;  case 54 => 69;  }
  case 72 => { case 54 => 79;  }
  case 74 => { case 50 => 81;  case 51 => 66;  case 52 => 67;  case 53 => 68;  case 54 => 69;  }
  case 76 => { case 57 => 83;  }
  case 80 => { case 55 => 87;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(41,2,0);  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 20 => SHIFT(4);  case 23 => SHIFT(5);  case 24 => SHIFT(6);  case 25 => SHIFT(7);  case 26 => SHIFT(8);  case 27 => SHIFT(9);  case 28 => SHIFT(10);  case 29 => SHIFT(11);  case 30 => SHIFT(12);  case 32 => SHIFT(13);  case 33 => SHIFT(14);  case 34 => SHIFT(15);  case 35 => SHIFT(16);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(17);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(20);  case _ => REDUCE(44,19,0);  }
  case 8 => { case 3 => SHIFT(20);  case _ => REDUCE(44,19,0);  }
  case 9 => { case 3 => SHIFT(20);  case _ => REDUCE(44,19,0);  }
  case 10 => { case 3 => SHIFT(20);  case _ => REDUCE(44,19,0);  }
  case 11 => { case 30 => SHIFT(26);  case _ => REDUCE(42,16,0);  }
  case 12 => { case 5 => SHIFT(28);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(29);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(30);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 17 => { case _ => REDUCE(41,5,3);  }
  case 18 => { case _ => REDUCE(41,3,3);  }
  case 19 => { case _ => REDUCE(41,4,3);  }
  case 20 => { case 13 => SHIFT(33);  case 16 => SHIFT(34);  case _ => REDUCE(45,23,1);  }
  case 21 => { case _ => REDUCE(41,8,3);  }
  case 22 => { case 3 => SHIFT(20);  case _ => REDUCE(44,19,0);  }
  case 23 => { case _ => REDUCE(41,9,3);  }
  case 24 => { case _ => REDUCE(41,10,3);  }
  case 25 => { case _ => REDUCE(41,11,3);  }
  case 26 => { case 5 => SHIFT(36);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(37);  case _ => ERROR;  }
  case 28 => { case _ => REDUCE(41,7,3);  }
  case 29 => { case _ => REDUCE(41,6,3);  }
  case 30 => { case _ => REDUCE(41,12,3);  }
  case 31 => { case _ => REDUCE(41,13,3);  }
  case 32 => { case _ => REDUCE(41,14,3);  }
  case 33 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 34 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 35 => { case _ => REDUCE(44,20,2);  }
  case 36 => { case 19 => SHIFT(45);  case _ => ERROR;  }
  case 37 => { case 13 => SHIFT(46);  case _ => REDUCE(49,30,1);  }
  case 38 => { case 19 => SHIFT(47);  case _ => REDUCE(43,17,0);  }
  case 39 => { case _ => REDUCE(46,24,1);  }
  case 40 => { case 11 => SHIFT(49);  case _ => ERROR;  }
  case 41 => { case 8 => SHIFT(50);  case _ => REDUCE(58,48,1);  }
  case 42 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case 17 => SHIFT(51);  case _ => ERROR;  }
  case 43 => { case _ => REDUCE(45,21,3);  }
  case 44 => { case 17 => SHIFT(54);  case _ => ERROR;  }
  case 45 => { case _ => REDUCE(42,15,3);  }
  case 46 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 47 => { case 3 => SHIFT(37);  case _ => REDUCE(43,18,1);  }
  case 48 => { case _ => REDUCE(40,1,5);  }
  case 49 => { case 12 => SHIFT(57);  case _ => REDUCE(48,28,0);  }
  case 50 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 51 => { case _ => REDUCE(58,51,2);  }
  case 52 => { case 18 => SHIFT(60);  case _ => REDUCE(59,52,1);  }
  case 53 => { case 17 => SHIFT(61);  case _ => ERROR;  }
  case 54 => { case _ => REDUCE(45,22,4);  }
  case 55 => { case _ => REDUCE(49,29,3);  }
  case 56 => { case _ => REDUCE(46,25,3);  }
  case 57 => { case _ => REDUCE(48,27,1);  }
  case 58 => { case 3 => SHIFT(62);  case 16 => SHIFT(63);  case 22 => SHIFT(64);  case _ => ERROR;  }
  case 59 => { case 9 => SHIFT(70);  case _ => ERROR;  }
  case 60 => { case 3 => SHIFT(41);  case 16 => SHIFT(42);  case _ => ERROR;  }
  case 61 => { case _ => REDUCE(58,50,3);  }
  case 62 => { case 13 => SHIFT(72);  case _ => REDUCE(54,39,1);  }
  case 63 => { case 3 => SHIFT(62);  case 16 => SHIFT(63);  case 22 => SHIFT(64);  case _ => ERROR;  }
  case 64 => { case _ => REDUCE(52,34,1);  }
  case 65 => { case _ => REDUCE(47,26,4);  }
  case 66 => { case 12 => SHIFT(74);  case _ => REDUCE(50,31,1);  }
  case 67 => { case 5 => SHIFT(75);  case _ => REDUCE(56,44,0);  }
  case 68 => { case 3 => SHIFT(62);  case 16 => SHIFT(63);  case 22 => SHIFT(64);  case _ => REDUCE(52,35,1);  }
  case 69 => { case _ => REDUCE(53,37,1);  }
  case 70 => { case _ => REDUCE(58,49,4);  }
  case 71 => { case _ => REDUCE(59,53,3);  }
  case 72 => { case 3 => SHIFT(78);  case 16 => SHIFT(63);  case _ => ERROR;  }
  case 73 => { case 17 => SHIFT(80);  case _ => ERROR;  }
  case 74 => { case 3 => SHIFT(62);  case 16 => SHIFT(63);  case 22 => SHIFT(64);  case _ => ERROR;  }
  case 75 => { case _ => REDUCE(56,45,1);  }
  case 76 => { case 31 => SHIFT(82);  case _ => REDUCE(57,46,0);  }
  case 77 => { case _ => REDUCE(52,36,2);  }
  case 78 => { case _ => REDUCE(54,39,1);  }
  case 79 => { case _ => REDUCE(53,38,3);  }
  case 80 => { case 36 => SHIFT(84);  case 37 => SHIFT(85);  case 38 => SHIFT(86);  case _ => ERROR;  }
  case 81 => { case _ => REDUCE(50,32,3);  }
  case 82 => { case 3 => SHIFT(88);  case _ => ERROR;  }
  case 83 => { case _ => REDUCE(51,33,3);  }
  case 84 => { case _ => REDUCE(55,41,1);  }
  case 85 => { case _ => REDUCE(55,42,1);  }
  case 86 => { case _ => REDUCE(55,43,1);  }
  case 87 => { case _ => REDUCE(54,40,4);  }
  case 88 => { case _ => REDUCE(57,47,2);  }
  case _ => { case _ => ERROR }
  }
}
