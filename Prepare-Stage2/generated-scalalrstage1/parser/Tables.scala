
package scalalr.stage2
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 45 => 1;  case 46 => 2;  }
  case 7 => { case 49 => 22;  case 50 => 23;  }
  case 8 => { case 49 => 24;  case 50 => 23;  }
  case 9 => { case 49 => 25;  case 50 => 23;  }
  case 10 => { case 49 => 26;  case 50 => 23;  }
  case 11 => { case 47 => 28;  }
  case 13 => { case 49 => 30;  case 50 => 23;  }
  case 23 => { case 49 => 37;  case 50 => 23;  }
  case 28 => { case 51 => 40;  case 52 => 41;  case 54 => 42;  }
  case 35 => { case 62 => 45;  }
  case 36 => { case 62 => 46;  }
  case 40 => { case 48 => 50;  }
  case 44 => { case 62 => 54;  case 63 => 55;  }
  case 48 => { case 62 => 57;  }
  case 49 => { case 52 => 58;  case 54 => 42;  }
  case 51 => { case 53 => 60;  }
  case 52 => { case 62 => 54;  case 63 => 61;  }
  case 60 => { case 55 => 67;  case 56 => 68;  case 57 => 69;  case 58 => 70;  case 59 => 71;  }
  case 62 => { case 62 => 54;  case 63 => 73;  }
  case 65 => { case 57 => 75;  case 58 => 70;  case 59 => 71;  }
  case 69 => { case 64 => 79;  }
  case 70 => { case 57 => 80;  case 58 => 70;  case 59 => 71;  }
  case 74 => { case 59 => 82;  }
  case 76 => { case 55 => 84;  case 56 => 68;  case 57 => 69;  case 58 => 70;  case 59 => 71;  }
  case 78 => { case 65 => 89;  case 68 => 90;  }
  case 79 => { case 61 => 92;  }
  case 83 => { case 60 => 97;  }
  case 87 => { case 65 => 98;  case 68 => 90;  }
  case 100 => { case 65 => 110;  case 68 => 90;  }
  case 101 => { case 65 => 111;  case 68 => 90;  }
  case 102 => { case 65 => 112;  case 68 => 90;  }
  case 103 => { case 65 => 113;  case 68 => 90;  }
  case 104 => { case 65 => 114;  case 66 => 115;  case 67 => 116;  case 68 => 90;  }
  case 121 => { case 65 => 122;  case 68 => 90;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case _ => REDUCE(46,2,0);  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 22 => SHIFT(4);  case 25 => SHIFT(5);  case 26 => SHIFT(6);  case 27 => SHIFT(7);  case 28 => SHIFT(8);  case 29 => SHIFT(9);  case 30 => SHIFT(10);  case 31 => SHIFT(11);  case 32 => SHIFT(12);  case 33 => SHIFT(13);  case 34 => SHIFT(14);  case 35 => SHIFT(15);  case 36 => SHIFT(16);  case 37 => SHIFT(17);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(21);  case _ => REDUCE(49,20,0);  }
  case 8 => { case 3 => SHIFT(21);  case _ => REDUCE(49,20,0);  }
  case 9 => { case 3 => SHIFT(21);  case _ => REDUCE(49,20,0);  }
  case 10 => { case 3 => SHIFT(21);  case _ => REDUCE(49,20,0);  }
  case 11 => { case 32 => SHIFT(27);  case _ => REDUCE(47,17,0);  }
  case 12 => { case 5 => SHIFT(29);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(21);  case _ => REDUCE(49,20,0);  }
  case 14 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(33);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(34);  case _ => ERROR;  }
  case 18 => { case _ => REDUCE(46,5,3);  }
  case 19 => { case _ => REDUCE(46,3,3);  }
  case 20 => { case _ => REDUCE(46,4,3);  }
  case 21 => { case 13 => SHIFT(35);  case 16 => SHIFT(36);  case _ => REDUCE(50,24,1);  }
  case 22 => { case _ => REDUCE(46,8,3);  }
  case 23 => { case 3 => SHIFT(21);  case _ => REDUCE(49,20,0);  }
  case 24 => { case _ => REDUCE(46,9,3);  }
  case 25 => { case _ => REDUCE(46,10,3);  }
  case 26 => { case _ => REDUCE(46,11,3);  }
  case 27 => { case 5 => SHIFT(38);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(39);  case _ => ERROR;  }
  case 29 => { case _ => REDUCE(46,7,3);  }
  case 30 => { case _ => REDUCE(46,12,3);  }
  case 31 => { case _ => REDUCE(46,6,3);  }
  case 32 => { case _ => REDUCE(46,13,3);  }
  case 33 => { case _ => REDUCE(46,14,3);  }
  case 34 => { case _ => REDUCE(46,15,3);  }
  case 35 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 36 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 37 => { case _ => REDUCE(49,21,2);  }
  case 38 => { case 21 => SHIFT(47);  case _ => ERROR;  }
  case 39 => { case 13 => SHIFT(48);  case _ => REDUCE(54,31,1);  }
  case 40 => { case 21 => SHIFT(49);  case _ => REDUCE(48,18,0);  }
  case 41 => { case _ => REDUCE(51,25,1);  }
  case 42 => { case 11 => SHIFT(51);  case _ => ERROR;  }
  case 43 => { case 8 => SHIFT(52);  case _ => REDUCE(62,50,1);  }
  case 44 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case 17 => SHIFT(53);  case _ => ERROR;  }
  case 45 => { case _ => REDUCE(50,22,3);  }
  case 46 => { case 17 => SHIFT(56);  case _ => ERROR;  }
  case 47 => { case _ => REDUCE(47,16,3);  }
  case 48 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(39);  case _ => REDUCE(48,19,1);  }
  case 50 => { case _ => REDUCE(45,1,5);  }
  case 51 => { case 12 => SHIFT(59);  case _ => REDUCE(53,29,0);  }
  case 52 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 53 => { case _ => REDUCE(62,53,2);  }
  case 54 => { case 18 => SHIFT(62);  case _ => REDUCE(63,54,1);  }
  case 55 => { case 17 => SHIFT(63);  case _ => ERROR;  }
  case 56 => { case _ => REDUCE(50,23,4);  }
  case 57 => { case _ => REDUCE(54,30,3);  }
  case 58 => { case _ => REDUCE(51,26,3);  }
  case 59 => { case _ => REDUCE(53,28,1);  }
  case 60 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 24 => SHIFT(66);  case _ => ERROR;  }
  case 61 => { case 9 => SHIFT(72);  case _ => ERROR;  }
  case 62 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 63 => { case _ => REDUCE(62,52,3);  }
  case 64 => { case 13 => SHIFT(74);  case _ => REDUCE(59,40,1);  }
  case 65 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 24 => SHIFT(66);  case _ => ERROR;  }
  case 66 => { case _ => REDUCE(57,35,1);  }
  case 67 => { case _ => REDUCE(52,27,4);  }
  case 68 => { case 12 => SHIFT(76);  case _ => REDUCE(55,32,1);  }
  case 69 => { case 5 => SHIFT(77);  case 42 => SHIFT(78);  case _ => REDUCE(64,56,0);  }
  case 70 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 24 => SHIFT(66);  case _ => REDUCE(57,36,1);  }
  case 71 => { case _ => REDUCE(58,38,1);  }
  case 72 => { case _ => REDUCE(62,51,4);  }
  case 73 => { case _ => REDUCE(63,55,3);  }
  case 74 => { case 3 => SHIFT(81);  case 16 => SHIFT(65);  case _ => ERROR;  }
  case 75 => { case 17 => SHIFT(83);  case _ => ERROR;  }
  case 76 => { case 3 => SHIFT(64);  case 16 => SHIFT(65);  case 24 => SHIFT(66);  case _ => ERROR;  }
  case 77 => { case _ => REDUCE(64,57,1);  }
  case 78 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 79 => { case 33 => SHIFT(91);  case _ => REDUCE(61,48,0);  }
  case 80 => { case _ => REDUCE(57,37,2);  }
  case 81 => { case _ => REDUCE(59,40,1);  }
  case 82 => { case _ => REDUCE(58,39,3);  }
  case 83 => { case 19 => SHIFT(93);  case 20 => SHIFT(94);  case 39 => SHIFT(95);  case 41 => SHIFT(96);  case _ => ERROR;  }
  case 84 => { case _ => REDUCE(55,33,3);  }
  case 85 => { case _ => REDUCE(68,69,1);  }
  case 86 => { case _ => REDUCE(68,71,1);  }
  case 87 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 88 => { case 3 => SHIFT(99);  case _ => ERROR;  }
  case 89 => { case 38 => SHIFT(100);  case 39 => SHIFT(101);  case 40 => SHIFT(102);  case 41 => SHIFT(103);  case _ => REDUCE(64,58,2);  }
  case 90 => { case 16 => SHIFT(104);  case _ => REDUCE(65,59,1);  }
  case 91 => { case 3 => SHIFT(105);  case _ => ERROR;  }
  case 92 => { case _ => REDUCE(56,34,3);  }
  case 93 => { case 41 => SHIFT(106);  case _ => REDUCE(60,44,1);  }
  case 94 => { case _ => REDUCE(60,43,1);  }
  case 95 => { case 41 => SHIFT(107);  case _ => REDUCE(60,45,1);  }
  case 96 => { case 41 => SHIFT(108);  case _ => ERROR;  }
  case 97 => { case _ => REDUCE(59,41,4);  }
  case 98 => { case 17 => SHIFT(109);  case 38 => SHIFT(100);  case 39 => SHIFT(101);  case 40 => SHIFT(102);  case 41 => SHIFT(103);  case _ => ERROR;  }
  case 99 => { case _ => REDUCE(68,70,2);  }
  case 100 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 101 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 102 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 103 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 104 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => REDUCE(66,65,0);  }
  case 105 => { case _ => REDUCE(61,49,2);  }
  case 106 => { case 41 => SHIFT(117);  case _ => ERROR;  }
  case 107 => { case 41 => SHIFT(118);  case _ => ERROR;  }
  case 108 => { case 41 => SHIFT(119);  case _ => ERROR;  }
  case 109 => { case _ => REDUCE(68,72,3);  }
  case 110 => { case 38 => SHIFT(100);  case 39 => SHIFT(101);  case 40 => SHIFT(102);  case 41 => SHIFT(103);  case _ => REDUCE(65,62,3);  }
  case 111 => { case 41 => SHIFT(103);  case _ => REDUCE(65,63,3);  }
  case 112 => { case 41 => SHIFT(103);  case _ => REDUCE(65,64,3);  }
  case 113 => { case 41 => SHIFT(103);  case _ => REDUCE(65,61,3);  }
  case 114 => { case 38 => SHIFT(100);  case 39 => SHIFT(101);  case 40 => SHIFT(102);  case 41 => SHIFT(103);  case _ => REDUCE(67,67,1);  }
  case 115 => { case 17 => SHIFT(120);  case _ => ERROR;  }
  case 116 => { case 18 => SHIFT(121);  case _ => REDUCE(66,66,1);  }
  case 117 => { case _ => REDUCE(60,46,3);  }
  case 118 => { case _ => REDUCE(60,47,3);  }
  case 119 => { case _ => REDUCE(59,42,6);  }
  case 120 => { case _ => REDUCE(65,60,4);  }
  case 121 => { case 3 => SHIFT(85);  case 4 => SHIFT(86);  case 16 => SHIFT(87);  case 43 => SHIFT(88);  case _ => ERROR;  }
  case 122 => { case 38 => SHIFT(100);  case 39 => SHIFT(101);  case 40 => SHIFT(102);  case 41 => SHIFT(103);  case _ => REDUCE(67,68,3);  }
  case _ => { case _ => ERROR }
  }
}
