
package tinyfun
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 24 => 1;  }
  case 1 => { case 25 => 14;  case 26 => 15;  case 27 => 16;  case 28 => 17;  case 29 => 18;  case 30 => 19;  case 31 => 20;  }
  case 7 => { case 26 => 22;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 11 => { case 27 => 24;  case 30 => 19;  case 31 => 25;  }
  case 23 => { case 26 => 81;  case 27 => 82;  case 28 => 39;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 26 => { case 26 => 81;  case 27 => 82;  case 28 => 40;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 27 => { case 26 => 81;  case 27 => 82;  case 28 => 41;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 29 => { case 26 => 42;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 30 => { case 26 => 43;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 31 => { case 26 => 44;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 32 => { case 26 => 45;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 33 => { case 26 => 46;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 34 => { case 26 => 47;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 35 => { case 26 => 81;  case 27 => 82;  case 28 => 48;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 36 => { case 26 => 49;  case 27 => 16;  case 30 => 19;  case 31 => 20;  }
  case 58 => { case 26 => 87;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 60 => { case 27 => 90;  case 30 => 64;  case 31 => 91;  }
  case 67 => { case 26 => 96;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 68 => { case 26 => 97;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 69 => { case 26 => 98;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 70 => { case 26 => 99;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 71 => { case 26 => 100;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 76 => { case 26 => 102;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 78 => { case 27 => 105;  case 30 => 84;  case 31 => 106;  }
  case 88 => { case 26 => 81;  case 27 => 82;  case 28 => 119;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 92 => { case 26 => 81;  case 27 => 82;  case 28 => 120;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 93 => { case 26 => 81;  case 27 => 82;  case 28 => 121;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 94 => { case 26 => 81;  case 27 => 82;  case 28 => 122;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 95 => { case 26 => 123;  case 27 => 63;  case 30 => 64;  case 31 => 65;  }
  case 103 => { case 26 => 81;  case 27 => 82;  case 28 => 126;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 107 => { case 26 => 81;  case 27 => 82;  case 28 => 127;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 108 => { case 26 => 81;  case 27 => 82;  case 28 => 128;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 109 => { case 26 => 129;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case 110 => { case 26 => 130;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case 111 => { case 26 => 131;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case 112 => { case 26 => 132;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case 113 => { case 26 => 133;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case 114 => { case 26 => 134;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case 115 => { case 26 => 81;  case 27 => 82;  case 28 => 135;  case 29 => 83;  case 30 => 84;  case 31 => 85;  }
  case 116 => { case 26 => 136;  case 27 => 82;  case 30 => 84;  case 31 => 85;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 0 => REDUCE(24,1,0);  case 3 => REDUCE(24,1,0);  case 4 => REDUCE(24,1,0);  case 5 => REDUCE(24,1,0);  case 6 => REDUCE(24,1,0);  case 7 => REDUCE(24,1,0);  case 13 => REDUCE(24,1,0);  case 15 => REDUCE(24,1,0);  case 17 => REDUCE(24,1,0);  case 18 => REDUCE(24,1,0);  case 19 => REDUCE(24,1,0);  case 20 => REDUCE(24,1,0);  case _ => ERROR;  }
  case 1 => { case 0 => SHIFT(2);  case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 13 => SHIFT(8);  case 15 => SHIFT(9);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 2 => { case _ => ACCEPT;  }
  case 3 => { case 11 => REDUCE(30,25,1);  case 14 => REDUCE(30,25,1);  case 17 => REDUCE(30,25,1);  case 18 => REDUCE(30,25,1);  case 19 => REDUCE(30,25,1);  case 20 => REDUCE(30,25,1);  case 21 => REDUCE(30,25,1);  case _ => ERROR;  }
  case 4 => { case 11 => REDUCE(30,26,1);  case 14 => REDUCE(30,26,1);  case 17 => REDUCE(30,26,1);  case 18 => REDUCE(30,26,1);  case 19 => REDUCE(30,26,1);  case 20 => REDUCE(30,26,1);  case 21 => REDUCE(30,26,1);  case _ => ERROR;  }
  case 5 => { case 11 => REDUCE(30,27,1);  case 14 => REDUCE(30,27,1);  case 17 => REDUCE(30,27,1);  case 18 => REDUCE(30,27,1);  case 19 => REDUCE(30,27,1);  case 20 => REDUCE(30,27,1);  case 21 => REDUCE(30,27,1);  case _ => ERROR;  }
  case 6 => { case 7 => REDUCE(31,28,1);  case 11 => REDUCE(31,28,1);  case 14 => REDUCE(31,28,1);  case 16 => REDUCE(31,28,1);  case 17 => REDUCE(31,28,1);  case 18 => REDUCE(31,28,1);  case 19 => REDUCE(31,28,1);  case 20 => REDUCE(31,28,1);  case 21 => REDUCE(31,28,1);  case _ => ERROR;  }
  case 7 => { case 1 => SHIFT(21);  case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 8 => { case 14 => REDUCE(25,4,1);  case _ => ERROR;  }
  case 9 => { case 14 => REDUCE(25,5,1);  case _ => ERROR;  }
  case 10 => { case 7 => SHIFT(23);  case _ => ERROR;  }
  case 11 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(66);  case 7 => SHIFT(7);  case _ => ERROR;  }
  case 12 => { case 7 => SHIFT(26);  case _ => ERROR;  }
  case 13 => { case 7 => SHIFT(27);  case _ => ERROR;  }
  case 14 => { case 14 => SHIFT(28);  case _ => ERROR;  }
  case 15 => { case 17 => SHIFT(29);  case 18 => SHIFT(30);  case 19 => SHIFT(31);  case 20 => SHIFT(32);  case 21 => SHIFT(33);  case 11 => REDUCE(29,23,1);  case 14 => REDUCE(29,23,1);  case _ => ERROR;  }
  case 16 => { case 11 => REDUCE(26,6,1);  case 14 => REDUCE(26,6,1);  case 17 => REDUCE(26,6,1);  case 18 => REDUCE(26,6,1);  case 19 => REDUCE(26,6,1);  case 20 => REDUCE(26,6,1);  case 21 => REDUCE(26,6,1);  case _ => ERROR;  }
  case 17 => { case 14 => REDUCE(25,3,1);  case _ => ERROR;  }
  case 18 => { case 11 => SHIFT(34);  case 14 => REDUCE(28,22,1);  case _ => ERROR;  }
  case 19 => { case 11 => REDUCE(27,19,1);  case 14 => REDUCE(27,19,1);  case 17 => REDUCE(27,19,1);  case 18 => REDUCE(27,19,1);  case 19 => REDUCE(27,19,1);  case 20 => REDUCE(27,19,1);  case 21 => REDUCE(27,19,1);  case _ => ERROR;  }
  case 20 => { case 7 => SHIFT(35);  case 16 => SHIFT(36);  case 11 => REDUCE(27,18,1);  case 14 => REDUCE(27,18,1);  case 17 => REDUCE(27,18,1);  case 18 => REDUCE(27,18,1);  case 19 => REDUCE(27,18,1);  case 20 => REDUCE(27,18,1);  case 21 => REDUCE(27,18,1);  case _ => ERROR;  }
  case 21 => { case 8 => SHIFT(37);  case _ => ERROR;  }
  case 22 => { case 8 => SHIFT(38);  case 17 => SHIFT(67);  case 18 => SHIFT(68);  case 19 => SHIFT(69);  case 20 => SHIFT(70);  case 21 => SHIFT(71);  case _ => ERROR;  }
  case 23 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 24 => { case 11 => REDUCE(26,7,2);  case 14 => REDUCE(26,7,2);  case 17 => REDUCE(26,7,2);  case 18 => REDUCE(26,7,2);  case 19 => REDUCE(26,7,2);  case 20 => REDUCE(26,7,2);  case 21 => REDUCE(26,7,2);  case _ => ERROR;  }
  case 25 => { case 11 => REDUCE(27,18,1);  case 14 => REDUCE(27,18,1);  case 17 => REDUCE(27,18,1);  case 18 => REDUCE(27,18,1);  case 19 => REDUCE(27,18,1);  case 20 => REDUCE(27,18,1);  case 21 => REDUCE(27,18,1);  case _ => ERROR;  }
  case 26 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 27 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 28 => { case 0 => REDUCE(24,2,3);  case 3 => REDUCE(24,2,3);  case 4 => REDUCE(24,2,3);  case 5 => REDUCE(24,2,3);  case 6 => REDUCE(24,2,3);  case 7 => REDUCE(24,2,3);  case 13 => REDUCE(24,2,3);  case 15 => REDUCE(24,2,3);  case 17 => REDUCE(24,2,3);  case 18 => REDUCE(24,2,3);  case 19 => REDUCE(24,2,3);  case 20 => REDUCE(24,2,3);  case _ => ERROR;  }
  case 29 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 30 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 31 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 32 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 33 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 34 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 35 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 36 => { case 3 => SHIFT(3);  case 4 => SHIFT(4);  case 5 => SHIFT(5);  case 6 => SHIFT(6);  case 7 => SHIFT(7);  case 17 => SHIFT(10);  case 18 => SHIFT(11);  case 19 => SHIFT(12);  case 20 => SHIFT(13);  case _ => ERROR;  }
  case 37 => { case 11 => REDUCE(27,21,3);  case 14 => REDUCE(27,21,3);  case 17 => REDUCE(27,21,3);  case 18 => REDUCE(27,21,3);  case 19 => REDUCE(27,21,3);  case 20 => REDUCE(27,21,3);  case 21 => REDUCE(27,21,3);  case _ => ERROR;  }
  case 38 => { case 11 => REDUCE(27,20,3);  case 14 => REDUCE(27,20,3);  case 17 => REDUCE(27,20,3);  case 18 => REDUCE(27,20,3);  case 19 => REDUCE(27,20,3);  case 20 => REDUCE(27,20,3);  case 21 => REDUCE(27,20,3);  case _ => ERROR;  }
  case 39 => { case 8 => SHIFT(50);  case _ => ERROR;  }
  case 40 => { case 8 => SHIFT(51);  case _ => ERROR;  }
  case 41 => { case 8 => SHIFT(52);  case _ => ERROR;  }
  case 42 => { case 19 => SHIFT(31);  case 20 => SHIFT(32);  case 21 => SHIFT(33);  case 11 => REDUCE(26,11,3);  case 14 => REDUCE(26,11,3);  case 17 => REDUCE(26,11,3);  case 18 => REDUCE(26,11,3);  case _ => ERROR;  }
  case 43 => { case 19 => SHIFT(31);  case 20 => SHIFT(32);  case 21 => SHIFT(33);  case 11 => REDUCE(26,13,3);  case 14 => REDUCE(26,13,3);  case 17 => REDUCE(26,13,3);  case 18 => REDUCE(26,13,3);  case _ => ERROR;  }
  case 44 => { case 21 => SHIFT(33);  case 11 => REDUCE(26,10,3);  case 14 => REDUCE(26,10,3);  case 17 => REDUCE(26,10,3);  case 18 => REDUCE(26,10,3);  case 19 => REDUCE(26,10,3);  case 20 => REDUCE(26,10,3);  case _ => ERROR;  }
  case 45 => { case 21 => SHIFT(33);  case 11 => REDUCE(26,12,3);  case 14 => REDUCE(26,12,3);  case 17 => REDUCE(26,12,3);  case 18 => REDUCE(26,12,3);  case 19 => REDUCE(26,12,3);  case 20 => REDUCE(26,12,3);  case _ => ERROR;  }
  case 46 => { case 21 => SHIFT(33);  case 11 => REDUCE(26,9,3);  case 14 => REDUCE(26,9,3);  case 17 => REDUCE(26,9,3);  case 18 => REDUCE(26,9,3);  case 19 => REDUCE(26,9,3);  case 20 => REDUCE(26,9,3);  case _ => ERROR;  }
  case 47 => { case 17 => SHIFT(29);  case 18 => SHIFT(30);  case 19 => SHIFT(31);  case 20 => SHIFT(32);  case 21 => SHIFT(33);  case 11 => REDUCE(29,24,3);  case 14 => REDUCE(29,24,3);  case _ => ERROR;  }
  case 48 => { case 8 => SHIFT(53);  case _ => ERROR;  }
  case 49 => { case 17 => SHIFT(29);  case 18 => SHIFT(30);  case 19 => SHIFT(31);  case 20 => SHIFT(32);  case 21 => SHIFT(33);  case 11 => REDUCE(26,8,3);  case 14 => REDUCE(26,8,3);  case _ => ERROR;  }
  case 50 => { case 11 => REDUCE(26,16,4);  case 14 => REDUCE(26,16,4);  case 17 => REDUCE(26,16,4);  case 18 => REDUCE(26,16,4);  case 19 => REDUCE(26,16,4);  case 20 => REDUCE(26,16,4);  case 21 => REDUCE(26,16,4);  case _ => ERROR;  }
  case 51 => { case 11 => REDUCE(26,15,4);  case 14 => REDUCE(26,15,4);  case 17 => REDUCE(26,15,4);  case 18 => REDUCE(26,15,4);  case 19 => REDUCE(26,15,4);  case 20 => REDUCE(26,15,4);  case 21 => REDUCE(26,15,4);  case _ => ERROR;  }
  case 52 => { case 11 => REDUCE(26,17,4);  case 14 => REDUCE(26,17,4);  case 17 => REDUCE(26,17,4);  case 18 => REDUCE(26,17,4);  case 19 => REDUCE(26,17,4);  case 20 => REDUCE(26,17,4);  case 21 => REDUCE(26,17,4);  case _ => ERROR;  }
  case 53 => { case 11 => REDUCE(26,14,4);  case 14 => REDUCE(26,14,4);  case 17 => REDUCE(26,14,4);  case 18 => REDUCE(26,14,4);  case 19 => REDUCE(26,14,4);  case 20 => REDUCE(26,14,4);  case 21 => REDUCE(26,14,4);  case _ => ERROR;  }
  case 54 => { case 8 => REDUCE(30,25,1);  case 17 => REDUCE(30,25,1);  case 18 => REDUCE(30,25,1);  case 19 => REDUCE(30,25,1);  case 20 => REDUCE(30,25,1);  case 21 => REDUCE(30,25,1);  case _ => ERROR;  }
  case 55 => { case 8 => REDUCE(30,26,1);  case 17 => REDUCE(30,26,1);  case 18 => REDUCE(30,26,1);  case 19 => REDUCE(30,26,1);  case 20 => REDUCE(30,26,1);  case 21 => REDUCE(30,26,1);  case _ => ERROR;  }
  case 56 => { case 8 => REDUCE(30,27,1);  case 17 => REDUCE(30,27,1);  case 18 => REDUCE(30,27,1);  case 19 => REDUCE(30,27,1);  case 20 => REDUCE(30,27,1);  case 21 => REDUCE(30,27,1);  case _ => ERROR;  }
  case 57 => { case 7 => REDUCE(31,28,1);  case 8 => REDUCE(31,28,1);  case 16 => REDUCE(31,28,1);  case 17 => REDUCE(31,28,1);  case 18 => REDUCE(31,28,1);  case 19 => REDUCE(31,28,1);  case 20 => REDUCE(31,28,1);  case 21 => REDUCE(31,28,1);  case _ => ERROR;  }
  case 58 => { case 1 => SHIFT(86);  case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 59 => { case 7 => SHIFT(88);  case _ => ERROR;  }
  case 60 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(89);  case 7 => SHIFT(58);  case _ => ERROR;  }
  case 61 => { case 7 => SHIFT(92);  case _ => ERROR;  }
  case 62 => { case 7 => SHIFT(93);  case _ => ERROR;  }
  case 63 => { case 8 => REDUCE(26,6,1);  case 17 => REDUCE(26,6,1);  case 18 => REDUCE(26,6,1);  case 19 => REDUCE(26,6,1);  case 20 => REDUCE(26,6,1);  case 21 => REDUCE(26,6,1);  case _ => ERROR;  }
  case 64 => { case 8 => REDUCE(27,19,1);  case 17 => REDUCE(27,19,1);  case 18 => REDUCE(27,19,1);  case 19 => REDUCE(27,19,1);  case 20 => REDUCE(27,19,1);  case 21 => REDUCE(27,19,1);  case _ => ERROR;  }
  case 65 => { case 7 => SHIFT(94);  case 16 => SHIFT(95);  case 8 => REDUCE(27,18,1);  case 17 => REDUCE(27,18,1);  case 18 => REDUCE(27,18,1);  case 19 => REDUCE(27,18,1);  case 20 => REDUCE(27,18,1);  case 21 => REDUCE(27,18,1);  case _ => ERROR;  }
  case 66 => { case 11 => REDUCE(31,28,1);  case 14 => REDUCE(31,28,1);  case 17 => REDUCE(31,28,1);  case 18 => REDUCE(31,28,1);  case 19 => REDUCE(31,28,1);  case 20 => REDUCE(31,28,1);  case 21 => REDUCE(31,28,1);  case _ => ERROR;  }
  case 67 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 68 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 69 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 70 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 71 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 72 => { case 8 => REDUCE(30,25,1);  case 11 => REDUCE(30,25,1);  case 17 => REDUCE(30,25,1);  case 18 => REDUCE(30,25,1);  case 19 => REDUCE(30,25,1);  case 20 => REDUCE(30,25,1);  case 21 => REDUCE(30,25,1);  case _ => ERROR;  }
  case 73 => { case 8 => REDUCE(30,26,1);  case 11 => REDUCE(30,26,1);  case 17 => REDUCE(30,26,1);  case 18 => REDUCE(30,26,1);  case 19 => REDUCE(30,26,1);  case 20 => REDUCE(30,26,1);  case 21 => REDUCE(30,26,1);  case _ => ERROR;  }
  case 74 => { case 8 => REDUCE(30,27,1);  case 11 => REDUCE(30,27,1);  case 17 => REDUCE(30,27,1);  case 18 => REDUCE(30,27,1);  case 19 => REDUCE(30,27,1);  case 20 => REDUCE(30,27,1);  case 21 => REDUCE(30,27,1);  case _ => ERROR;  }
  case 75 => { case 7 => REDUCE(31,28,1);  case 8 => REDUCE(31,28,1);  case 11 => REDUCE(31,28,1);  case 16 => REDUCE(31,28,1);  case 17 => REDUCE(31,28,1);  case 18 => REDUCE(31,28,1);  case 19 => REDUCE(31,28,1);  case 20 => REDUCE(31,28,1);  case 21 => REDUCE(31,28,1);  case _ => ERROR;  }
  case 76 => { case 1 => SHIFT(101);  case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 77 => { case 7 => SHIFT(103);  case _ => ERROR;  }
  case 78 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(104);  case 7 => SHIFT(76);  case _ => ERROR;  }
  case 79 => { case 7 => SHIFT(107);  case _ => ERROR;  }
  case 80 => { case 7 => SHIFT(108);  case _ => ERROR;  }
  case 81 => { case 17 => SHIFT(109);  case 18 => SHIFT(110);  case 19 => SHIFT(111);  case 20 => SHIFT(112);  case 21 => SHIFT(113);  case 8 => REDUCE(29,23,1);  case 11 => REDUCE(29,23,1);  case _ => ERROR;  }
  case 82 => { case 8 => REDUCE(26,6,1);  case 11 => REDUCE(26,6,1);  case 17 => REDUCE(26,6,1);  case 18 => REDUCE(26,6,1);  case 19 => REDUCE(26,6,1);  case 20 => REDUCE(26,6,1);  case 21 => REDUCE(26,6,1);  case _ => ERROR;  }
  case 83 => { case 11 => SHIFT(114);  case 8 => REDUCE(28,22,1);  case _ => ERROR;  }
  case 84 => { case 8 => REDUCE(27,19,1);  case 11 => REDUCE(27,19,1);  case 17 => REDUCE(27,19,1);  case 18 => REDUCE(27,19,1);  case 19 => REDUCE(27,19,1);  case 20 => REDUCE(27,19,1);  case 21 => REDUCE(27,19,1);  case _ => ERROR;  }
  case 85 => { case 7 => SHIFT(115);  case 16 => SHIFT(116);  case 8 => REDUCE(27,18,1);  case 11 => REDUCE(27,18,1);  case 17 => REDUCE(27,18,1);  case 18 => REDUCE(27,18,1);  case 19 => REDUCE(27,18,1);  case 20 => REDUCE(27,18,1);  case 21 => REDUCE(27,18,1);  case _ => ERROR;  }
  case 86 => { case 8 => SHIFT(117);  case _ => ERROR;  }
  case 87 => { case 8 => SHIFT(118);  case 17 => SHIFT(67);  case 18 => SHIFT(68);  case 19 => SHIFT(69);  case 20 => SHIFT(70);  case 21 => SHIFT(71);  case _ => ERROR;  }
  case 88 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 89 => { case 8 => REDUCE(31,28,1);  case 17 => REDUCE(31,28,1);  case 18 => REDUCE(31,28,1);  case 19 => REDUCE(31,28,1);  case 20 => REDUCE(31,28,1);  case 21 => REDUCE(31,28,1);  case _ => ERROR;  }
  case 90 => { case 8 => REDUCE(26,7,2);  case 17 => REDUCE(26,7,2);  case 18 => REDUCE(26,7,2);  case 19 => REDUCE(26,7,2);  case 20 => REDUCE(26,7,2);  case 21 => REDUCE(26,7,2);  case _ => ERROR;  }
  case 91 => { case 8 => REDUCE(27,18,1);  case 17 => REDUCE(27,18,1);  case 18 => REDUCE(27,18,1);  case 19 => REDUCE(27,18,1);  case 20 => REDUCE(27,18,1);  case 21 => REDUCE(27,18,1);  case _ => ERROR;  }
  case 92 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 93 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 94 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 95 => { case 3 => SHIFT(54);  case 4 => SHIFT(55);  case 5 => SHIFT(56);  case 6 => SHIFT(57);  case 7 => SHIFT(58);  case 17 => SHIFT(59);  case 18 => SHIFT(60);  case 19 => SHIFT(61);  case 20 => SHIFT(62);  case _ => ERROR;  }
  case 96 => { case 19 => SHIFT(69);  case 20 => SHIFT(70);  case 21 => SHIFT(71);  case 8 => REDUCE(26,11,3);  case 17 => REDUCE(26,11,3);  case 18 => REDUCE(26,11,3);  case _ => ERROR;  }
  case 97 => { case 19 => SHIFT(69);  case 20 => SHIFT(70);  case 21 => SHIFT(71);  case 8 => REDUCE(26,13,3);  case 17 => REDUCE(26,13,3);  case 18 => REDUCE(26,13,3);  case _ => ERROR;  }
  case 98 => { case 21 => SHIFT(71);  case 8 => REDUCE(26,10,3);  case 17 => REDUCE(26,10,3);  case 18 => REDUCE(26,10,3);  case 19 => REDUCE(26,10,3);  case 20 => REDUCE(26,10,3);  case _ => ERROR;  }
  case 99 => { case 21 => SHIFT(71);  case 8 => REDUCE(26,12,3);  case 17 => REDUCE(26,12,3);  case 18 => REDUCE(26,12,3);  case 19 => REDUCE(26,12,3);  case 20 => REDUCE(26,12,3);  case _ => ERROR;  }
  case 100 => { case 21 => SHIFT(71);  case 8 => REDUCE(26,9,3);  case 17 => REDUCE(26,9,3);  case 18 => REDUCE(26,9,3);  case 19 => REDUCE(26,9,3);  case 20 => REDUCE(26,9,3);  case _ => ERROR;  }
  case 101 => { case 8 => SHIFT(124);  case _ => ERROR;  }
  case 102 => { case 8 => SHIFT(125);  case 17 => SHIFT(67);  case 18 => SHIFT(68);  case 19 => SHIFT(69);  case 20 => SHIFT(70);  case 21 => SHIFT(71);  case _ => ERROR;  }
  case 103 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 104 => { case 8 => REDUCE(31,28,1);  case 11 => REDUCE(31,28,1);  case 17 => REDUCE(31,28,1);  case 18 => REDUCE(31,28,1);  case 19 => REDUCE(31,28,1);  case 20 => REDUCE(31,28,1);  case 21 => REDUCE(31,28,1);  case _ => ERROR;  }
  case 105 => { case 8 => REDUCE(26,7,2);  case 11 => REDUCE(26,7,2);  case 17 => REDUCE(26,7,2);  case 18 => REDUCE(26,7,2);  case 19 => REDUCE(26,7,2);  case 20 => REDUCE(26,7,2);  case 21 => REDUCE(26,7,2);  case _ => ERROR;  }
  case 106 => { case 8 => REDUCE(27,18,1);  case 11 => REDUCE(27,18,1);  case 17 => REDUCE(27,18,1);  case 18 => REDUCE(27,18,1);  case 19 => REDUCE(27,18,1);  case 20 => REDUCE(27,18,1);  case 21 => REDUCE(27,18,1);  case _ => ERROR;  }
  case 107 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 108 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 109 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 110 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 111 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 112 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 113 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 114 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 115 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 116 => { case 3 => SHIFT(72);  case 4 => SHIFT(73);  case 5 => SHIFT(74);  case 6 => SHIFT(75);  case 7 => SHIFT(76);  case 17 => SHIFT(77);  case 18 => SHIFT(78);  case 19 => SHIFT(79);  case 20 => SHIFT(80);  case _ => ERROR;  }
  case 117 => { case 8 => REDUCE(27,21,3);  case 17 => REDUCE(27,21,3);  case 18 => REDUCE(27,21,3);  case 19 => REDUCE(27,21,3);  case 20 => REDUCE(27,21,3);  case 21 => REDUCE(27,21,3);  case _ => ERROR;  }
  case 118 => { case 8 => REDUCE(27,20,3);  case 17 => REDUCE(27,20,3);  case 18 => REDUCE(27,20,3);  case 19 => REDUCE(27,20,3);  case 20 => REDUCE(27,20,3);  case 21 => REDUCE(27,20,3);  case _ => ERROR;  }
  case 119 => { case 8 => SHIFT(137);  case _ => ERROR;  }
  case 120 => { case 8 => SHIFT(138);  case _ => ERROR;  }
  case 121 => { case 8 => SHIFT(139);  case _ => ERROR;  }
  case 122 => { case 8 => SHIFT(140);  case _ => ERROR;  }
  case 123 => { case 17 => SHIFT(67);  case 18 => SHIFT(68);  case 19 => SHIFT(69);  case 20 => SHIFT(70);  case 21 => SHIFT(71);  case 8 => REDUCE(26,8,3);  case _ => ERROR;  }
  case 124 => { case 8 => REDUCE(27,21,3);  case 11 => REDUCE(27,21,3);  case 17 => REDUCE(27,21,3);  case 18 => REDUCE(27,21,3);  case 19 => REDUCE(27,21,3);  case 20 => REDUCE(27,21,3);  case 21 => REDUCE(27,21,3);  case _ => ERROR;  }
  case 125 => { case 8 => REDUCE(27,20,3);  case 11 => REDUCE(27,20,3);  case 17 => REDUCE(27,20,3);  case 18 => REDUCE(27,20,3);  case 19 => REDUCE(27,20,3);  case 20 => REDUCE(27,20,3);  case 21 => REDUCE(27,20,3);  case _ => ERROR;  }
  case 126 => { case 8 => SHIFT(141);  case _ => ERROR;  }
  case 127 => { case 8 => SHIFT(142);  case _ => ERROR;  }
  case 128 => { case 8 => SHIFT(143);  case _ => ERROR;  }
  case 129 => { case 19 => SHIFT(111);  case 20 => SHIFT(112);  case 21 => SHIFT(113);  case 8 => REDUCE(26,11,3);  case 11 => REDUCE(26,11,3);  case 17 => REDUCE(26,11,3);  case 18 => REDUCE(26,11,3);  case _ => ERROR;  }
  case 130 => { case 19 => SHIFT(111);  case 20 => SHIFT(112);  case 21 => SHIFT(113);  case 8 => REDUCE(26,13,3);  case 11 => REDUCE(26,13,3);  case 17 => REDUCE(26,13,3);  case 18 => REDUCE(26,13,3);  case _ => ERROR;  }
  case 131 => { case 21 => SHIFT(113);  case 8 => REDUCE(26,10,3);  case 11 => REDUCE(26,10,3);  case 17 => REDUCE(26,10,3);  case 18 => REDUCE(26,10,3);  case 19 => REDUCE(26,10,3);  case 20 => REDUCE(26,10,3);  case _ => ERROR;  }
  case 132 => { case 21 => SHIFT(113);  case 8 => REDUCE(26,12,3);  case 11 => REDUCE(26,12,3);  case 17 => REDUCE(26,12,3);  case 18 => REDUCE(26,12,3);  case 19 => REDUCE(26,12,3);  case 20 => REDUCE(26,12,3);  case _ => ERROR;  }
  case 133 => { case 21 => SHIFT(113);  case 8 => REDUCE(26,9,3);  case 11 => REDUCE(26,9,3);  case 17 => REDUCE(26,9,3);  case 18 => REDUCE(26,9,3);  case 19 => REDUCE(26,9,3);  case 20 => REDUCE(26,9,3);  case _ => ERROR;  }
  case 134 => { case 17 => SHIFT(109);  case 18 => SHIFT(110);  case 19 => SHIFT(111);  case 20 => SHIFT(112);  case 21 => SHIFT(113);  case 8 => REDUCE(29,24,3);  case 11 => REDUCE(29,24,3);  case _ => ERROR;  }
  case 135 => { case 8 => SHIFT(144);  case _ => ERROR;  }
  case 136 => { case 17 => SHIFT(109);  case 18 => SHIFT(110);  case 19 => SHIFT(111);  case 20 => SHIFT(112);  case 21 => SHIFT(113);  case 8 => REDUCE(26,8,3);  case 11 => REDUCE(26,8,3);  case _ => ERROR;  }
  case 137 => { case 8 => REDUCE(26,16,4);  case 17 => REDUCE(26,16,4);  case 18 => REDUCE(26,16,4);  case 19 => REDUCE(26,16,4);  case 20 => REDUCE(26,16,4);  case 21 => REDUCE(26,16,4);  case _ => ERROR;  }
  case 138 => { case 8 => REDUCE(26,15,4);  case 17 => REDUCE(26,15,4);  case 18 => REDUCE(26,15,4);  case 19 => REDUCE(26,15,4);  case 20 => REDUCE(26,15,4);  case 21 => REDUCE(26,15,4);  case _ => ERROR;  }
  case 139 => { case 8 => REDUCE(26,17,4);  case 17 => REDUCE(26,17,4);  case 18 => REDUCE(26,17,4);  case 19 => REDUCE(26,17,4);  case 20 => REDUCE(26,17,4);  case 21 => REDUCE(26,17,4);  case _ => ERROR;  }
  case 140 => { case 8 => REDUCE(26,14,4);  case 17 => REDUCE(26,14,4);  case 18 => REDUCE(26,14,4);  case 19 => REDUCE(26,14,4);  case 20 => REDUCE(26,14,4);  case 21 => REDUCE(26,14,4);  case _ => ERROR;  }
  case 141 => { case 8 => REDUCE(26,16,4);  case 11 => REDUCE(26,16,4);  case 17 => REDUCE(26,16,4);  case 18 => REDUCE(26,16,4);  case 19 => REDUCE(26,16,4);  case 20 => REDUCE(26,16,4);  case 21 => REDUCE(26,16,4);  case _ => ERROR;  }
  case 142 => { case 8 => REDUCE(26,15,4);  case 11 => REDUCE(26,15,4);  case 17 => REDUCE(26,15,4);  case 18 => REDUCE(26,15,4);  case 19 => REDUCE(26,15,4);  case 20 => REDUCE(26,15,4);  case 21 => REDUCE(26,15,4);  case _ => ERROR;  }
  case 143 => { case 8 => REDUCE(26,17,4);  case 11 => REDUCE(26,17,4);  case 17 => REDUCE(26,17,4);  case 18 => REDUCE(26,17,4);  case 19 => REDUCE(26,17,4);  case 20 => REDUCE(26,17,4);  case 21 => REDUCE(26,17,4);  case _ => ERROR;  }
  case 144 => { case 8 => REDUCE(26,14,4);  case 11 => REDUCE(26,14,4);  case 17 => REDUCE(26,14,4);  case 18 => REDUCE(26,14,4);  case 19 => REDUCE(26,14,4);  case 20 => REDUCE(26,14,4);  case 21 => REDUCE(26,14,4);  case _ => ERROR;  }
  case _ => { case _ => ERROR }
  }
}
