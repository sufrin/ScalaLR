
package scalalr.stage2
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 46 => 1;  case 47 => 2;  }
  case 4 => { case 50 => 20;  }
  case 7 => { case 50 => 23;  case 51 => 24;  case 52 => 25;  }
  case 8 => { case 50 => 23;  case 51 => 26;  case 52 => 25;  }
  case 9 => { case 50 => 23;  case 51 => 27;  case 52 => 25;  }
  case 10 => { case 50 => 23;  case 51 => 28;  case 52 => 25;  }
  case 11 => { case 48 => 30;  }
  case 13 => { case 50 => 23;  case 51 => 32;  case 52 => 25;  }
  case 14 => { case 50 => 33;  }
  case 15 => { case 50 => 34;  }
  case 16 => { case 50 => 35;  }
  case 17 => { case 50 => 36;  }
  case 25 => { case 50 => 23;  case 51 => 39;  case 52 => 25;  }
  case 30 => { case 53 => 42;  case 54 => 43;  case 56 => 44;  }
  case 37 => { case 64 => 47;  }
  case 38 => { case 64 => 48;  }
  case 42 => { case 49 => 52;  }
  case 46 => { case 64 => 56;  case 65 => 57;  }
  case 50 => { case 64 => 59;  }
  case 51 => { case 54 => 60;  case 56 => 44;  }
  case 53 => { case 55 => 62;  }
  case 54 => { case 64 => 56;  case 65 => 63;  }
  case 62 => { case 57 => 70;  case 58 => 71;  case 59 => 72;  case 60 => 73;  case 61 => 74;  }
  case 64 => { case 64 => 56;  case 65 => 76;  }
  case 68 => { case 59 => 78;  case 60 => 73;  case 61 => 74;  }
  case 72 => { case 66 => 82;  }
  case 73 => { case 59 => 83;  case 60 => 73;  case 61 => 74;  }
  case 77 => { case 61 => 85;  }
  case 79 => { case 57 => 87;  case 58 => 71;  case 59 => 72;  case 60 => 73;  case 61 => 74;  }
  case 81 => { case 67 => 93;  case 69 => 94;  case 70 => 95;  }
  case 82 => { case 63 => 97;  }
  case 86 => { case 62 => 102;  }
  case 92 => { case 67 => 104;  case 68 => 105;  case 69 => 94;  case 70 => 95;  case 71 => 106;  case 72 => 107;  }
  case 108 => { case 67 => 119;  case 69 => 94;  case 70 => 95;  }
  case 109 => { case 67 => 120;  case 69 => 94;  case 70 => 95;  }
  case 110 => { case 67 => 121;  case 69 => 94;  case 70 => 95;  }
  case 111 => { case 67 => 104;  case 68 => 122;  case 69 => 94;  case 70 => 95;  case 71 => 106;  case 72 => 107;  }
  case 112 => { case 70 => 123;  }
  case 118 => { case 67 => 127;  case 69 => 94;  case 70 => 95;  }
  case 129 => { case 67 => 104;  case 68 => 130;  case 69 => 94;  case 70 => 95;  case 71 => 106;  case 72 => 107;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case _ => REDUCE(47,2,0);  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 24 => SHIFT(4);  case 27 => SHIFT(5);  case 28 => SHIFT(6);  case 29 => SHIFT(7);  case 30 => SHIFT(8);  case 31 => SHIFT(9);  case 32 => SHIFT(10);  case 33 => SHIFT(11);  case 34 => SHIFT(12);  case 35 => SHIFT(13);  case 36 => SHIFT(14);  case 37 => SHIFT(15);  case 38 => SHIFT(16);  case 39 => SHIFT(17);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(21);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(22);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => REDUCE(51,22,0);  }
  case 8 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => REDUCE(51,22,0);  }
  case 9 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => REDUCE(51,22,0);  }
  case 10 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => REDUCE(51,22,0);  }
  case 11 => { case 34 => SHIFT(29);  case _ => REDUCE(48,17,0);  }
  case 12 => { case 5 => SHIFT(31);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => REDUCE(51,22,0);  }
  case 14 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 18 => { case _ => REDUCE(50,20,1);  }
  case 19 => { case _ => REDUCE(50,21,1);  }
  case 20 => { case _ => REDUCE(47,5,3);  }
  case 21 => { case _ => REDUCE(47,3,3);  }
  case 22 => { case _ => REDUCE(47,4,3);  }
  case 23 => { case 14 => SHIFT(37);  case 17 => SHIFT(38);  case _ => REDUCE(52,26,1);  }
  case 24 => { case _ => REDUCE(47,8,3);  }
  case 25 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => REDUCE(51,22,0);  }
  case 26 => { case _ => REDUCE(47,9,3);  }
  case 27 => { case _ => REDUCE(47,10,3);  }
  case 28 => { case _ => REDUCE(47,11,3);  }
  case 29 => { case 5 => SHIFT(40);  case _ => ERROR;  }
  case 30 => { case 3 => SHIFT(41);  case _ => ERROR;  }
  case 31 => { case _ => REDUCE(47,7,3);  }
  case 32 => { case _ => REDUCE(47,12,3);  }
  case 33 => { case _ => REDUCE(47,6,3);  }
  case 34 => { case _ => REDUCE(47,13,3);  }
  case 35 => { case _ => REDUCE(47,14,3);  }
  case 36 => { case _ => REDUCE(47,15,3);  }
  case 37 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case _ => ERROR;  }
  case 38 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case _ => ERROR;  }
  case 39 => { case _ => REDUCE(51,23,2);  }
  case 40 => { case 22 => SHIFT(49);  case _ => ERROR;  }
  case 41 => { case 14 => SHIFT(50);  case _ => REDUCE(56,33,1);  }
  case 42 => { case 22 => SHIFT(51);  case _ => REDUCE(49,18,0);  }
  case 43 => { case _ => REDUCE(53,27,1);  }
  case 44 => { case 12 => SHIFT(53);  case _ => ERROR;  }
  case 45 => { case 9 => SHIFT(54);  case _ => REDUCE(64,53,1);  }
  case 46 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case 18 => SHIFT(55);  case _ => ERROR;  }
  case 47 => { case _ => REDUCE(52,24,3);  }
  case 48 => { case 18 => SHIFT(58);  case _ => ERROR;  }
  case 49 => { case _ => REDUCE(48,16,3);  }
  case 50 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case _ => ERROR;  }
  case 51 => { case 3 => SHIFT(41);  case _ => REDUCE(49,19,1);  }
  case 52 => { case _ => REDUCE(46,1,5);  }
  case 53 => { case 13 => SHIFT(61);  case _ => REDUCE(55,31,0);  }
  case 54 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case _ => ERROR;  }
  case 55 => { case _ => REDUCE(64,56,2);  }
  case 56 => { case 19 => SHIFT(64);  case _ => REDUCE(65,57,1);  }
  case 57 => { case 18 => SHIFT(65);  case _ => ERROR;  }
  case 58 => { case _ => REDUCE(52,25,4);  }
  case 59 => { case _ => REDUCE(56,32,3);  }
  case 60 => { case _ => REDUCE(53,28,3);  }
  case 61 => { case _ => REDUCE(55,30,1);  }
  case 62 => { case 3 => SHIFT(66);  case 6 => SHIFT(67);  case 17 => SHIFT(68);  case 26 => SHIFT(69);  case _ => ERROR;  }
  case 63 => { case 10 => SHIFT(75);  case _ => ERROR;  }
  case 64 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case _ => ERROR;  }
  case 65 => { case _ => REDUCE(64,55,3);  }
  case 66 => { case 14 => SHIFT(77);  case _ => REDUCE(61,42,1);  }
  case 67 => { case _ => REDUCE(61,43,1);  }
  case 68 => { case 3 => SHIFT(66);  case 6 => SHIFT(67);  case 17 => SHIFT(68);  case 26 => SHIFT(69);  case _ => ERROR;  }
  case 69 => { case _ => REDUCE(59,37,1);  }
  case 70 => { case _ => REDUCE(54,29,4);  }
  case 71 => { case 13 => SHIFT(79);  case _ => REDUCE(57,34,1);  }
  case 72 => { case 5 => SHIFT(80);  case 23 => SHIFT(81);  case _ => REDUCE(66,59,0);  }
  case 73 => { case 3 => SHIFT(66);  case 6 => SHIFT(67);  case 17 => SHIFT(68);  case 26 => SHIFT(69);  case _ => REDUCE(59,38,1);  }
  case 74 => { case _ => REDUCE(60,40,1);  }
  case 75 => { case _ => REDUCE(64,54,4);  }
  case 76 => { case _ => REDUCE(65,58,3);  }
  case 77 => { case 3 => SHIFT(84);  case 6 => SHIFT(67);  case 17 => SHIFT(68);  case _ => ERROR;  }
  case 78 => { case 18 => SHIFT(86);  case _ => ERROR;  }
  case 79 => { case 3 => SHIFT(66);  case 6 => SHIFT(67);  case 17 => SHIFT(68);  case 26 => SHIFT(69);  case _ => ERROR;  }
  case 80 => { case _ => REDUCE(66,60,1);  }
  case 81 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => ERROR;  }
  case 82 => { case 35 => SHIFT(96);  case _ => REDUCE(63,51,0);  }
  case 83 => { case _ => REDUCE(59,39,2);  }
  case 84 => { case _ => REDUCE(61,42,1);  }
  case 85 => { case _ => REDUCE(60,41,3);  }
  case 86 => { case 20 => SHIFT(98);  case 21 => SHIFT(99);  case 41 => SHIFT(100);  case 43 => SHIFT(101);  case _ => ERROR;  }
  case 87 => { case _ => REDUCE(57,35,3);  }
  case 88 => { case 3 => SHIFT(103);  case _ => ERROR;  }
  case 89 => { case _ => REDUCE(70,74,1);  }
  case 90 => { case _ => REDUCE(69,71,1);  }
  case 91 => { case _ => REDUCE(69,72,1);  }
  case 92 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => REDUCE(72,78,0);  }
  case 93 => { case 40 => SHIFT(108);  case 41 => SHIFT(109);  case 42 => SHIFT(110);  case _ => REDUCE(66,61,2);  }
  case 94 => { case _ => REDUCE(67,62,1);  }
  case 95 => { case 17 => SHIFT(111);  case 43 => SHIFT(112);  case _ => REDUCE(69,70,1);  }
  case 96 => { case 3 => SHIFT(113);  case _ => ERROR;  }
  case 97 => { case _ => REDUCE(58,36,3);  }
  case 98 => { case 43 => SHIFT(114);  case _ => REDUCE(62,46,1);  }
  case 99 => { case _ => REDUCE(62,45,1);  }
  case 100 => { case 43 => SHIFT(115);  case _ => REDUCE(62,47,1);  }
  case 101 => { case 43 => SHIFT(116);  case _ => ERROR;  }
  case 102 => { case _ => REDUCE(61,44,4);  }
  case 103 => { case _ => REDUCE(70,75,2);  }
  case 104 => { case 40 => SHIFT(108);  case 41 => SHIFT(109);  case 42 => SHIFT(110);  case _ => REDUCE(71,76,1);  }
  case 105 => { case 18 => SHIFT(117);  case _ => ERROR;  }
  case 106 => { case 19 => SHIFT(118);  case _ => REDUCE(72,79,1);  }
  case 107 => { case _ => REDUCE(68,69,1);  }
  case 108 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => ERROR;  }
  case 109 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => ERROR;  }
  case 110 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => ERROR;  }
  case 111 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => REDUCE(72,78,0);  }
  case 112 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case _ => ERROR;  }
  case 113 => { case _ => REDUCE(63,52,2);  }
  case 114 => { case 43 => SHIFT(124);  case _ => ERROR;  }
  case 115 => { case 43 => SHIFT(125);  case _ => ERROR;  }
  case 116 => { case 43 => SHIFT(126);  case _ => ERROR;  }
  case 117 => { case _ => REDUCE(69,73,3);  }
  case 118 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => ERROR;  }
  case 119 => { case 40 => SHIFT(108);  case 41 => SHIFT(109);  case 42 => SHIFT(110);  case _ => REDUCE(67,66,3);  }
  case 120 => { case _ => REDUCE(67,67,3);  }
  case 121 => { case _ => REDUCE(67,68,3);  }
  case 122 => { case 18 => SHIFT(128);  case _ => ERROR;  }
  case 123 => { case 17 => SHIFT(129);  case _ => REDUCE(67,65,3);  }
  case 124 => { case _ => REDUCE(62,48,3);  }
  case 125 => { case _ => REDUCE(62,49,3);  }
  case 126 => { case _ => REDUCE(62,50,3);  }
  case 127 => { case 40 => SHIFT(108);  case 41 => SHIFT(109);  case 42 => SHIFT(110);  case _ => REDUCE(71,77,3);  }
  case 128 => { case _ => REDUCE(67,63,4);  }
  case 129 => { case 44 => SHIFT(88);  case 3 => SHIFT(89);  case 4 => SHIFT(90);  case 6 => SHIFT(91);  case 17 => SHIFT(92);  case _ => REDUCE(72,78,0);  }
  case 130 => { case 18 => SHIFT(131);  case _ => ERROR;  }
  case 131 => { case _ => REDUCE(67,64,6);  }
  case _ => { case _ => ERROR }
  }
}
