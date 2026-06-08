
package scalalr.stage2
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 47 => 1;  case 48 => 2;  }
  case 4 => { case 51 => 20;  }
  case 7 => { case 51 => 23;  case 52 => 24;  case 53 => 25;  }
  case 8 => { case 51 => 23;  case 52 => 26;  case 53 => 25;  }
  case 9 => { case 51 => 23;  case 52 => 27;  case 53 => 25;  }
  case 10 => { case 51 => 23;  case 52 => 28;  case 53 => 25;  }
  case 11 => { case 49 => 30;  }
  case 13 => { case 51 => 23;  case 52 => 32;  case 53 => 25;  }
  case 14 => { case 51 => 33;  }
  case 15 => { case 51 => 34;  }
  case 16 => { case 51 => 35;  }
  case 17 => { case 51 => 36;  }
  case 25 => { case 51 => 23;  case 52 => 39;  case 53 => 25;  }
  case 30 => { case 54 => 42;  case 55 => 43;  case 56 => 44;  }
  case 37 => { case 64 => 47;  }
  case 38 => { case 64 => 48;  }
  case 42 => { case 50 => 53;  }
  case 44 => { case 71 => 55;  }
  case 46 => { case 64 => 58;  case 65 => 59;  case 75 => 60;  case 76 => 61;  }
  case 51 => { case 64 => 63;  }
  case 52 => { case 55 => 64;  case 56 => 44;  }
  case 55 => { case 57 => 69;  case 58 => 70;  case 59 => 71;  case 60 => 72;  case 61 => 73;  case 72 => 74;  case 73 => 75;  }
  case 56 => { case 64 => 148;  case 65 => 76;  case 75 => 149;  case 76 => 150;  }
  case 67 => { case 59 => 81;  case 60 => 155;  case 61 => 156;  case 72 => 157;  case 73 => 158;  }
  case 71 => { case 66 => 85;  }
  case 74 => { case 60 => 86;  case 61 => 73;  }
  case 78 => { case 64 => 88;  }
  case 80 => { case 61 => 90;  }
  case 82 => { case 57 => 92;  case 58 => 70;  case 59 => 71;  case 60 => 72;  case 61 => 73;  case 72 => 74;  case 73 => 75;  }
  case 84 => { case 67 => 98;  case 69 => 99;  case 70 => 100;  }
  case 85 => { case 63 => 102;  case 74 => 103;  }
  case 91 => { case 62 => 108;  }
  case 96 => { case 67 => 109;  case 68 => 110;  case 69 => 164;  case 70 => 165;  case 77 => 111;  case 78 => 112;  }
  case 114 => { case 67 => 125;  case 69 => 99;  case 70 => 100;  }
  case 115 => { case 67 => 126;  case 69 => 99;  case 70 => 100;  }
  case 116 => { case 67 => 127;  case 69 => 99;  case 70 => 100;  }
  case 117 => { case 67 => 109;  case 68 => 128;  case 69 => 164;  case 70 => 165;  case 77 => 111;  case 78 => 112;  }
  case 118 => { case 70 => 129;  }
  case 124 => { case 67 => 133;  case 69 => 164;  case 70 => 165;  }
  case 135 => { case 67 => 109;  case 68 => 136;  case 69 => 164;  case 70 => 165;  case 77 => 111;  case 78 => 112;  }
  case 141 => { case 64 => 58;  case 65 => 173;  case 75 => 60;  case 76 => 61;  }
  case 143 => { case 64 => 58;  case 65 => 176;  case 75 => 60;  case 76 => 61;  }
  case 145 => { case 64 => 58;  case 65 => 179;  case 75 => 60;  case 76 => 61;  }
  case 147 => { case 64 => 58;  case 65 => 182;  case 75 => 60;  case 76 => 61;  }
  case 153 => { case 59 => 185;  case 60 => 155;  case 61 => 156;  case 72 => 157;  case 73 => 158;  }
  case 157 => { case 60 => 186;  case 61 => 156;  }
  case 162 => { case 67 => 109;  case 68 => 187;  case 69 => 164;  case 70 => 165;  case 77 => 111;  case 78 => 112;  }
  case 166 => { case 67 => 191;  case 69 => 164;  case 70 => 165;  }
  case 167 => { case 67 => 192;  case 69 => 164;  case 70 => 165;  }
  case 168 => { case 67 => 193;  case 69 => 164;  case 70 => 165;  }
  case 171 => { case 64 => 148;  case 65 => 195;  case 75 => 149;  case 76 => 150;  }
  case 174 => { case 64 => 148;  case 65 => 197;  case 75 => 149;  case 76 => 150;  }
  case 177 => { case 64 => 148;  case 65 => 199;  case 75 => 149;  case 76 => 150;  }
  case 180 => { case 64 => 148;  case 65 => 201;  case 75 => 149;  case 76 => 150;  }
  case 183 => { case 64 => 203;  }
  case 184 => { case 61 => 205;  }
  case 189 => { case 67 => 109;  case 68 => 208;  case 69 => 164;  case 70 => 165;  case 77 => 111;  case 78 => 112;  }
  case 190 => { case 70 => 211;  }
  case 206 => { case 62 => 220;  }
  case 223 => { case 67 => 109;  case 68 => 227;  case 69 => 164;  case 70 => 165;  case 77 => 111;  case 78 => 112;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 20 => REDUCE(48,2,0);  case 23 => REDUCE(48,2,0);  case 24 => REDUCE(48,2,0);  case 25 => REDUCE(48,2,0);  case 26 => REDUCE(48,2,0);  case 27 => REDUCE(48,2,0);  case 28 => REDUCE(48,2,0);  case 29 => REDUCE(48,2,0);  case 30 => REDUCE(48,2,0);  case 31 => REDUCE(48,2,0);  case 32 => REDUCE(48,2,0);  case 33 => REDUCE(48,2,0);  case 34 => REDUCE(48,2,0);  case 35 => REDUCE(48,2,0);  case _ => ERROR;  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 20 => SHIFT(4);  case 23 => SHIFT(5);  case 24 => SHIFT(6);  case 25 => SHIFT(7);  case 26 => SHIFT(8);  case 27 => SHIFT(9);  case 28 => SHIFT(10);  case 29 => SHIFT(11);  case 30 => SHIFT(12);  case 31 => SHIFT(13);  case 32 => SHIFT(14);  case 33 => SHIFT(15);  case 34 => SHIFT(16);  case 35 => SHIFT(17);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(21);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(22);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(138);  case 6 => SHIFT(139);  case 20 => REDUCE(52,22,0);  case 23 => REDUCE(52,22,0);  case 24 => REDUCE(52,22,0);  case 25 => REDUCE(52,22,0);  case 26 => REDUCE(52,22,0);  case 27 => REDUCE(52,22,0);  case 28 => REDUCE(52,22,0);  case 29 => REDUCE(52,22,0);  case 30 => REDUCE(52,22,0);  case 31 => REDUCE(52,22,0);  case 32 => REDUCE(52,22,0);  case 33 => REDUCE(52,22,0);  case 34 => REDUCE(52,22,0);  case 35 => REDUCE(52,22,0);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(138);  case 6 => SHIFT(139);  case 20 => REDUCE(52,22,0);  case 23 => REDUCE(52,22,0);  case 24 => REDUCE(52,22,0);  case 25 => REDUCE(52,22,0);  case 26 => REDUCE(52,22,0);  case 27 => REDUCE(52,22,0);  case 28 => REDUCE(52,22,0);  case 29 => REDUCE(52,22,0);  case 30 => REDUCE(52,22,0);  case 31 => REDUCE(52,22,0);  case 32 => REDUCE(52,22,0);  case 33 => REDUCE(52,22,0);  case 34 => REDUCE(52,22,0);  case 35 => REDUCE(52,22,0);  case _ => ERROR;  }
  case 9 => { case 3 => SHIFT(138);  case 6 => SHIFT(139);  case 20 => REDUCE(52,22,0);  case 23 => REDUCE(52,22,0);  case 24 => REDUCE(52,22,0);  case 25 => REDUCE(52,22,0);  case 26 => REDUCE(52,22,0);  case 27 => REDUCE(52,22,0);  case 28 => REDUCE(52,22,0);  case 29 => REDUCE(52,22,0);  case 30 => REDUCE(52,22,0);  case 31 => REDUCE(52,22,0);  case 32 => REDUCE(52,22,0);  case 33 => REDUCE(52,22,0);  case 34 => REDUCE(52,22,0);  case 35 => REDUCE(52,22,0);  case _ => ERROR;  }
  case 10 => { case 3 => SHIFT(138);  case 6 => SHIFT(139);  case 20 => REDUCE(52,22,0);  case 23 => REDUCE(52,22,0);  case 24 => REDUCE(52,22,0);  case 25 => REDUCE(52,22,0);  case 26 => REDUCE(52,22,0);  case 27 => REDUCE(52,22,0);  case 28 => REDUCE(52,22,0);  case 29 => REDUCE(52,22,0);  case 30 => REDUCE(52,22,0);  case 31 => REDUCE(52,22,0);  case 32 => REDUCE(52,22,0);  case 33 => REDUCE(52,22,0);  case 34 => REDUCE(52,22,0);  case 35 => REDUCE(52,22,0);  case _ => ERROR;  }
  case 11 => { case 30 => SHIFT(29);  case 3 => REDUCE(49,17,0);  case _ => ERROR;  }
  case 12 => { case 5 => SHIFT(31);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(138);  case 6 => SHIFT(139);  case 20 => REDUCE(52,22,0);  case 23 => REDUCE(52,22,0);  case 24 => REDUCE(52,22,0);  case 25 => REDUCE(52,22,0);  case 26 => REDUCE(52,22,0);  case 27 => REDUCE(52,22,0);  case 28 => REDUCE(52,22,0);  case 29 => REDUCE(52,22,0);  case 30 => REDUCE(52,22,0);  case 31 => REDUCE(52,22,0);  case 32 => REDUCE(52,22,0);  case 33 => REDUCE(52,22,0);  case 34 => REDUCE(52,22,0);  case 35 => REDUCE(52,22,0);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(18);  case 6 => SHIFT(19);  case _ => ERROR;  }
  case 18 => { case 20 => REDUCE(51,20,1);  case 23 => REDUCE(51,20,1);  case 24 => REDUCE(51,20,1);  case 25 => REDUCE(51,20,1);  case 26 => REDUCE(51,20,1);  case 27 => REDUCE(51,20,1);  case 28 => REDUCE(51,20,1);  case 29 => REDUCE(51,20,1);  case 30 => REDUCE(51,20,1);  case 31 => REDUCE(51,20,1);  case 32 => REDUCE(51,20,1);  case 33 => REDUCE(51,20,1);  case 34 => REDUCE(51,20,1);  case 35 => REDUCE(51,20,1);  case _ => ERROR;  }
  case 19 => { case 20 => REDUCE(51,21,1);  case 23 => REDUCE(51,21,1);  case 24 => REDUCE(51,21,1);  case 25 => REDUCE(51,21,1);  case 26 => REDUCE(51,21,1);  case 27 => REDUCE(51,21,1);  case 28 => REDUCE(51,21,1);  case 29 => REDUCE(51,21,1);  case 30 => REDUCE(51,21,1);  case 31 => REDUCE(51,21,1);  case 32 => REDUCE(51,21,1);  case 33 => REDUCE(51,21,1);  case 34 => REDUCE(51,21,1);  case 35 => REDUCE(51,21,1);  case _ => ERROR;  }
  case 20 => { case 20 => REDUCE(48,5,3);  case 23 => REDUCE(48,5,3);  case 24 => REDUCE(48,5,3);  case 25 => REDUCE(48,5,3);  case 26 => REDUCE(48,5,3);  case 27 => REDUCE(48,5,3);  case 28 => REDUCE(48,5,3);  case 29 => REDUCE(48,5,3);  case 30 => REDUCE(48,5,3);  case 31 => REDUCE(48,5,3);  case 32 => REDUCE(48,5,3);  case 33 => REDUCE(48,5,3);  case 34 => REDUCE(48,5,3);  case 35 => REDUCE(48,5,3);  case _ => ERROR;  }
  case 21 => { case 20 => REDUCE(48,3,3);  case 23 => REDUCE(48,3,3);  case 24 => REDUCE(48,3,3);  case 25 => REDUCE(48,3,3);  case 26 => REDUCE(48,3,3);  case 27 => REDUCE(48,3,3);  case 28 => REDUCE(48,3,3);  case 29 => REDUCE(48,3,3);  case 30 => REDUCE(48,3,3);  case 31 => REDUCE(48,3,3);  case 32 => REDUCE(48,3,3);  case 33 => REDUCE(48,3,3);  case 34 => REDUCE(48,3,3);  case 35 => REDUCE(48,3,3);  case _ => ERROR;  }
  case 22 => { case 20 => REDUCE(48,4,3);  case 23 => REDUCE(48,4,3);  case 24 => REDUCE(48,4,3);  case 25 => REDUCE(48,4,3);  case 26 => REDUCE(48,4,3);  case 27 => REDUCE(48,4,3);  case 28 => REDUCE(48,4,3);  case 29 => REDUCE(48,4,3);  case 30 => REDUCE(48,4,3);  case 31 => REDUCE(48,4,3);  case 32 => REDUCE(48,4,3);  case 33 => REDUCE(48,4,3);  case 34 => REDUCE(48,4,3);  case 35 => REDUCE(48,4,3);  case _ => ERROR;  }
  case 23 => { case 14 => SHIFT(37);  case 17 => SHIFT(38);  case 3 => REDUCE(53,26,1);  case 6 => REDUCE(53,26,1);  case 20 => REDUCE(53,26,1);  case 23 => REDUCE(53,26,1);  case 24 => REDUCE(53,26,1);  case 25 => REDUCE(53,26,1);  case 26 => REDUCE(53,26,1);  case 27 => REDUCE(53,26,1);  case 28 => REDUCE(53,26,1);  case 29 => REDUCE(53,26,1);  case 30 => REDUCE(53,26,1);  case 31 => REDUCE(53,26,1);  case 32 => REDUCE(53,26,1);  case 33 => REDUCE(53,26,1);  case 34 => REDUCE(53,26,1);  case 35 => REDUCE(53,26,1);  case _ => ERROR;  }
  case 24 => { case 20 => REDUCE(48,8,3);  case 23 => REDUCE(48,8,3);  case 24 => REDUCE(48,8,3);  case 25 => REDUCE(48,8,3);  case 26 => REDUCE(48,8,3);  case 27 => REDUCE(48,8,3);  case 28 => REDUCE(48,8,3);  case 29 => REDUCE(48,8,3);  case 30 => REDUCE(48,8,3);  case 31 => REDUCE(48,8,3);  case 32 => REDUCE(48,8,3);  case 33 => REDUCE(48,8,3);  case 34 => REDUCE(48,8,3);  case 35 => REDUCE(48,8,3);  case _ => ERROR;  }
  case 25 => { case 3 => SHIFT(138);  case 6 => SHIFT(139);  case 20 => REDUCE(52,22,0);  case 23 => REDUCE(52,22,0);  case 24 => REDUCE(52,22,0);  case 25 => REDUCE(52,22,0);  case 26 => REDUCE(52,22,0);  case 27 => REDUCE(52,22,0);  case 28 => REDUCE(52,22,0);  case 29 => REDUCE(52,22,0);  case 30 => REDUCE(52,22,0);  case 31 => REDUCE(52,22,0);  case 32 => REDUCE(52,22,0);  case 33 => REDUCE(52,22,0);  case 34 => REDUCE(52,22,0);  case 35 => REDUCE(52,22,0);  case _ => ERROR;  }
  case 26 => { case 20 => REDUCE(48,9,3);  case 23 => REDUCE(48,9,3);  case 24 => REDUCE(48,9,3);  case 25 => REDUCE(48,9,3);  case 26 => REDUCE(48,9,3);  case 27 => REDUCE(48,9,3);  case 28 => REDUCE(48,9,3);  case 29 => REDUCE(48,9,3);  case 30 => REDUCE(48,9,3);  case 31 => REDUCE(48,9,3);  case 32 => REDUCE(48,9,3);  case 33 => REDUCE(48,9,3);  case 34 => REDUCE(48,9,3);  case 35 => REDUCE(48,9,3);  case _ => ERROR;  }
  case 27 => { case 20 => REDUCE(48,10,3);  case 23 => REDUCE(48,10,3);  case 24 => REDUCE(48,10,3);  case 25 => REDUCE(48,10,3);  case 26 => REDUCE(48,10,3);  case 27 => REDUCE(48,10,3);  case 28 => REDUCE(48,10,3);  case 29 => REDUCE(48,10,3);  case 30 => REDUCE(48,10,3);  case 31 => REDUCE(48,10,3);  case 32 => REDUCE(48,10,3);  case 33 => REDUCE(48,10,3);  case 34 => REDUCE(48,10,3);  case 35 => REDUCE(48,10,3);  case _ => ERROR;  }
  case 28 => { case 20 => REDUCE(48,11,3);  case 23 => REDUCE(48,11,3);  case 24 => REDUCE(48,11,3);  case 25 => REDUCE(48,11,3);  case 26 => REDUCE(48,11,3);  case 27 => REDUCE(48,11,3);  case 28 => REDUCE(48,11,3);  case 29 => REDUCE(48,11,3);  case 30 => REDUCE(48,11,3);  case 31 => REDUCE(48,11,3);  case 32 => REDUCE(48,11,3);  case 33 => REDUCE(48,11,3);  case 34 => REDUCE(48,11,3);  case 35 => REDUCE(48,11,3);  case _ => ERROR;  }
  case 29 => { case 5 => SHIFT(40);  case _ => ERROR;  }
  case 30 => { case 3 => SHIFT(41);  case _ => ERROR;  }
  case 31 => { case 20 => REDUCE(48,7,3);  case 23 => REDUCE(48,7,3);  case 24 => REDUCE(48,7,3);  case 25 => REDUCE(48,7,3);  case 26 => REDUCE(48,7,3);  case 27 => REDUCE(48,7,3);  case 28 => REDUCE(48,7,3);  case 29 => REDUCE(48,7,3);  case 30 => REDUCE(48,7,3);  case 31 => REDUCE(48,7,3);  case 32 => REDUCE(48,7,3);  case 33 => REDUCE(48,7,3);  case 34 => REDUCE(48,7,3);  case 35 => REDUCE(48,7,3);  case _ => ERROR;  }
  case 32 => { case 20 => REDUCE(48,12,3);  case 23 => REDUCE(48,12,3);  case 24 => REDUCE(48,12,3);  case 25 => REDUCE(48,12,3);  case 26 => REDUCE(48,12,3);  case 27 => REDUCE(48,12,3);  case 28 => REDUCE(48,12,3);  case 29 => REDUCE(48,12,3);  case 30 => REDUCE(48,12,3);  case 31 => REDUCE(48,12,3);  case 32 => REDUCE(48,12,3);  case 33 => REDUCE(48,12,3);  case 34 => REDUCE(48,12,3);  case 35 => REDUCE(48,12,3);  case _ => ERROR;  }
  case 33 => { case 20 => REDUCE(48,6,3);  case 23 => REDUCE(48,6,3);  case 24 => REDUCE(48,6,3);  case 25 => REDUCE(48,6,3);  case 26 => REDUCE(48,6,3);  case 27 => REDUCE(48,6,3);  case 28 => REDUCE(48,6,3);  case 29 => REDUCE(48,6,3);  case 30 => REDUCE(48,6,3);  case 31 => REDUCE(48,6,3);  case 32 => REDUCE(48,6,3);  case 33 => REDUCE(48,6,3);  case 34 => REDUCE(48,6,3);  case 35 => REDUCE(48,6,3);  case _ => ERROR;  }
  case 34 => { case 20 => REDUCE(48,13,3);  case 23 => REDUCE(48,13,3);  case 24 => REDUCE(48,13,3);  case 25 => REDUCE(48,13,3);  case 26 => REDUCE(48,13,3);  case 27 => REDUCE(48,13,3);  case 28 => REDUCE(48,13,3);  case 29 => REDUCE(48,13,3);  case 30 => REDUCE(48,13,3);  case 31 => REDUCE(48,13,3);  case 32 => REDUCE(48,13,3);  case 33 => REDUCE(48,13,3);  case 34 => REDUCE(48,13,3);  case 35 => REDUCE(48,13,3);  case _ => ERROR;  }
  case 35 => { case 20 => REDUCE(48,14,3);  case 23 => REDUCE(48,14,3);  case 24 => REDUCE(48,14,3);  case 25 => REDUCE(48,14,3);  case 26 => REDUCE(48,14,3);  case 27 => REDUCE(48,14,3);  case 28 => REDUCE(48,14,3);  case 29 => REDUCE(48,14,3);  case 30 => REDUCE(48,14,3);  case 31 => REDUCE(48,14,3);  case 32 => REDUCE(48,14,3);  case 33 => REDUCE(48,14,3);  case 34 => REDUCE(48,14,3);  case 35 => REDUCE(48,14,3);  case _ => ERROR;  }
  case 36 => { case 20 => REDUCE(48,15,3);  case 23 => REDUCE(48,15,3);  case 24 => REDUCE(48,15,3);  case 25 => REDUCE(48,15,3);  case 26 => REDUCE(48,15,3);  case 27 => REDUCE(48,15,3);  case 28 => REDUCE(48,15,3);  case 29 => REDUCE(48,15,3);  case 30 => REDUCE(48,15,3);  case 31 => REDUCE(48,15,3);  case 32 => REDUCE(48,15,3);  case 33 => REDUCE(48,15,3);  case 34 => REDUCE(48,15,3);  case 35 => REDUCE(48,15,3);  case _ => ERROR;  }
  case 37 => { case 3 => SHIFT(45);  case 17 => SHIFT(46);  case _ => ERROR;  }
  case 38 => { case 3 => SHIFT(140);  case 17 => SHIFT(141);  case _ => ERROR;  }
  case 39 => { case 20 => REDUCE(52,23,2);  case 23 => REDUCE(52,23,2);  case 24 => REDUCE(52,23,2);  case 25 => REDUCE(52,23,2);  case 26 => REDUCE(52,23,2);  case 27 => REDUCE(52,23,2);  case 28 => REDUCE(52,23,2);  case 29 => REDUCE(52,23,2);  case 30 => REDUCE(52,23,2);  case 31 => REDUCE(52,23,2);  case 32 => REDUCE(52,23,2);  case 33 => REDUCE(52,23,2);  case 34 => REDUCE(52,23,2);  case 35 => REDUCE(52,23,2);  case _ => ERROR;  }
  case 40 => { case 44 => SHIFT(49);  case _ => ERROR;  }
  case 41 => { case 12 => SHIFT(50);  case 14 => SHIFT(51);  case _ => ERROR;  }
  case 42 => { case 44 => SHIFT(52);  case 0 => REDUCE(50,18,0);  case _ => ERROR;  }
  case 43 => { case 0 => REDUCE(54,27,1);  case 44 => REDUCE(54,27,1);  case _ => ERROR;  }
  case 44 => { case 13 => SHIFT(54);  case 3 => REDUCE(71,71,0);  case 6 => REDUCE(71,71,0);  case 17 => REDUCE(71,71,0);  case 22 => REDUCE(71,71,0);  case _ => ERROR;  }
  case 45 => { case 9 => SHIFT(56);  case 3 => REDUCE(64,49,1);  case 6 => REDUCE(64,49,1);  case 20 => REDUCE(64,49,1);  case 23 => REDUCE(64,49,1);  case 24 => REDUCE(64,49,1);  case 25 => REDUCE(64,49,1);  case 26 => REDUCE(64,49,1);  case 27 => REDUCE(64,49,1);  case 28 => REDUCE(64,49,1);  case 29 => REDUCE(64,49,1);  case 30 => REDUCE(64,49,1);  case 31 => REDUCE(64,49,1);  case 32 => REDUCE(64,49,1);  case 33 => REDUCE(64,49,1);  case 34 => REDUCE(64,49,1);  case 35 => REDUCE(64,49,1);  case _ => ERROR;  }
  case 46 => { case 3 => SHIFT(142);  case 17 => SHIFT(143);  case 18 => SHIFT(57);  case _ => ERROR;  }
  case 47 => { case 3 => REDUCE(53,24,3);  case 6 => REDUCE(53,24,3);  case 20 => REDUCE(53,24,3);  case 23 => REDUCE(53,24,3);  case 24 => REDUCE(53,24,3);  case 25 => REDUCE(53,24,3);  case 26 => REDUCE(53,24,3);  case 27 => REDUCE(53,24,3);  case 28 => REDUCE(53,24,3);  case 29 => REDUCE(53,24,3);  case 30 => REDUCE(53,24,3);  case 31 => REDUCE(53,24,3);  case 32 => REDUCE(53,24,3);  case 33 => REDUCE(53,24,3);  case 34 => REDUCE(53,24,3);  case 35 => REDUCE(53,24,3);  case _ => ERROR;  }
  case 48 => { case 18 => SHIFT(62);  case _ => ERROR;  }
  case 49 => { case 3 => REDUCE(49,16,3);  case _ => ERROR;  }
  case 50 => { case 3 => REDUCE(56,31,2);  case 6 => REDUCE(56,31,2);  case 13 => REDUCE(56,31,2);  case 17 => REDUCE(56,31,2);  case 22 => REDUCE(56,31,2);  case _ => ERROR;  }
  case 51 => { case 3 => SHIFT(144);  case 17 => SHIFT(145);  case _ => ERROR;  }
  case 52 => { case 3 => SHIFT(41);  case 0 => REDUCE(50,19,1);  case _ => ERROR;  }
  case 53 => { case 0 => REDUCE(47,1,5);  case _ => ERROR;  }
  case 54 => { case 3 => REDUCE(71,72,1);  case 6 => REDUCE(71,72,1);  case 17 => REDUCE(71,72,1);  case 22 => REDUCE(71,72,1);  case _ => ERROR;  }
  case 55 => { case 3 => SHIFT(65);  case 6 => SHIFT(66);  case 17 => SHIFT(67);  case 22 => SHIFT(68);  case _ => ERROR;  }
  case 56 => { case 3 => SHIFT(146);  case 17 => SHIFT(147);  case _ => ERROR;  }
  case 57 => { case 3 => REDUCE(64,52,2);  case 6 => REDUCE(64,52,2);  case 20 => REDUCE(64,52,2);  case 23 => REDUCE(64,52,2);  case 24 => REDUCE(64,52,2);  case 25 => REDUCE(64,52,2);  case 26 => REDUCE(64,52,2);  case 27 => REDUCE(64,52,2);  case 28 => REDUCE(64,52,2);  case 29 => REDUCE(64,52,2);  case 30 => REDUCE(64,52,2);  case 31 => REDUCE(64,52,2);  case 32 => REDUCE(64,52,2);  case 33 => REDUCE(64,52,2);  case 34 => REDUCE(64,52,2);  case 35 => REDUCE(64,52,2);  case _ => ERROR;  }
  case 58 => { case 18 => REDUCE(75,78,1);  case 19 => REDUCE(75,78,1);  case _ => ERROR;  }
  case 59 => { case 18 => SHIFT(77);  case _ => ERROR;  }
  case 60 => { case 19 => SHIFT(78);  case 18 => REDUCE(76,80,1);  case _ => ERROR;  }
  case 61 => { case 18 => REDUCE(65,53,1);  case _ => ERROR;  }
  case 62 => { case 3 => REDUCE(53,25,4);  case 6 => REDUCE(53,25,4);  case 20 => REDUCE(53,25,4);  case 23 => REDUCE(53,25,4);  case 24 => REDUCE(53,25,4);  case 25 => REDUCE(53,25,4);  case 26 => REDUCE(53,25,4);  case 27 => REDUCE(53,25,4);  case 28 => REDUCE(53,25,4);  case 29 => REDUCE(53,25,4);  case 30 => REDUCE(53,25,4);  case 31 => REDUCE(53,25,4);  case 32 => REDUCE(53,25,4);  case 33 => REDUCE(53,25,4);  case 34 => REDUCE(53,25,4);  case 35 => REDUCE(53,25,4);  case _ => ERROR;  }
  case 63 => { case 12 => SHIFT(79);  case _ => ERROR;  }
  case 64 => { case 0 => REDUCE(54,28,3);  case 44 => REDUCE(54,28,3);  case _ => ERROR;  }
  case 65 => { case 14 => SHIFT(80);  case 0 => REDUCE(61,39,1);  case 3 => REDUCE(61,39,1);  case 5 => REDUCE(61,39,1);  case 6 => REDUCE(61,39,1);  case 13 => REDUCE(61,39,1);  case 17 => REDUCE(61,39,1);  case 31 => REDUCE(61,39,1);  case 39 => REDUCE(61,39,1);  case 44 => REDUCE(61,39,1);  case _ => ERROR;  }
  case 66 => { case 0 => REDUCE(61,40,1);  case 3 => REDUCE(61,40,1);  case 5 => REDUCE(61,40,1);  case 6 => REDUCE(61,40,1);  case 13 => REDUCE(61,40,1);  case 17 => REDUCE(61,40,1);  case 31 => REDUCE(61,40,1);  case 39 => REDUCE(61,40,1);  case 44 => REDUCE(61,40,1);  case _ => ERROR;  }
  case 67 => { case 3 => SHIFT(151);  case 6 => SHIFT(152);  case 17 => SHIFT(153);  case 22 => SHIFT(154);  case _ => ERROR;  }
  case 68 => { case 0 => REDUCE(59,35,1);  case 5 => REDUCE(59,35,1);  case 13 => REDUCE(59,35,1);  case 31 => REDUCE(59,35,1);  case 39 => REDUCE(59,35,1);  case 44 => REDUCE(59,35,1);  case _ => ERROR;  }
  case 69 => { case 0 => REDUCE(55,29,3);  case 44 => REDUCE(55,29,3);  case _ => ERROR;  }
  case 70 => { case 13 => SHIFT(82);  case 0 => REDUCE(57,32,1);  case 44 => REDUCE(57,32,1);  case _ => ERROR;  }
  case 71 => { case 5 => SHIFT(83);  case 39 => SHIFT(84);  case 0 => REDUCE(66,54,0);  case 13 => REDUCE(66,54,0);  case 31 => REDUCE(66,54,0);  case 44 => REDUCE(66,54,0);  case _ => ERROR;  }
  case 72 => { case 0 => REDUCE(72,73,1);  case 3 => REDUCE(72,73,1);  case 5 => REDUCE(72,73,1);  case 6 => REDUCE(72,73,1);  case 13 => REDUCE(72,73,1);  case 17 => REDUCE(72,73,1);  case 31 => REDUCE(72,73,1);  case 39 => REDUCE(72,73,1);  case 44 => REDUCE(72,73,1);  case _ => ERROR;  }
  case 73 => { case 0 => REDUCE(60,37,1);  case 3 => REDUCE(60,37,1);  case 5 => REDUCE(60,37,1);  case 6 => REDUCE(60,37,1);  case 13 => REDUCE(60,37,1);  case 17 => REDUCE(60,37,1);  case 31 => REDUCE(60,37,1);  case 39 => REDUCE(60,37,1);  case 44 => REDUCE(60,37,1);  case _ => ERROR;  }
  case 74 => { case 3 => SHIFT(65);  case 6 => SHIFT(66);  case 17 => SHIFT(67);  case 0 => REDUCE(73,75,1);  case 5 => REDUCE(73,75,1);  case 13 => REDUCE(73,75,1);  case 31 => REDUCE(73,75,1);  case 39 => REDUCE(73,75,1);  case 44 => REDUCE(73,75,1);  case _ => ERROR;  }
  case 75 => { case 0 => REDUCE(59,36,1);  case 5 => REDUCE(59,36,1);  case 13 => REDUCE(59,36,1);  case 31 => REDUCE(59,36,1);  case 39 => REDUCE(59,36,1);  case 44 => REDUCE(59,36,1);  case _ => ERROR;  }
  case 76 => { case 10 => SHIFT(87);  case _ => ERROR;  }
  case 77 => { case 3 => REDUCE(64,51,3);  case 6 => REDUCE(64,51,3);  case 20 => REDUCE(64,51,3);  case 23 => REDUCE(64,51,3);  case 24 => REDUCE(64,51,3);  case 25 => REDUCE(64,51,3);  case 26 => REDUCE(64,51,3);  case 27 => REDUCE(64,51,3);  case 28 => REDUCE(64,51,3);  case 29 => REDUCE(64,51,3);  case 30 => REDUCE(64,51,3);  case 31 => REDUCE(64,51,3);  case 32 => REDUCE(64,51,3);  case 33 => REDUCE(64,51,3);  case 34 => REDUCE(64,51,3);  case 35 => REDUCE(64,51,3);  case _ => ERROR;  }
  case 78 => { case 3 => SHIFT(142);  case 17 => SHIFT(143);  case _ => ERROR;  }
  case 79 => { case 3 => REDUCE(56,30,4);  case 6 => REDUCE(56,30,4);  case 13 => REDUCE(56,30,4);  case 17 => REDUCE(56,30,4);  case 22 => REDUCE(56,30,4);  case _ => ERROR;  }
  case 80 => { case 3 => SHIFT(89);  case 6 => SHIFT(66);  case 17 => SHIFT(67);  case _ => ERROR;  }
  case 81 => { case 18 => SHIFT(91);  case _ => ERROR;  }
  case 82 => { case 3 => SHIFT(65);  case 6 => SHIFT(66);  case 17 => SHIFT(67);  case 22 => SHIFT(68);  case _ => ERROR;  }
  case 83 => { case 0 => REDUCE(66,55,1);  case 13 => REDUCE(66,55,1);  case 31 => REDUCE(66,55,1);  case 44 => REDUCE(66,55,1);  case _ => ERROR;  }
  case 84 => { case 3 => SHIFT(93);  case 4 => SHIFT(94);  case 6 => SHIFT(95);  case 17 => SHIFT(96);  case 40 => SHIFT(97);  case _ => ERROR;  }
  case 85 => { case 31 => SHIFT(101);  case 0 => REDUCE(74,76,0);  case 13 => REDUCE(74,76,0);  case 44 => REDUCE(74,76,0);  case _ => ERROR;  }
  case 86 => { case 0 => REDUCE(72,74,2);  case 3 => REDUCE(72,74,2);  case 5 => REDUCE(72,74,2);  case 6 => REDUCE(72,74,2);  case 13 => REDUCE(72,74,2);  case 17 => REDUCE(72,74,2);  case 31 => REDUCE(72,74,2);  case 39 => REDUCE(72,74,2);  case 44 => REDUCE(72,74,2);  case _ => ERROR;  }
  case 87 => { case 3 => REDUCE(64,50,4);  case 6 => REDUCE(64,50,4);  case 20 => REDUCE(64,50,4);  case 23 => REDUCE(64,50,4);  case 24 => REDUCE(64,50,4);  case 25 => REDUCE(64,50,4);  case 26 => REDUCE(64,50,4);  case 27 => REDUCE(64,50,4);  case 28 => REDUCE(64,50,4);  case 29 => REDUCE(64,50,4);  case 30 => REDUCE(64,50,4);  case 31 => REDUCE(64,50,4);  case 32 => REDUCE(64,50,4);  case 33 => REDUCE(64,50,4);  case 34 => REDUCE(64,50,4);  case 35 => REDUCE(64,50,4);  case _ => ERROR;  }
  case 88 => { case 18 => REDUCE(75,79,3);  case 19 => REDUCE(75,79,3);  case _ => ERROR;  }
  case 89 => { case 0 => REDUCE(61,39,1);  case 3 => REDUCE(61,39,1);  case 5 => REDUCE(61,39,1);  case 6 => REDUCE(61,39,1);  case 13 => REDUCE(61,39,1);  case 17 => REDUCE(61,39,1);  case 31 => REDUCE(61,39,1);  case 39 => REDUCE(61,39,1);  case 44 => REDUCE(61,39,1);  case _ => ERROR;  }
  case 90 => { case 0 => REDUCE(60,38,3);  case 3 => REDUCE(60,38,3);  case 5 => REDUCE(60,38,3);  case 6 => REDUCE(60,38,3);  case 13 => REDUCE(60,38,3);  case 17 => REDUCE(60,38,3);  case 31 => REDUCE(60,38,3);  case 39 => REDUCE(60,38,3);  case 44 => REDUCE(60,38,3);  case _ => ERROR;  }
  case 91 => { case 36 => SHIFT(104);  case 37 => SHIFT(105);  case 38 => SHIFT(106);  case 43 => SHIFT(107);  case _ => ERROR;  }
  case 92 => { case 0 => REDUCE(57,33,3);  case 44 => REDUCE(57,33,3);  case _ => ERROR;  }
  case 93 => { case 0 => REDUCE(70,69,1);  case 13 => REDUCE(70,69,1);  case 17 => REDUCE(70,69,1);  case 31 => REDUCE(70,69,1);  case 38 => REDUCE(70,69,1);  case 41 => REDUCE(70,69,1);  case 42 => REDUCE(70,69,1);  case 43 => REDUCE(70,69,1);  case 44 => REDUCE(70,69,1);  case _ => ERROR;  }
  case 94 => { case 0 => REDUCE(69,66,1);  case 13 => REDUCE(69,66,1);  case 31 => REDUCE(69,66,1);  case 38 => REDUCE(69,66,1);  case 41 => REDUCE(69,66,1);  case 42 => REDUCE(69,66,1);  case 44 => REDUCE(69,66,1);  case _ => ERROR;  }
  case 95 => { case 0 => REDUCE(69,68,1);  case 13 => REDUCE(69,68,1);  case 31 => REDUCE(69,68,1);  case 38 => REDUCE(69,68,1);  case 41 => REDUCE(69,68,1);  case 42 => REDUCE(69,68,1);  case 44 => REDUCE(69,68,1);  case _ => ERROR;  }
  case 96 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case 18 => REDUCE(78,83,0);  case _ => ERROR;  }
  case 97 => { case 3 => SHIFT(113);  case _ => ERROR;  }
  case 98 => { case 38 => SHIFT(114);  case 41 => SHIFT(115);  case 42 => SHIFT(116);  case 0 => REDUCE(66,56,2);  case 13 => REDUCE(66,56,2);  case 31 => REDUCE(66,56,2);  case 44 => REDUCE(66,56,2);  case _ => ERROR;  }
  case 99 => { case 0 => REDUCE(67,57,1);  case 13 => REDUCE(67,57,1);  case 31 => REDUCE(67,57,1);  case 38 => REDUCE(67,57,1);  case 41 => REDUCE(67,57,1);  case 42 => REDUCE(67,57,1);  case 44 => REDUCE(67,57,1);  case _ => ERROR;  }
  case 100 => { case 17 => SHIFT(117);  case 43 => SHIFT(118);  case 0 => REDUCE(69,65,1);  case 13 => REDUCE(69,65,1);  case 31 => REDUCE(69,65,1);  case 38 => REDUCE(69,65,1);  case 41 => REDUCE(69,65,1);  case 42 => REDUCE(69,65,1);  case 44 => REDUCE(69,65,1);  case _ => ERROR;  }
  case 101 => { case 3 => SHIFT(119);  case _ => ERROR;  }
  case 102 => { case 0 => REDUCE(58,34,3);  case 13 => REDUCE(58,34,3);  case 44 => REDUCE(58,34,3);  case _ => ERROR;  }
  case 103 => { case 0 => REDUCE(63,48,1);  case 13 => REDUCE(63,48,1);  case 44 => REDUCE(63,48,1);  case _ => ERROR;  }
  case 104 => { case 43 => SHIFT(120);  case 0 => REDUCE(62,43,1);  case 3 => REDUCE(62,43,1);  case 5 => REDUCE(62,43,1);  case 6 => REDUCE(62,43,1);  case 13 => REDUCE(62,43,1);  case 17 => REDUCE(62,43,1);  case 31 => REDUCE(62,43,1);  case 39 => REDUCE(62,43,1);  case 44 => REDUCE(62,43,1);  case _ => ERROR;  }
  case 105 => { case 0 => REDUCE(62,42,1);  case 3 => REDUCE(62,42,1);  case 5 => REDUCE(62,42,1);  case 6 => REDUCE(62,42,1);  case 13 => REDUCE(62,42,1);  case 17 => REDUCE(62,42,1);  case 31 => REDUCE(62,42,1);  case 39 => REDUCE(62,42,1);  case 44 => REDUCE(62,42,1);  case _ => ERROR;  }
  case 106 => { case 43 => SHIFT(121);  case 0 => REDUCE(62,44,1);  case 3 => REDUCE(62,44,1);  case 5 => REDUCE(62,44,1);  case 6 => REDUCE(62,44,1);  case 13 => REDUCE(62,44,1);  case 17 => REDUCE(62,44,1);  case 31 => REDUCE(62,44,1);  case 39 => REDUCE(62,44,1);  case 44 => REDUCE(62,44,1);  case _ => ERROR;  }
  case 107 => { case 43 => SHIFT(122);  case _ => ERROR;  }
  case 108 => { case 0 => REDUCE(61,41,4);  case 3 => REDUCE(61,41,4);  case 5 => REDUCE(61,41,4);  case 6 => REDUCE(61,41,4);  case 13 => REDUCE(61,41,4);  case 17 => REDUCE(61,41,4);  case 31 => REDUCE(61,41,4);  case 39 => REDUCE(61,41,4);  case 44 => REDUCE(61,41,4);  case _ => ERROR;  }
  case 109 => { case 38 => SHIFT(166);  case 41 => SHIFT(167);  case 42 => SHIFT(168);  case 18 => REDUCE(77,81,1);  case 19 => REDUCE(77,81,1);  case _ => ERROR;  }
  case 110 => { case 18 => SHIFT(123);  case _ => ERROR;  }
  case 111 => { case 19 => SHIFT(124);  case 18 => REDUCE(78,84,1);  case _ => ERROR;  }
  case 112 => { case 18 => REDUCE(68,64,1);  case _ => ERROR;  }
  case 113 => { case 0 => REDUCE(70,70,2);  case 13 => REDUCE(70,70,2);  case 17 => REDUCE(70,70,2);  case 31 => REDUCE(70,70,2);  case 38 => REDUCE(70,70,2);  case 41 => REDUCE(70,70,2);  case 42 => REDUCE(70,70,2);  case 43 => REDUCE(70,70,2);  case 44 => REDUCE(70,70,2);  case _ => ERROR;  }
  case 114 => { case 3 => SHIFT(93);  case 4 => SHIFT(94);  case 6 => SHIFT(95);  case 17 => SHIFT(96);  case 40 => SHIFT(97);  case _ => ERROR;  }
  case 115 => { case 3 => SHIFT(93);  case 4 => SHIFT(94);  case 6 => SHIFT(95);  case 17 => SHIFT(96);  case 40 => SHIFT(97);  case _ => ERROR;  }
  case 116 => { case 3 => SHIFT(93);  case 4 => SHIFT(94);  case 6 => SHIFT(95);  case 17 => SHIFT(96);  case 40 => SHIFT(97);  case _ => ERROR;  }
  case 117 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case 18 => REDUCE(78,83,0);  case _ => ERROR;  }
  case 118 => { case 3 => SHIFT(169);  case 40 => SHIFT(170);  case _ => ERROR;  }
  case 119 => { case 0 => REDUCE(74,77,2);  case 13 => REDUCE(74,77,2);  case 44 => REDUCE(74,77,2);  case _ => ERROR;  }
  case 120 => { case 43 => SHIFT(130);  case _ => ERROR;  }
  case 121 => { case 43 => SHIFT(131);  case _ => ERROR;  }
  case 122 => { case 43 => SHIFT(132);  case _ => ERROR;  }
  case 123 => { case 0 => REDUCE(69,67,3);  case 13 => REDUCE(69,67,3);  case 31 => REDUCE(69,67,3);  case 38 => REDUCE(69,67,3);  case 41 => REDUCE(69,67,3);  case 42 => REDUCE(69,67,3);  case 44 => REDUCE(69,67,3);  case _ => ERROR;  }
  case 124 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case _ => ERROR;  }
  case 125 => { case 0 => REDUCE(67,62,3);  case 13 => REDUCE(67,62,3);  case 31 => REDUCE(67,62,3);  case 38 => REDUCE(67,62,3);  case 41 => REDUCE(67,62,3);  case 42 => REDUCE(67,62,3);  case 44 => REDUCE(67,62,3);  case _ => ERROR;  }
  case 126 => { case 38 => SHIFT(114);  case 41 => SHIFT(115);  case 42 => SHIFT(116);  case 0 => REDUCE(67,61,3);  case 13 => REDUCE(67,61,3);  case 31 => REDUCE(67,61,3);  case 44 => REDUCE(67,61,3);  case _ => ERROR;  }
  case 127 => { case 0 => REDUCE(67,63,3);  case 13 => REDUCE(67,63,3);  case 31 => REDUCE(67,63,3);  case 38 => REDUCE(67,63,3);  case 41 => REDUCE(67,63,3);  case 42 => REDUCE(67,63,3);  case 44 => REDUCE(67,63,3);  case _ => ERROR;  }
  case 128 => { case 18 => SHIFT(134);  case _ => ERROR;  }
  case 129 => { case 17 => SHIFT(135);  case 0 => REDUCE(67,60,3);  case 13 => REDUCE(67,60,3);  case 31 => REDUCE(67,60,3);  case 38 => REDUCE(67,60,3);  case 41 => REDUCE(67,60,3);  case 42 => REDUCE(67,60,3);  case 44 => REDUCE(67,60,3);  case _ => ERROR;  }
  case 130 => { case 0 => REDUCE(62,45,3);  case 3 => REDUCE(62,45,3);  case 5 => REDUCE(62,45,3);  case 6 => REDUCE(62,45,3);  case 13 => REDUCE(62,45,3);  case 17 => REDUCE(62,45,3);  case 31 => REDUCE(62,45,3);  case 39 => REDUCE(62,45,3);  case 44 => REDUCE(62,45,3);  case _ => ERROR;  }
  case 131 => { case 0 => REDUCE(62,46,3);  case 3 => REDUCE(62,46,3);  case 5 => REDUCE(62,46,3);  case 6 => REDUCE(62,46,3);  case 13 => REDUCE(62,46,3);  case 17 => REDUCE(62,46,3);  case 31 => REDUCE(62,46,3);  case 39 => REDUCE(62,46,3);  case 44 => REDUCE(62,46,3);  case _ => ERROR;  }
  case 132 => { case 0 => REDUCE(62,47,3);  case 3 => REDUCE(62,47,3);  case 5 => REDUCE(62,47,3);  case 6 => REDUCE(62,47,3);  case 13 => REDUCE(62,47,3);  case 17 => REDUCE(62,47,3);  case 31 => REDUCE(62,47,3);  case 39 => REDUCE(62,47,3);  case 44 => REDUCE(62,47,3);  case _ => ERROR;  }
  case 133 => { case 38 => SHIFT(166);  case 41 => SHIFT(167);  case 42 => SHIFT(168);  case 18 => REDUCE(77,82,3);  case 19 => REDUCE(77,82,3);  case _ => ERROR;  }
  case 134 => { case 0 => REDUCE(67,58,4);  case 13 => REDUCE(67,58,4);  case 31 => REDUCE(67,58,4);  case 38 => REDUCE(67,58,4);  case 41 => REDUCE(67,58,4);  case 42 => REDUCE(67,58,4);  case 44 => REDUCE(67,58,4);  case _ => ERROR;  }
  case 135 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case 18 => REDUCE(78,83,0);  case _ => ERROR;  }
  case 136 => { case 18 => SHIFT(137);  case _ => ERROR;  }
  case 137 => { case 0 => REDUCE(67,59,6);  case 13 => REDUCE(67,59,6);  case 31 => REDUCE(67,59,6);  case 38 => REDUCE(67,59,6);  case 41 => REDUCE(67,59,6);  case 42 => REDUCE(67,59,6);  case 44 => REDUCE(67,59,6);  case _ => ERROR;  }
  case 138 => { case 3 => REDUCE(51,20,1);  case 6 => REDUCE(51,20,1);  case 14 => REDUCE(51,20,1);  case 17 => REDUCE(51,20,1);  case 20 => REDUCE(51,20,1);  case 23 => REDUCE(51,20,1);  case 24 => REDUCE(51,20,1);  case 25 => REDUCE(51,20,1);  case 26 => REDUCE(51,20,1);  case 27 => REDUCE(51,20,1);  case 28 => REDUCE(51,20,1);  case 29 => REDUCE(51,20,1);  case 30 => REDUCE(51,20,1);  case 31 => REDUCE(51,20,1);  case 32 => REDUCE(51,20,1);  case 33 => REDUCE(51,20,1);  case 34 => REDUCE(51,20,1);  case 35 => REDUCE(51,20,1);  case _ => ERROR;  }
  case 139 => { case 3 => REDUCE(51,21,1);  case 6 => REDUCE(51,21,1);  case 14 => REDUCE(51,21,1);  case 17 => REDUCE(51,21,1);  case 20 => REDUCE(51,21,1);  case 23 => REDUCE(51,21,1);  case 24 => REDUCE(51,21,1);  case 25 => REDUCE(51,21,1);  case 26 => REDUCE(51,21,1);  case 27 => REDUCE(51,21,1);  case 28 => REDUCE(51,21,1);  case 29 => REDUCE(51,21,1);  case 30 => REDUCE(51,21,1);  case 31 => REDUCE(51,21,1);  case 32 => REDUCE(51,21,1);  case 33 => REDUCE(51,21,1);  case 34 => REDUCE(51,21,1);  case 35 => REDUCE(51,21,1);  case _ => ERROR;  }
  case 140 => { case 9 => SHIFT(171);  case 18 => REDUCE(64,49,1);  case _ => ERROR;  }
  case 141 => { case 3 => SHIFT(142);  case 17 => SHIFT(143);  case 18 => SHIFT(172);  case _ => ERROR;  }
  case 142 => { case 9 => SHIFT(174);  case 18 => REDUCE(64,49,1);  case 19 => REDUCE(64,49,1);  case _ => ERROR;  }
  case 143 => { case 3 => SHIFT(142);  case 17 => SHIFT(143);  case 18 => SHIFT(175);  case _ => ERROR;  }
  case 144 => { case 9 => SHIFT(177);  case 12 => REDUCE(64,49,1);  case _ => ERROR;  }
  case 145 => { case 3 => SHIFT(142);  case 17 => SHIFT(143);  case 18 => SHIFT(178);  case _ => ERROR;  }
  case 146 => { case 9 => SHIFT(180);  case 10 => REDUCE(64,49,1);  case 19 => REDUCE(64,49,1);  case _ => ERROR;  }
  case 147 => { case 3 => SHIFT(142);  case 17 => SHIFT(143);  case 18 => SHIFT(181);  case _ => ERROR;  }
  case 148 => { case 10 => REDUCE(75,78,1);  case 19 => REDUCE(75,78,1);  case _ => ERROR;  }
  case 149 => { case 19 => SHIFT(183);  case 10 => REDUCE(76,80,1);  case _ => ERROR;  }
  case 150 => { case 10 => REDUCE(65,53,1);  case _ => ERROR;  }
  case 151 => { case 14 => SHIFT(184);  case 3 => REDUCE(61,39,1);  case 6 => REDUCE(61,39,1);  case 17 => REDUCE(61,39,1);  case 18 => REDUCE(61,39,1);  case _ => ERROR;  }
  case 152 => { case 3 => REDUCE(61,40,1);  case 6 => REDUCE(61,40,1);  case 17 => REDUCE(61,40,1);  case 18 => REDUCE(61,40,1);  case _ => ERROR;  }
  case 153 => { case 3 => SHIFT(151);  case 6 => SHIFT(152);  case 17 => SHIFT(153);  case 22 => SHIFT(154);  case _ => ERROR;  }
  case 154 => { case 18 => REDUCE(59,35,1);  case _ => ERROR;  }
  case 155 => { case 3 => REDUCE(72,73,1);  case 6 => REDUCE(72,73,1);  case 17 => REDUCE(72,73,1);  case 18 => REDUCE(72,73,1);  case _ => ERROR;  }
  case 156 => { case 3 => REDUCE(60,37,1);  case 6 => REDUCE(60,37,1);  case 17 => REDUCE(60,37,1);  case 18 => REDUCE(60,37,1);  case _ => ERROR;  }
  case 157 => { case 3 => SHIFT(151);  case 6 => SHIFT(152);  case 17 => SHIFT(153);  case 18 => REDUCE(73,75,1);  case _ => ERROR;  }
  case 158 => { case 18 => REDUCE(59,36,1);  case _ => ERROR;  }
  case 159 => { case 17 => REDUCE(70,69,1);  case 18 => REDUCE(70,69,1);  case 19 => REDUCE(70,69,1);  case 38 => REDUCE(70,69,1);  case 41 => REDUCE(70,69,1);  case 42 => REDUCE(70,69,1);  case 43 => REDUCE(70,69,1);  case _ => ERROR;  }
  case 160 => { case 18 => REDUCE(69,66,1);  case 19 => REDUCE(69,66,1);  case 38 => REDUCE(69,66,1);  case 41 => REDUCE(69,66,1);  case 42 => REDUCE(69,66,1);  case _ => ERROR;  }
  case 161 => { case 18 => REDUCE(69,68,1);  case 19 => REDUCE(69,68,1);  case 38 => REDUCE(69,68,1);  case 41 => REDUCE(69,68,1);  case 42 => REDUCE(69,68,1);  case _ => ERROR;  }
  case 162 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case 18 => REDUCE(78,83,0);  case _ => ERROR;  }
  case 163 => { case 3 => SHIFT(188);  case _ => ERROR;  }
  case 164 => { case 18 => REDUCE(67,57,1);  case 19 => REDUCE(67,57,1);  case 38 => REDUCE(67,57,1);  case 41 => REDUCE(67,57,1);  case 42 => REDUCE(67,57,1);  case _ => ERROR;  }
  case 165 => { case 17 => SHIFT(189);  case 43 => SHIFT(190);  case 18 => REDUCE(69,65,1);  case 19 => REDUCE(69,65,1);  case 38 => REDUCE(69,65,1);  case 41 => REDUCE(69,65,1);  case 42 => REDUCE(69,65,1);  case _ => ERROR;  }
  case 166 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case _ => ERROR;  }
  case 167 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case _ => ERROR;  }
  case 168 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case _ => ERROR;  }
  case 169 => { case 0 => REDUCE(70,69,1);  case 13 => REDUCE(70,69,1);  case 17 => REDUCE(70,69,1);  case 31 => REDUCE(70,69,1);  case 38 => REDUCE(70,69,1);  case 41 => REDUCE(70,69,1);  case 42 => REDUCE(70,69,1);  case 44 => REDUCE(70,69,1);  case _ => ERROR;  }
  case 170 => { case 3 => SHIFT(194);  case _ => ERROR;  }
  case 171 => { case 3 => SHIFT(146);  case 17 => SHIFT(147);  case _ => ERROR;  }
  case 172 => { case 18 => REDUCE(64,52,2);  case _ => ERROR;  }
  case 173 => { case 18 => SHIFT(196);  case _ => ERROR;  }
  case 174 => { case 3 => SHIFT(146);  case 17 => SHIFT(147);  case _ => ERROR;  }
  case 175 => { case 18 => REDUCE(64,52,2);  case 19 => REDUCE(64,52,2);  case _ => ERROR;  }
  case 176 => { case 18 => SHIFT(198);  case _ => ERROR;  }
  case 177 => { case 3 => SHIFT(146);  case 17 => SHIFT(147);  case _ => ERROR;  }
  case 178 => { case 12 => REDUCE(64,52,2);  case _ => ERROR;  }
  case 179 => { case 18 => SHIFT(200);  case _ => ERROR;  }
  case 180 => { case 3 => SHIFT(146);  case 17 => SHIFT(147);  case _ => ERROR;  }
  case 181 => { case 10 => REDUCE(64,52,2);  case 19 => REDUCE(64,52,2);  case _ => ERROR;  }
  case 182 => { case 18 => SHIFT(202);  case _ => ERROR;  }
  case 183 => { case 3 => SHIFT(146);  case 17 => SHIFT(147);  case _ => ERROR;  }
  case 184 => { case 3 => SHIFT(204);  case 6 => SHIFT(152);  case 17 => SHIFT(153);  case _ => ERROR;  }
  case 185 => { case 18 => SHIFT(206);  case _ => ERROR;  }
  case 186 => { case 3 => REDUCE(72,74,2);  case 6 => REDUCE(72,74,2);  case 17 => REDUCE(72,74,2);  case 18 => REDUCE(72,74,2);  case _ => ERROR;  }
  case 187 => { case 18 => SHIFT(207);  case _ => ERROR;  }
  case 188 => { case 17 => REDUCE(70,70,2);  case 18 => REDUCE(70,70,2);  case 19 => REDUCE(70,70,2);  case 38 => REDUCE(70,70,2);  case 41 => REDUCE(70,70,2);  case 42 => REDUCE(70,70,2);  case 43 => REDUCE(70,70,2);  case _ => ERROR;  }
  case 189 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case 18 => REDUCE(78,83,0);  case _ => ERROR;  }
  case 190 => { case 3 => SHIFT(209);  case 40 => SHIFT(210);  case _ => ERROR;  }
  case 191 => { case 18 => REDUCE(67,62,3);  case 19 => REDUCE(67,62,3);  case 38 => REDUCE(67,62,3);  case 41 => REDUCE(67,62,3);  case 42 => REDUCE(67,62,3);  case _ => ERROR;  }
  case 192 => { case 38 => SHIFT(166);  case 41 => SHIFT(167);  case 42 => SHIFT(168);  case 18 => REDUCE(67,61,3);  case 19 => REDUCE(67,61,3);  case _ => ERROR;  }
  case 193 => { case 18 => REDUCE(67,63,3);  case 19 => REDUCE(67,63,3);  case 38 => REDUCE(67,63,3);  case 41 => REDUCE(67,63,3);  case 42 => REDUCE(67,63,3);  case _ => ERROR;  }
  case 194 => { case 0 => REDUCE(70,70,2);  case 13 => REDUCE(70,70,2);  case 17 => REDUCE(70,70,2);  case 31 => REDUCE(70,70,2);  case 38 => REDUCE(70,70,2);  case 41 => REDUCE(70,70,2);  case 42 => REDUCE(70,70,2);  case 44 => REDUCE(70,70,2);  case _ => ERROR;  }
  case 195 => { case 10 => SHIFT(212);  case _ => ERROR;  }
  case 196 => { case 18 => REDUCE(64,51,3);  case _ => ERROR;  }
  case 197 => { case 10 => SHIFT(213);  case _ => ERROR;  }
  case 198 => { case 18 => REDUCE(64,51,3);  case 19 => REDUCE(64,51,3);  case _ => ERROR;  }
  case 199 => { case 10 => SHIFT(214);  case _ => ERROR;  }
  case 200 => { case 12 => REDUCE(64,51,3);  case _ => ERROR;  }
  case 201 => { case 10 => SHIFT(215);  case _ => ERROR;  }
  case 202 => { case 10 => REDUCE(64,51,3);  case 19 => REDUCE(64,51,3);  case _ => ERROR;  }
  case 203 => { case 10 => REDUCE(75,79,3);  case 19 => REDUCE(75,79,3);  case _ => ERROR;  }
  case 204 => { case 3 => REDUCE(61,39,1);  case 6 => REDUCE(61,39,1);  case 17 => REDUCE(61,39,1);  case 18 => REDUCE(61,39,1);  case _ => ERROR;  }
  case 205 => { case 3 => REDUCE(60,38,3);  case 6 => REDUCE(60,38,3);  case 17 => REDUCE(60,38,3);  case 18 => REDUCE(60,38,3);  case _ => ERROR;  }
  case 206 => { case 36 => SHIFT(216);  case 37 => SHIFT(217);  case 38 => SHIFT(218);  case 43 => SHIFT(219);  case _ => ERROR;  }
  case 207 => { case 18 => REDUCE(69,67,3);  case 19 => REDUCE(69,67,3);  case 38 => REDUCE(69,67,3);  case 41 => REDUCE(69,67,3);  case 42 => REDUCE(69,67,3);  case _ => ERROR;  }
  case 208 => { case 18 => SHIFT(221);  case _ => ERROR;  }
  case 209 => { case 17 => REDUCE(70,69,1);  case 18 => REDUCE(70,69,1);  case 19 => REDUCE(70,69,1);  case 38 => REDUCE(70,69,1);  case 41 => REDUCE(70,69,1);  case 42 => REDUCE(70,69,1);  case _ => ERROR;  }
  case 210 => { case 3 => SHIFT(222);  case _ => ERROR;  }
  case 211 => { case 17 => SHIFT(223);  case 18 => REDUCE(67,60,3);  case 19 => REDUCE(67,60,3);  case 38 => REDUCE(67,60,3);  case 41 => REDUCE(67,60,3);  case 42 => REDUCE(67,60,3);  case _ => ERROR;  }
  case 212 => { case 18 => REDUCE(64,50,4);  case _ => ERROR;  }
  case 213 => { case 18 => REDUCE(64,50,4);  case 19 => REDUCE(64,50,4);  case _ => ERROR;  }
  case 214 => { case 12 => REDUCE(64,50,4);  case _ => ERROR;  }
  case 215 => { case 10 => REDUCE(64,50,4);  case 19 => REDUCE(64,50,4);  case _ => ERROR;  }
  case 216 => { case 43 => SHIFT(224);  case 3 => REDUCE(62,43,1);  case 6 => REDUCE(62,43,1);  case 17 => REDUCE(62,43,1);  case 18 => REDUCE(62,43,1);  case _ => ERROR;  }
  case 217 => { case 3 => REDUCE(62,42,1);  case 6 => REDUCE(62,42,1);  case 17 => REDUCE(62,42,1);  case 18 => REDUCE(62,42,1);  case _ => ERROR;  }
  case 218 => { case 43 => SHIFT(225);  case 3 => REDUCE(62,44,1);  case 6 => REDUCE(62,44,1);  case 17 => REDUCE(62,44,1);  case 18 => REDUCE(62,44,1);  case _ => ERROR;  }
  case 219 => { case 43 => SHIFT(226);  case _ => ERROR;  }
  case 220 => { case 3 => REDUCE(61,41,4);  case 6 => REDUCE(61,41,4);  case 17 => REDUCE(61,41,4);  case 18 => REDUCE(61,41,4);  case _ => ERROR;  }
  case 221 => { case 18 => REDUCE(67,58,4);  case 19 => REDUCE(67,58,4);  case 38 => REDUCE(67,58,4);  case 41 => REDUCE(67,58,4);  case 42 => REDUCE(67,58,4);  case _ => ERROR;  }
  case 222 => { case 17 => REDUCE(70,70,2);  case 18 => REDUCE(70,70,2);  case 19 => REDUCE(70,70,2);  case 38 => REDUCE(70,70,2);  case 41 => REDUCE(70,70,2);  case 42 => REDUCE(70,70,2);  case _ => ERROR;  }
  case 223 => { case 3 => SHIFT(159);  case 4 => SHIFT(160);  case 6 => SHIFT(161);  case 17 => SHIFT(162);  case 40 => SHIFT(163);  case 18 => REDUCE(78,83,0);  case _ => ERROR;  }
  case 224 => { case 43 => SHIFT(228);  case _ => ERROR;  }
  case 225 => { case 43 => SHIFT(229);  case _ => ERROR;  }
  case 226 => { case 43 => SHIFT(230);  case _ => ERROR;  }
  case 227 => { case 18 => SHIFT(231);  case _ => ERROR;  }
  case 228 => { case 3 => REDUCE(62,45,3);  case 6 => REDUCE(62,45,3);  case 17 => REDUCE(62,45,3);  case 18 => REDUCE(62,45,3);  case _ => ERROR;  }
  case 229 => { case 3 => REDUCE(62,46,3);  case 6 => REDUCE(62,46,3);  case 17 => REDUCE(62,46,3);  case 18 => REDUCE(62,46,3);  case _ => ERROR;  }
  case 230 => { case 3 => REDUCE(62,47,3);  case 6 => REDUCE(62,47,3);  case 17 => REDUCE(62,47,3);  case 18 => REDUCE(62,47,3);  case _ => ERROR;  }
  case 231 => { case 18 => REDUCE(67,59,6);  case 19 => REDUCE(67,59,6);  case 38 => REDUCE(67,59,6);  case 41 => REDUCE(67,59,6);  case 42 => REDUCE(67,59,6);  case _ => ERROR;  }
  case _ => { case _ => ERROR }
  }
}
