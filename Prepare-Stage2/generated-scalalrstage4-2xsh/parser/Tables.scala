
package scalalr.stage2
object Tables {
val goto: Int => Int => Int = {
  case 0 => { case 41 => 1;  case 42 => 2;  }
  case 7 => { case 45 => 22;  case 46 => 23;  }
  case 8 => { case 45 => 24;  case 46 => 23;  }
  case 9 => { case 45 => 25;  case 46 => 23;  }
  case 10 => { case 45 => 26;  case 46 => 23;  }
  case 11 => { case 43 => 28;  }
  case 13 => { case 45 => 30;  case 46 => 23;  }
  case 23 => { case 45 => 37;  case 46 => 23;  }
  case 28 => { case 47 => 40;  case 48 => 41;  case 49 => 42;  }
  case 35 => { case 58 => 45;  }
  case 36 => { case 58 => 46;  }
  case 40 => { case 44 => 51;  }
  case 42 => { case 60 => 53;  }
  case 44 => { case 58 => 56;  case 59 => 57;  case 64 => 58;  case 65 => 59;  }
  case 49 => { case 58 => 61;  }
  case 50 => { case 48 => 62;  case 49 => 42;  }
  case 53 => { case 50 => 66;  case 51 => 67;  case 52 => 68;  case 53 => 69;  case 54 => 70;  case 61 => 71;  case 62 => 72;  }
  case 54 => { case 58 => 105;  case 59 => 73;  case 64 => 106;  case 65 => 107;  }
  case 64 => { case 52 => 78;  case 53 => 111;  case 54 => 112;  case 61 => 113;  case 62 => 114;  }
  case 68 => { case 56 => 81;  }
  case 71 => { case 53 => 82;  case 54 => 70;  }
  case 75 => { case 58 => 84;  }
  case 77 => { case 54 => 86;  }
  case 79 => { case 50 => 88;  case 51 => 67;  case 52 => 68;  case 53 => 69;  case 54 => 70;  case 61 => 71;  case 62 => 72;  }
  case 81 => { case 57 => 90;  case 63 => 91;  }
  case 87 => { case 55 => 95;  }
  case 98 => { case 58 => 56;  case 59 => 117;  case 64 => 58;  case 65 => 59;  }
  case 100 => { case 58 => 56;  case 59 => 120;  case 64 => 58;  case 65 => 59;  }
  case 102 => { case 58 => 56;  case 59 => 123;  case 64 => 58;  case 65 => 59;  }
  case 104 => { case 58 => 56;  case 59 => 126;  case 64 => 58;  case 65 => 59;  }
  case 109 => { case 52 => 129;  case 53 => 111;  case 54 => 112;  case 61 => 113;  case 62 => 114;  }
  case 113 => { case 53 => 130;  case 54 => 112;  }
  case 115 => { case 58 => 105;  case 59 => 131;  case 64 => 106;  case 65 => 107;  }
  case 118 => { case 58 => 105;  case 59 => 133;  case 64 => 106;  case 65 => 107;  }
  case 121 => { case 58 => 105;  case 59 => 135;  case 64 => 106;  case 65 => 107;  }
  case 124 => { case 58 => 105;  case 59 => 137;  case 64 => 106;  case 65 => 107;  }
  case 127 => { case 58 => 139;  }
  case 128 => { case 54 => 141;  }
  case 142 => { case 55 => 150;  }
  case state => { case symbol => throw org.sufrin.scalalr.ErroneousGoto(state, symbol)}
  }

import org.sufrin.scalalr.Action._
val action: Int => Int => Action = {
  case 0 => { case 19 => REDUCE(42,2,0);  case 22 => REDUCE(42,2,0);  case 23 => REDUCE(42,2,0);  case 24 => REDUCE(42,2,0);  case 25 => REDUCE(42,2,0);  case 26 => REDUCE(42,2,0);  case 27 => REDUCE(42,2,0);  case 28 => REDUCE(42,2,0);  case 29 => REDUCE(42,2,0);  case 30 => REDUCE(42,2,0);  case 31 => REDUCE(42,2,0);  case 32 => REDUCE(42,2,0);  case 33 => REDUCE(42,2,0);  case 34 => REDUCE(42,2,0);  case _ => ERROR;  }
  case 1 => { case 0 => SHIFT(3);  case _ => ERROR;  }
  case 2 => { case 19 => SHIFT(4);  case 22 => SHIFT(5);  case 23 => SHIFT(6);  case 24 => SHIFT(7);  case 25 => SHIFT(8);  case 26 => SHIFT(9);  case 27 => SHIFT(10);  case 28 => SHIFT(11);  case 29 => SHIFT(12);  case 30 => SHIFT(13);  case 31 => SHIFT(14);  case 32 => SHIFT(15);  case 33 => SHIFT(16);  case 34 => SHIFT(17);  case _ => ERROR;  }
  case 3 => { case _ => ACCEPT;  }
  case 4 => { case 3 => SHIFT(18);  case _ => ERROR;  }
  case 5 => { case 3 => SHIFT(19);  case _ => ERROR;  }
  case 6 => { case 3 => SHIFT(20);  case _ => ERROR;  }
  case 7 => { case 3 => SHIFT(21);  case 19 => REDUCE(45,20,0);  case 22 => REDUCE(45,20,0);  case 23 => REDUCE(45,20,0);  case 24 => REDUCE(45,20,0);  case 25 => REDUCE(45,20,0);  case 26 => REDUCE(45,20,0);  case 27 => REDUCE(45,20,0);  case 28 => REDUCE(45,20,0);  case 29 => REDUCE(45,20,0);  case 30 => REDUCE(45,20,0);  case 31 => REDUCE(45,20,0);  case 32 => REDUCE(45,20,0);  case 33 => REDUCE(45,20,0);  case 34 => REDUCE(45,20,0);  case _ => ERROR;  }
  case 8 => { case 3 => SHIFT(21);  case 19 => REDUCE(45,20,0);  case 22 => REDUCE(45,20,0);  case 23 => REDUCE(45,20,0);  case 24 => REDUCE(45,20,0);  case 25 => REDUCE(45,20,0);  case 26 => REDUCE(45,20,0);  case 27 => REDUCE(45,20,0);  case 28 => REDUCE(45,20,0);  case 29 => REDUCE(45,20,0);  case 30 => REDUCE(45,20,0);  case 31 => REDUCE(45,20,0);  case 32 => REDUCE(45,20,0);  case 33 => REDUCE(45,20,0);  case 34 => REDUCE(45,20,0);  case _ => ERROR;  }
  case 9 => { case 3 => SHIFT(21);  case 19 => REDUCE(45,20,0);  case 22 => REDUCE(45,20,0);  case 23 => REDUCE(45,20,0);  case 24 => REDUCE(45,20,0);  case 25 => REDUCE(45,20,0);  case 26 => REDUCE(45,20,0);  case 27 => REDUCE(45,20,0);  case 28 => REDUCE(45,20,0);  case 29 => REDUCE(45,20,0);  case 30 => REDUCE(45,20,0);  case 31 => REDUCE(45,20,0);  case 32 => REDUCE(45,20,0);  case 33 => REDUCE(45,20,0);  case 34 => REDUCE(45,20,0);  case _ => ERROR;  }
  case 10 => { case 3 => SHIFT(21);  case 19 => REDUCE(45,20,0);  case 22 => REDUCE(45,20,0);  case 23 => REDUCE(45,20,0);  case 24 => REDUCE(45,20,0);  case 25 => REDUCE(45,20,0);  case 26 => REDUCE(45,20,0);  case 27 => REDUCE(45,20,0);  case 28 => REDUCE(45,20,0);  case 29 => REDUCE(45,20,0);  case 30 => REDUCE(45,20,0);  case 31 => REDUCE(45,20,0);  case 32 => REDUCE(45,20,0);  case 33 => REDUCE(45,20,0);  case 34 => REDUCE(45,20,0);  case _ => ERROR;  }
  case 11 => { case 29 => SHIFT(27);  case 3 => REDUCE(43,17,0);  case _ => ERROR;  }
  case 12 => { case 5 => SHIFT(29);  case _ => ERROR;  }
  case 13 => { case 3 => SHIFT(21);  case 19 => REDUCE(45,20,0);  case 22 => REDUCE(45,20,0);  case 23 => REDUCE(45,20,0);  case 24 => REDUCE(45,20,0);  case 25 => REDUCE(45,20,0);  case 26 => REDUCE(45,20,0);  case 27 => REDUCE(45,20,0);  case 28 => REDUCE(45,20,0);  case 29 => REDUCE(45,20,0);  case 30 => REDUCE(45,20,0);  case 31 => REDUCE(45,20,0);  case 32 => REDUCE(45,20,0);  case 33 => REDUCE(45,20,0);  case 34 => REDUCE(45,20,0);  case _ => ERROR;  }
  case 14 => { case 3 => SHIFT(31);  case _ => ERROR;  }
  case 15 => { case 3 => SHIFT(32);  case _ => ERROR;  }
  case 16 => { case 3 => SHIFT(33);  case _ => ERROR;  }
  case 17 => { case 3 => SHIFT(34);  case _ => ERROR;  }
  case 18 => { case 19 => REDUCE(42,5,3);  case 22 => REDUCE(42,5,3);  case 23 => REDUCE(42,5,3);  case 24 => REDUCE(42,5,3);  case 25 => REDUCE(42,5,3);  case 26 => REDUCE(42,5,3);  case 27 => REDUCE(42,5,3);  case 28 => REDUCE(42,5,3);  case 29 => REDUCE(42,5,3);  case 30 => REDUCE(42,5,3);  case 31 => REDUCE(42,5,3);  case 32 => REDUCE(42,5,3);  case 33 => REDUCE(42,5,3);  case 34 => REDUCE(42,5,3);  case _ => ERROR;  }
  case 19 => { case 19 => REDUCE(42,3,3);  case 22 => REDUCE(42,3,3);  case 23 => REDUCE(42,3,3);  case 24 => REDUCE(42,3,3);  case 25 => REDUCE(42,3,3);  case 26 => REDUCE(42,3,3);  case 27 => REDUCE(42,3,3);  case 28 => REDUCE(42,3,3);  case 29 => REDUCE(42,3,3);  case 30 => REDUCE(42,3,3);  case 31 => REDUCE(42,3,3);  case 32 => REDUCE(42,3,3);  case 33 => REDUCE(42,3,3);  case 34 => REDUCE(42,3,3);  case _ => ERROR;  }
  case 20 => { case 19 => REDUCE(42,4,3);  case 22 => REDUCE(42,4,3);  case 23 => REDUCE(42,4,3);  case 24 => REDUCE(42,4,3);  case 25 => REDUCE(42,4,3);  case 26 => REDUCE(42,4,3);  case 27 => REDUCE(42,4,3);  case 28 => REDUCE(42,4,3);  case 29 => REDUCE(42,4,3);  case 30 => REDUCE(42,4,3);  case 31 => REDUCE(42,4,3);  case 32 => REDUCE(42,4,3);  case 33 => REDUCE(42,4,3);  case 34 => REDUCE(42,4,3);  case _ => ERROR;  }
  case 21 => { case 13 => SHIFT(35);  case 16 => SHIFT(36);  case 3 => REDUCE(46,24,1);  case 19 => REDUCE(46,24,1);  case 22 => REDUCE(46,24,1);  case 23 => REDUCE(46,24,1);  case 24 => REDUCE(46,24,1);  case 25 => REDUCE(46,24,1);  case 26 => REDUCE(46,24,1);  case 27 => REDUCE(46,24,1);  case 28 => REDUCE(46,24,1);  case 29 => REDUCE(46,24,1);  case 30 => REDUCE(46,24,1);  case 31 => REDUCE(46,24,1);  case 32 => REDUCE(46,24,1);  case 33 => REDUCE(46,24,1);  case 34 => REDUCE(46,24,1);  case _ => ERROR;  }
  case 22 => { case 19 => REDUCE(42,8,3);  case 22 => REDUCE(42,8,3);  case 23 => REDUCE(42,8,3);  case 24 => REDUCE(42,8,3);  case 25 => REDUCE(42,8,3);  case 26 => REDUCE(42,8,3);  case 27 => REDUCE(42,8,3);  case 28 => REDUCE(42,8,3);  case 29 => REDUCE(42,8,3);  case 30 => REDUCE(42,8,3);  case 31 => REDUCE(42,8,3);  case 32 => REDUCE(42,8,3);  case 33 => REDUCE(42,8,3);  case 34 => REDUCE(42,8,3);  case _ => ERROR;  }
  case 23 => { case 3 => SHIFT(21);  case 19 => REDUCE(45,20,0);  case 22 => REDUCE(45,20,0);  case 23 => REDUCE(45,20,0);  case 24 => REDUCE(45,20,0);  case 25 => REDUCE(45,20,0);  case 26 => REDUCE(45,20,0);  case 27 => REDUCE(45,20,0);  case 28 => REDUCE(45,20,0);  case 29 => REDUCE(45,20,0);  case 30 => REDUCE(45,20,0);  case 31 => REDUCE(45,20,0);  case 32 => REDUCE(45,20,0);  case 33 => REDUCE(45,20,0);  case 34 => REDUCE(45,20,0);  case _ => ERROR;  }
  case 24 => { case 19 => REDUCE(42,9,3);  case 22 => REDUCE(42,9,3);  case 23 => REDUCE(42,9,3);  case 24 => REDUCE(42,9,3);  case 25 => REDUCE(42,9,3);  case 26 => REDUCE(42,9,3);  case 27 => REDUCE(42,9,3);  case 28 => REDUCE(42,9,3);  case 29 => REDUCE(42,9,3);  case 30 => REDUCE(42,9,3);  case 31 => REDUCE(42,9,3);  case 32 => REDUCE(42,9,3);  case 33 => REDUCE(42,9,3);  case 34 => REDUCE(42,9,3);  case _ => ERROR;  }
  case 25 => { case 19 => REDUCE(42,10,3);  case 22 => REDUCE(42,10,3);  case 23 => REDUCE(42,10,3);  case 24 => REDUCE(42,10,3);  case 25 => REDUCE(42,10,3);  case 26 => REDUCE(42,10,3);  case 27 => REDUCE(42,10,3);  case 28 => REDUCE(42,10,3);  case 29 => REDUCE(42,10,3);  case 30 => REDUCE(42,10,3);  case 31 => REDUCE(42,10,3);  case 32 => REDUCE(42,10,3);  case 33 => REDUCE(42,10,3);  case 34 => REDUCE(42,10,3);  case _ => ERROR;  }
  case 26 => { case 19 => REDUCE(42,11,3);  case 22 => REDUCE(42,11,3);  case 23 => REDUCE(42,11,3);  case 24 => REDUCE(42,11,3);  case 25 => REDUCE(42,11,3);  case 26 => REDUCE(42,11,3);  case 27 => REDUCE(42,11,3);  case 28 => REDUCE(42,11,3);  case 29 => REDUCE(42,11,3);  case 30 => REDUCE(42,11,3);  case 31 => REDUCE(42,11,3);  case 32 => REDUCE(42,11,3);  case 33 => REDUCE(42,11,3);  case 34 => REDUCE(42,11,3);  case _ => ERROR;  }
  case 27 => { case 5 => SHIFT(38);  case _ => ERROR;  }
  case 28 => { case 3 => SHIFT(39);  case _ => ERROR;  }
  case 29 => { case 19 => REDUCE(42,7,3);  case 22 => REDUCE(42,7,3);  case 23 => REDUCE(42,7,3);  case 24 => REDUCE(42,7,3);  case 25 => REDUCE(42,7,3);  case 26 => REDUCE(42,7,3);  case 27 => REDUCE(42,7,3);  case 28 => REDUCE(42,7,3);  case 29 => REDUCE(42,7,3);  case 30 => REDUCE(42,7,3);  case 31 => REDUCE(42,7,3);  case 32 => REDUCE(42,7,3);  case 33 => REDUCE(42,7,3);  case 34 => REDUCE(42,7,3);  case _ => ERROR;  }
  case 30 => { case 19 => REDUCE(42,12,3);  case 22 => REDUCE(42,12,3);  case 23 => REDUCE(42,12,3);  case 24 => REDUCE(42,12,3);  case 25 => REDUCE(42,12,3);  case 26 => REDUCE(42,12,3);  case 27 => REDUCE(42,12,3);  case 28 => REDUCE(42,12,3);  case 29 => REDUCE(42,12,3);  case 30 => REDUCE(42,12,3);  case 31 => REDUCE(42,12,3);  case 32 => REDUCE(42,12,3);  case 33 => REDUCE(42,12,3);  case 34 => REDUCE(42,12,3);  case _ => ERROR;  }
  case 31 => { case 19 => REDUCE(42,6,3);  case 22 => REDUCE(42,6,3);  case 23 => REDUCE(42,6,3);  case 24 => REDUCE(42,6,3);  case 25 => REDUCE(42,6,3);  case 26 => REDUCE(42,6,3);  case 27 => REDUCE(42,6,3);  case 28 => REDUCE(42,6,3);  case 29 => REDUCE(42,6,3);  case 30 => REDUCE(42,6,3);  case 31 => REDUCE(42,6,3);  case 32 => REDUCE(42,6,3);  case 33 => REDUCE(42,6,3);  case 34 => REDUCE(42,6,3);  case _ => ERROR;  }
  case 32 => { case 19 => REDUCE(42,13,3);  case 22 => REDUCE(42,13,3);  case 23 => REDUCE(42,13,3);  case 24 => REDUCE(42,13,3);  case 25 => REDUCE(42,13,3);  case 26 => REDUCE(42,13,3);  case 27 => REDUCE(42,13,3);  case 28 => REDUCE(42,13,3);  case 29 => REDUCE(42,13,3);  case 30 => REDUCE(42,13,3);  case 31 => REDUCE(42,13,3);  case 32 => REDUCE(42,13,3);  case 33 => REDUCE(42,13,3);  case 34 => REDUCE(42,13,3);  case _ => ERROR;  }
  case 33 => { case 19 => REDUCE(42,14,3);  case 22 => REDUCE(42,14,3);  case 23 => REDUCE(42,14,3);  case 24 => REDUCE(42,14,3);  case 25 => REDUCE(42,14,3);  case 26 => REDUCE(42,14,3);  case 27 => REDUCE(42,14,3);  case 28 => REDUCE(42,14,3);  case 29 => REDUCE(42,14,3);  case 30 => REDUCE(42,14,3);  case 31 => REDUCE(42,14,3);  case 32 => REDUCE(42,14,3);  case 33 => REDUCE(42,14,3);  case 34 => REDUCE(42,14,3);  case _ => ERROR;  }
  case 34 => { case 19 => REDUCE(42,15,3);  case 22 => REDUCE(42,15,3);  case 23 => REDUCE(42,15,3);  case 24 => REDUCE(42,15,3);  case 25 => REDUCE(42,15,3);  case 26 => REDUCE(42,15,3);  case 27 => REDUCE(42,15,3);  case 28 => REDUCE(42,15,3);  case 29 => REDUCE(42,15,3);  case 30 => REDUCE(42,15,3);  case 31 => REDUCE(42,15,3);  case 32 => REDUCE(42,15,3);  case 33 => REDUCE(42,15,3);  case 34 => REDUCE(42,15,3);  case _ => ERROR;  }
  case 35 => { case 3 => SHIFT(43);  case 16 => SHIFT(44);  case _ => ERROR;  }
  case 36 => { case 3 => SHIFT(97);  case 16 => SHIFT(98);  case _ => ERROR;  }
  case 37 => { case 19 => REDUCE(45,21,2);  case 22 => REDUCE(45,21,2);  case 23 => REDUCE(45,21,2);  case 24 => REDUCE(45,21,2);  case 25 => REDUCE(45,21,2);  case 26 => REDUCE(45,21,2);  case 27 => REDUCE(45,21,2);  case 28 => REDUCE(45,21,2);  case 29 => REDUCE(45,21,2);  case 30 => REDUCE(45,21,2);  case 31 => REDUCE(45,21,2);  case 32 => REDUCE(45,21,2);  case 33 => REDUCE(45,21,2);  case 34 => REDUCE(45,21,2);  case _ => ERROR;  }
  case 38 => { case 38 => SHIFT(47);  case _ => ERROR;  }
  case 39 => { case 11 => SHIFT(48);  case 13 => SHIFT(49);  case _ => ERROR;  }
  case 40 => { case 38 => SHIFT(50);  case 0 => REDUCE(44,18,0);  case _ => ERROR;  }
  case 41 => { case 0 => REDUCE(47,25,1);  case 38 => REDUCE(47,25,1);  case _ => ERROR;  }
  case 42 => { case 12 => SHIFT(52);  case 3 => REDUCE(60,50,0);  case 16 => REDUCE(60,50,0);  case 21 => REDUCE(60,50,0);  case _ => ERROR;  }
  case 43 => { case 8 => SHIFT(54);  case 3 => REDUCE(58,45,1);  case 19 => REDUCE(58,45,1);  case 22 => REDUCE(58,45,1);  case 23 => REDUCE(58,45,1);  case 24 => REDUCE(58,45,1);  case 25 => REDUCE(58,45,1);  case 26 => REDUCE(58,45,1);  case 27 => REDUCE(58,45,1);  case 28 => REDUCE(58,45,1);  case 29 => REDUCE(58,45,1);  case 30 => REDUCE(58,45,1);  case 31 => REDUCE(58,45,1);  case 32 => REDUCE(58,45,1);  case 33 => REDUCE(58,45,1);  case 34 => REDUCE(58,45,1);  case _ => ERROR;  }
  case 44 => { case 3 => SHIFT(99);  case 16 => SHIFT(100);  case 17 => SHIFT(55);  case _ => ERROR;  }
  case 45 => { case 3 => REDUCE(46,22,3);  case 19 => REDUCE(46,22,3);  case 22 => REDUCE(46,22,3);  case 23 => REDUCE(46,22,3);  case 24 => REDUCE(46,22,3);  case 25 => REDUCE(46,22,3);  case 26 => REDUCE(46,22,3);  case 27 => REDUCE(46,22,3);  case 28 => REDUCE(46,22,3);  case 29 => REDUCE(46,22,3);  case 30 => REDUCE(46,22,3);  case 31 => REDUCE(46,22,3);  case 32 => REDUCE(46,22,3);  case 33 => REDUCE(46,22,3);  case 34 => REDUCE(46,22,3);  case _ => ERROR;  }
  case 46 => { case 17 => SHIFT(60);  case _ => ERROR;  }
  case 47 => { case 3 => REDUCE(43,16,3);  case _ => ERROR;  }
  case 48 => { case 3 => REDUCE(49,29,2);  case 12 => REDUCE(49,29,2);  case 16 => REDUCE(49,29,2);  case 21 => REDUCE(49,29,2);  case _ => ERROR;  }
  case 49 => { case 3 => SHIFT(101);  case 16 => SHIFT(102);  case _ => ERROR;  }
  case 50 => { case 3 => SHIFT(39);  case 0 => REDUCE(44,19,1);  case _ => ERROR;  }
  case 51 => { case 0 => REDUCE(41,1,5);  case _ => ERROR;  }
  case 52 => { case 3 => REDUCE(60,51,1);  case 16 => REDUCE(60,51,1);  case 21 => REDUCE(60,51,1);  case _ => ERROR;  }
  case 53 => { case 3 => SHIFT(63);  case 16 => SHIFT(64);  case 21 => SHIFT(65);  case _ => ERROR;  }
  case 54 => { case 3 => SHIFT(103);  case 16 => SHIFT(104);  case _ => ERROR;  }
  case 55 => { case 3 => REDUCE(58,48,2);  case 19 => REDUCE(58,48,2);  case 22 => REDUCE(58,48,2);  case 23 => REDUCE(58,48,2);  case 24 => REDUCE(58,48,2);  case 25 => REDUCE(58,48,2);  case 26 => REDUCE(58,48,2);  case 27 => REDUCE(58,48,2);  case 28 => REDUCE(58,48,2);  case 29 => REDUCE(58,48,2);  case 30 => REDUCE(58,48,2);  case 31 => REDUCE(58,48,2);  case 32 => REDUCE(58,48,2);  case 33 => REDUCE(58,48,2);  case 34 => REDUCE(58,48,2);  case _ => ERROR;  }
  case 56 => { case 17 => REDUCE(64,57,1);  case 18 => REDUCE(64,57,1);  case _ => ERROR;  }
  case 57 => { case 17 => SHIFT(74);  case _ => ERROR;  }
  case 58 => { case 18 => SHIFT(75);  case 17 => REDUCE(65,59,1);  case _ => ERROR;  }
  case 59 => { case 17 => REDUCE(59,49,1);  case _ => ERROR;  }
  case 60 => { case 3 => REDUCE(46,23,4);  case 19 => REDUCE(46,23,4);  case 22 => REDUCE(46,23,4);  case 23 => REDUCE(46,23,4);  case 24 => REDUCE(46,23,4);  case 25 => REDUCE(46,23,4);  case 26 => REDUCE(46,23,4);  case 27 => REDUCE(46,23,4);  case 28 => REDUCE(46,23,4);  case 29 => REDUCE(46,23,4);  case 30 => REDUCE(46,23,4);  case 31 => REDUCE(46,23,4);  case 32 => REDUCE(46,23,4);  case 33 => REDUCE(46,23,4);  case 34 => REDUCE(46,23,4);  case _ => ERROR;  }
  case 61 => { case 11 => SHIFT(76);  case _ => ERROR;  }
  case 62 => { case 0 => REDUCE(47,26,3);  case 38 => REDUCE(47,26,3);  case _ => ERROR;  }
  case 63 => { case 13 => SHIFT(77);  case 0 => REDUCE(54,37,1);  case 3 => REDUCE(54,37,1);  case 5 => REDUCE(54,37,1);  case 12 => REDUCE(54,37,1);  case 16 => REDUCE(54,37,1);  case 30 => REDUCE(54,37,1);  case 38 => REDUCE(54,37,1);  case _ => ERROR;  }
  case 64 => { case 3 => SHIFT(108);  case 16 => SHIFT(109);  case 21 => SHIFT(110);  case _ => ERROR;  }
  case 65 => { case 0 => REDUCE(52,33,1);  case 5 => REDUCE(52,33,1);  case 12 => REDUCE(52,33,1);  case 30 => REDUCE(52,33,1);  case 38 => REDUCE(52,33,1);  case _ => ERROR;  }
  case 66 => { case 0 => REDUCE(48,27,3);  case 38 => REDUCE(48,27,3);  case _ => ERROR;  }
  case 67 => { case 12 => SHIFT(79);  case 0 => REDUCE(50,30,1);  case 38 => REDUCE(50,30,1);  case _ => ERROR;  }
  case 68 => { case 5 => SHIFT(80);  case 0 => REDUCE(56,42,0);  case 12 => REDUCE(56,42,0);  case 30 => REDUCE(56,42,0);  case 38 => REDUCE(56,42,0);  case _ => ERROR;  }
  case 69 => { case 0 => REDUCE(61,52,1);  case 3 => REDUCE(61,52,1);  case 5 => REDUCE(61,52,1);  case 12 => REDUCE(61,52,1);  case 16 => REDUCE(61,52,1);  case 30 => REDUCE(61,52,1);  case 38 => REDUCE(61,52,1);  case _ => ERROR;  }
  case 70 => { case 0 => REDUCE(53,35,1);  case 3 => REDUCE(53,35,1);  case 5 => REDUCE(53,35,1);  case 12 => REDUCE(53,35,1);  case 16 => REDUCE(53,35,1);  case 30 => REDUCE(53,35,1);  case 38 => REDUCE(53,35,1);  case _ => ERROR;  }
  case 71 => { case 3 => SHIFT(63);  case 16 => SHIFT(64);  case 0 => REDUCE(62,54,1);  case 5 => REDUCE(62,54,1);  case 12 => REDUCE(62,54,1);  case 30 => REDUCE(62,54,1);  case 38 => REDUCE(62,54,1);  case _ => ERROR;  }
  case 72 => { case 0 => REDUCE(52,34,1);  case 5 => REDUCE(52,34,1);  case 12 => REDUCE(52,34,1);  case 30 => REDUCE(52,34,1);  case 38 => REDUCE(52,34,1);  case _ => ERROR;  }
  case 73 => { case 9 => SHIFT(83);  case _ => ERROR;  }
  case 74 => { case 3 => REDUCE(58,47,3);  case 19 => REDUCE(58,47,3);  case 22 => REDUCE(58,47,3);  case 23 => REDUCE(58,47,3);  case 24 => REDUCE(58,47,3);  case 25 => REDUCE(58,47,3);  case 26 => REDUCE(58,47,3);  case 27 => REDUCE(58,47,3);  case 28 => REDUCE(58,47,3);  case 29 => REDUCE(58,47,3);  case 30 => REDUCE(58,47,3);  case 31 => REDUCE(58,47,3);  case 32 => REDUCE(58,47,3);  case 33 => REDUCE(58,47,3);  case 34 => REDUCE(58,47,3);  case _ => ERROR;  }
  case 75 => { case 3 => SHIFT(99);  case 16 => SHIFT(100);  case _ => ERROR;  }
  case 76 => { case 3 => REDUCE(49,28,4);  case 12 => REDUCE(49,28,4);  case 16 => REDUCE(49,28,4);  case 21 => REDUCE(49,28,4);  case _ => ERROR;  }
  case 77 => { case 3 => SHIFT(85);  case 16 => SHIFT(64);  case _ => ERROR;  }
  case 78 => { case 17 => SHIFT(87);  case _ => ERROR;  }
  case 79 => { case 3 => SHIFT(63);  case 16 => SHIFT(64);  case 21 => SHIFT(65);  case _ => ERROR;  }
  case 80 => { case 0 => REDUCE(56,43,1);  case 12 => REDUCE(56,43,1);  case 30 => REDUCE(56,43,1);  case 38 => REDUCE(56,43,1);  case _ => ERROR;  }
  case 81 => { case 30 => SHIFT(89);  case 0 => REDUCE(63,55,0);  case 12 => REDUCE(63,55,0);  case 38 => REDUCE(63,55,0);  case _ => ERROR;  }
  case 82 => { case 0 => REDUCE(61,53,2);  case 3 => REDUCE(61,53,2);  case 5 => REDUCE(61,53,2);  case 12 => REDUCE(61,53,2);  case 16 => REDUCE(61,53,2);  case 30 => REDUCE(61,53,2);  case 38 => REDUCE(61,53,2);  case _ => ERROR;  }
  case 83 => { case 3 => REDUCE(58,46,4);  case 19 => REDUCE(58,46,4);  case 22 => REDUCE(58,46,4);  case 23 => REDUCE(58,46,4);  case 24 => REDUCE(58,46,4);  case 25 => REDUCE(58,46,4);  case 26 => REDUCE(58,46,4);  case 27 => REDUCE(58,46,4);  case 28 => REDUCE(58,46,4);  case 29 => REDUCE(58,46,4);  case 30 => REDUCE(58,46,4);  case 31 => REDUCE(58,46,4);  case 32 => REDUCE(58,46,4);  case 33 => REDUCE(58,46,4);  case 34 => REDUCE(58,46,4);  case _ => ERROR;  }
  case 84 => { case 17 => REDUCE(64,58,3);  case 18 => REDUCE(64,58,3);  case _ => ERROR;  }
  case 85 => { case 0 => REDUCE(54,37,1);  case 3 => REDUCE(54,37,1);  case 5 => REDUCE(54,37,1);  case 12 => REDUCE(54,37,1);  case 16 => REDUCE(54,37,1);  case 30 => REDUCE(54,37,1);  case 38 => REDUCE(54,37,1);  case _ => ERROR;  }
  case 86 => { case 0 => REDUCE(53,36,3);  case 3 => REDUCE(53,36,3);  case 5 => REDUCE(53,36,3);  case 12 => REDUCE(53,36,3);  case 16 => REDUCE(53,36,3);  case 30 => REDUCE(53,36,3);  case 38 => REDUCE(53,36,3);  case _ => ERROR;  }
  case 87 => { case 35 => SHIFT(92);  case 36 => SHIFT(93);  case 37 => SHIFT(94);  case _ => ERROR;  }
  case 88 => { case 0 => REDUCE(50,31,3);  case 38 => REDUCE(50,31,3);  case _ => ERROR;  }
  case 89 => { case 3 => SHIFT(96);  case _ => ERROR;  }
  case 90 => { case 0 => REDUCE(51,32,3);  case 12 => REDUCE(51,32,3);  case 38 => REDUCE(51,32,3);  case _ => ERROR;  }
  case 91 => { case 0 => REDUCE(57,44,1);  case 12 => REDUCE(57,44,1);  case 38 => REDUCE(57,44,1);  case _ => ERROR;  }
  case 92 => { case 0 => REDUCE(55,40,1);  case 3 => REDUCE(55,40,1);  case 5 => REDUCE(55,40,1);  case 12 => REDUCE(55,40,1);  case 16 => REDUCE(55,40,1);  case 30 => REDUCE(55,40,1);  case 38 => REDUCE(55,40,1);  case _ => ERROR;  }
  case 93 => { case 0 => REDUCE(55,39,1);  case 3 => REDUCE(55,39,1);  case 5 => REDUCE(55,39,1);  case 12 => REDUCE(55,39,1);  case 16 => REDUCE(55,39,1);  case 30 => REDUCE(55,39,1);  case 38 => REDUCE(55,39,1);  case _ => ERROR;  }
  case 94 => { case 0 => REDUCE(55,41,1);  case 3 => REDUCE(55,41,1);  case 5 => REDUCE(55,41,1);  case 12 => REDUCE(55,41,1);  case 16 => REDUCE(55,41,1);  case 30 => REDUCE(55,41,1);  case 38 => REDUCE(55,41,1);  case _ => ERROR;  }
  case 95 => { case 0 => REDUCE(54,38,4);  case 3 => REDUCE(54,38,4);  case 5 => REDUCE(54,38,4);  case 12 => REDUCE(54,38,4);  case 16 => REDUCE(54,38,4);  case 30 => REDUCE(54,38,4);  case 38 => REDUCE(54,38,4);  case _ => ERROR;  }
  case 96 => { case 0 => REDUCE(63,56,2);  case 12 => REDUCE(63,56,2);  case 38 => REDUCE(63,56,2);  case _ => ERROR;  }
  case 97 => { case 8 => SHIFT(115);  case 17 => REDUCE(58,45,1);  case _ => ERROR;  }
  case 98 => { case 3 => SHIFT(99);  case 16 => SHIFT(100);  case 17 => SHIFT(116);  case _ => ERROR;  }
  case 99 => { case 8 => SHIFT(118);  case 17 => REDUCE(58,45,1);  case 18 => REDUCE(58,45,1);  case _ => ERROR;  }
  case 100 => { case 3 => SHIFT(99);  case 16 => SHIFT(100);  case 17 => SHIFT(119);  case _ => ERROR;  }
  case 101 => { case 8 => SHIFT(121);  case 11 => REDUCE(58,45,1);  case _ => ERROR;  }
  case 102 => { case 3 => SHIFT(99);  case 16 => SHIFT(100);  case 17 => SHIFT(122);  case _ => ERROR;  }
  case 103 => { case 8 => SHIFT(124);  case 9 => REDUCE(58,45,1);  case 18 => REDUCE(58,45,1);  case _ => ERROR;  }
  case 104 => { case 3 => SHIFT(99);  case 16 => SHIFT(100);  case 17 => SHIFT(125);  case _ => ERROR;  }
  case 105 => { case 9 => REDUCE(64,57,1);  case 18 => REDUCE(64,57,1);  case _ => ERROR;  }
  case 106 => { case 18 => SHIFT(127);  case 9 => REDUCE(65,59,1);  case _ => ERROR;  }
  case 107 => { case 9 => REDUCE(59,49,1);  case _ => ERROR;  }
  case 108 => { case 13 => SHIFT(128);  case 3 => REDUCE(54,37,1);  case 16 => REDUCE(54,37,1);  case 17 => REDUCE(54,37,1);  case _ => ERROR;  }
  case 109 => { case 3 => SHIFT(108);  case 16 => SHIFT(109);  case 21 => SHIFT(110);  case _ => ERROR;  }
  case 110 => { case 17 => REDUCE(52,33,1);  case _ => ERROR;  }
  case 111 => { case 3 => REDUCE(61,52,1);  case 16 => REDUCE(61,52,1);  case 17 => REDUCE(61,52,1);  case _ => ERROR;  }
  case 112 => { case 3 => REDUCE(53,35,1);  case 16 => REDUCE(53,35,1);  case 17 => REDUCE(53,35,1);  case _ => ERROR;  }
  case 113 => { case 3 => SHIFT(108);  case 16 => SHIFT(109);  case 17 => REDUCE(62,54,1);  case _ => ERROR;  }
  case 114 => { case 17 => REDUCE(52,34,1);  case _ => ERROR;  }
  case 115 => { case 3 => SHIFT(103);  case 16 => SHIFT(104);  case _ => ERROR;  }
  case 116 => { case 17 => REDUCE(58,48,2);  case _ => ERROR;  }
  case 117 => { case 17 => SHIFT(132);  case _ => ERROR;  }
  case 118 => { case 3 => SHIFT(103);  case 16 => SHIFT(104);  case _ => ERROR;  }
  case 119 => { case 17 => REDUCE(58,48,2);  case 18 => REDUCE(58,48,2);  case _ => ERROR;  }
  case 120 => { case 17 => SHIFT(134);  case _ => ERROR;  }
  case 121 => { case 3 => SHIFT(103);  case 16 => SHIFT(104);  case _ => ERROR;  }
  case 122 => { case 11 => REDUCE(58,48,2);  case _ => ERROR;  }
  case 123 => { case 17 => SHIFT(136);  case _ => ERROR;  }
  case 124 => { case 3 => SHIFT(103);  case 16 => SHIFT(104);  case _ => ERROR;  }
  case 125 => { case 9 => REDUCE(58,48,2);  case 18 => REDUCE(58,48,2);  case _ => ERROR;  }
  case 126 => { case 17 => SHIFT(138);  case _ => ERROR;  }
  case 127 => { case 3 => SHIFT(103);  case 16 => SHIFT(104);  case _ => ERROR;  }
  case 128 => { case 3 => SHIFT(140);  case 16 => SHIFT(109);  case _ => ERROR;  }
  case 129 => { case 17 => SHIFT(142);  case _ => ERROR;  }
  case 130 => { case 3 => REDUCE(61,53,2);  case 16 => REDUCE(61,53,2);  case 17 => REDUCE(61,53,2);  case _ => ERROR;  }
  case 131 => { case 9 => SHIFT(143);  case _ => ERROR;  }
  case 132 => { case 17 => REDUCE(58,47,3);  case _ => ERROR;  }
  case 133 => { case 9 => SHIFT(144);  case _ => ERROR;  }
  case 134 => { case 17 => REDUCE(58,47,3);  case 18 => REDUCE(58,47,3);  case _ => ERROR;  }
  case 135 => { case 9 => SHIFT(145);  case _ => ERROR;  }
  case 136 => { case 11 => REDUCE(58,47,3);  case _ => ERROR;  }
  case 137 => { case 9 => SHIFT(146);  case _ => ERROR;  }
  case 138 => { case 9 => REDUCE(58,47,3);  case 18 => REDUCE(58,47,3);  case _ => ERROR;  }
  case 139 => { case 9 => REDUCE(64,58,3);  case 18 => REDUCE(64,58,3);  case _ => ERROR;  }
  case 140 => { case 3 => REDUCE(54,37,1);  case 16 => REDUCE(54,37,1);  case 17 => REDUCE(54,37,1);  case _ => ERROR;  }
  case 141 => { case 3 => REDUCE(53,36,3);  case 16 => REDUCE(53,36,3);  case 17 => REDUCE(53,36,3);  case _ => ERROR;  }
  case 142 => { case 35 => SHIFT(147);  case 36 => SHIFT(148);  case 37 => SHIFT(149);  case _ => ERROR;  }
  case 143 => { case 17 => REDUCE(58,46,4);  case _ => ERROR;  }
  case 144 => { case 17 => REDUCE(58,46,4);  case 18 => REDUCE(58,46,4);  case _ => ERROR;  }
  case 145 => { case 11 => REDUCE(58,46,4);  case _ => ERROR;  }
  case 146 => { case 9 => REDUCE(58,46,4);  case 18 => REDUCE(58,46,4);  case _ => ERROR;  }
  case 147 => { case 3 => REDUCE(55,40,1);  case 16 => REDUCE(55,40,1);  case 17 => REDUCE(55,40,1);  case _ => ERROR;  }
  case 148 => { case 3 => REDUCE(55,39,1);  case 16 => REDUCE(55,39,1);  case 17 => REDUCE(55,39,1);  case _ => ERROR;  }
  case 149 => { case 3 => REDUCE(55,41,1);  case 16 => REDUCE(55,41,1);  case 17 => REDUCE(55,41,1);  case _ => ERROR;  }
  case 150 => { case 3 => REDUCE(54,38,4);  case 16 => REDUCE(54,38,4);  case 17 => REDUCE(54,38,4);  case _ => ERROR;  }
  case _ => { case _ => ERROR }
  }
}
