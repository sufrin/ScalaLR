
package scalalr.parser.ScalaLR
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 34 => 3;  case 35 => 4;  }
  case 2 => { case 34 => 7;  case 35 => 4;  }
  case 5 => { case 34 => 10;  case 35 => 4;  }
  case 9 => { case 34 => 12;  case 35 => 4;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case 16 => SHIFT(2);  case _ => ERROR;  }
  case 1 => { case 8 => SHIFT(5);  case _ => REDUCE(35,3,1);  }
  case 2 => { case 3 => SHIFT(1);  case 16 => SHIFT(2);  case 17 => SHIFT(6);  case _ => ERROR;  }
  case 3 => { case 0 => SHIFT(8);  case _ => ERROR;  }
  case 4 => { case 18 => SHIFT(9);  case _ => REDUCE(34,1,1);  }
  case 5 => { case 3 => SHIFT(1);  case 16 => SHIFT(2);  case _ => ERROR;  }
  case 6 => { case _ => REDUCE(35,6,2);  }
  case 7 => { case 17 => SHIFT(11);  case _ => ERROR;  }
  case 8 => { case _ => ACCEPT;  }
  case 9 => { case 3 => SHIFT(1);  case 16 => SHIFT(2);  case _ => ERROR;  }
  case 10 => { case 9 => SHIFT(13);  case _ => ERROR;  }
  case 11 => { case _ => REDUCE(35,5,3);  }
  case 12 => { case _ => REDUCE(34,2,3);  }
  case 13 => { case _ => REDUCE(35,4,4);  }
  case _ => { case _ => ERROR }
  }
}
