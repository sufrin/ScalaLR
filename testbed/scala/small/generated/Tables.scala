
package testbed.scala.small.generated.Small
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 7 => 2;  case 8 => 3;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 0 => REDUCE(8,2,1);  case 4 => REDUCE(8,2,1);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(4);  case _ => ERROR;  }
  case 3 => { case 4 => SHIFT(5);  case 0 => REDUCE(7,1,1);  case _ => ERROR;  }
  case 4 => { case _ => ACCEPT;  }
  case 5 => { case 3 => SHIFT(6);  case _ => ERROR;  }
  case 6 => { case 0 => REDUCE(8,3,3);  case 4 => REDUCE(8,3,3);  case _ => ERROR;  }
  case _ => { case _ => ERROR }
  }
}
