
package SAB
object Tables {
val GOTOTABLE: Int => Int => Int = {
  case 0 => { case 5 => 2;  case 6 => 3;  case 7 => 4;  }
  case _ => { case _ => throw new Throwable("BAD GOTO")}
  }

import org.sufrin.scalalr.Action._
val ACTIONTABLE: Int => Int => Action = {
  case 0 => { case 3 => SHIFT(1);  case _ => ERROR;  }
  case 1 => { case 0 => REDUCE(6,3,1);  case _ => ERROR;  }
  case 2 => { case 0 => SHIFT(5);  case _ => ERROR;  }
  case 3 => { case 0 => REDUCE(5,1,1);  case _ => ERROR;  }
  case 4 => { case 0 => REDUCE(5,2,1);  case _ => ERROR;  }
  case 5 => { case _ => ACCEPT;  }
  case _ => { case _ => ERROR }
  }
}
