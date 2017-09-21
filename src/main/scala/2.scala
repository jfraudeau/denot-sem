
object lang2 {

  def main(args: Array[String]): Unit = {
    println(eval(tif))
  }

  def tinc1: Expr = EApp(EInc, EApp(EInc, EInt(2)))
  def tif: Expr = EIf(EApp(EApp(EEq, EInt(3)), tinc1), EInt(10), EApp(EInc, tinc1))

  sealed trait Expr
  case class EInt(x: Int) extends Expr
  case object EInc extends Expr
  case class EApp(e1: Expr, e2: Expr) extends Expr
  case object EEq extends Expr
  case class EIf(e: Expr, et: Expr, ef: Expr) extends Expr

  sealed trait Dom
  case object DError extends Dom
  case class DInt(int: Int) extends Dom
  case class DBool(bool: Boolean) extends Dom
  case class DFun(fun: Dom => Dom) extends Dom

  def eval: Expr => Dom = {
    case EInt(x) => DInt(x)
    case EInc => DFun {
      case DInt(n) => DInt(n+1)
      case _ => DError
    }
    case EApp(e1, e2) => eval(e1) match {
      case DFun(f) => f(eval(e2))
      case _ => DError
    }
    case EEq => DFun {
      case DInt(x1) => DFun {
        case DInt(x2) => if(x1==x2) DBool(true) else DBool(false)
        case _ => DError
      }
      case _ => DError
    }
    case EIf(e, et, ef) => eval(e) match {
      case DBool(true) => eval(et)
      case DBool(false) => eval(ef)
      case _ => DError
    }
  }
}
