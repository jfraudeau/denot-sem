
object lang1 {

  def main(args: Array[String]): Unit = {
    println(eval(tinc))
  }

  def tinc: Expr = EApp(EInc, EApp(EInc, EInt(2)))

  sealed trait Expr
  case class EInt(x: Int) extends Expr
  case object EInc extends Expr
  case class EApp(e1: Expr, e2: Expr) extends Expr

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
  }
}
