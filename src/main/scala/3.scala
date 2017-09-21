
object lang3 {


  def main(args: Array[String]): Unit = {
    println(ttinc[Dom])
    println(ttif[Dom])
  }

  def ttinc[D](implicit b: EBasic[D]): D = b.app(b.inc, b.app(b.inc, b.int(2)))
  def ttif[D](implicit b: EBasic[D], c: ECond[D]): D = c.if_(b.app(b.app(c.eq, b.int(3)), ttinc[D]), b.int(10), b.app(b.inc, ttinc[D]))

  sealed trait Dom
  case object DError extends Dom
  case class DInt(int: Int) extends Dom
  case class DBool(bool: Boolean) extends Dom
  case class DFun(fun: Dom => Dom) extends Dom

  trait EBasic[D] {
    def int(x: Int): D
    def inc: D
    def app(d1: D, d2: D): D
  }

  implicit val basicDom: EBasic[Dom] = new EBasic[Dom] {
    def int(x: Int) = DInt(x)
    def inc = DFun {
      case DInt(n) => DInt(n+1)
      case _ => DError
    }
    def app(d1: Dom, d2: Dom) = d1 match {
      case DFun(f) => f(d2)
      case _ => DError
    } 
  }

  trait ECond[D] {
    def eq: D
    def if_(cond: D, dt: D, df: D): D
  }

  implicit val condDom: ECond[Dom] = new ECond[Dom] {
    def eq: Dom = DFun {
      case DInt(x1) => DFun {
        case DInt(x2) => if(x1==x2) DBool(true) else DBool(false)
        case _ => DError
      }
      case _ => DError
    }
    def if_(cond: Dom, dt: Dom, df: Dom): Dom = cond match {
      case DBool(true) => dt
      case DBool(false) => df
      case _ => DError
    }
  }
}
