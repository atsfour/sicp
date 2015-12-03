sealed trait Expr

case class Num(i: Double) extends Expr {
  override def toString = i.toString
}

case class Var(x: Symbol) extends Expr {
  override def toString = x.toString
}

case class Sum(addend: Expr, augend: Expr) extends Expr

case class Prod(multiplier: Expr, multiplicand: Expr) extends Expr

object Expr {

  implicit def int2Num(i: Int): Num = Num(i)
  implicit def double2Num(x: Double): Num = Num(x)
  implicit def symbol2Var(x: Symbol): Var = Var(x)

  def makeSum(x: Expr, y: Expr): Expr = (x, y) match {
    case (Num(0), yy) => yy
    case (xx, Num(0)) => xx
    case (Num(xx), Num(yy)) => Num(xx + yy)
    case _ => Sum(x, y)
  }

  def makeProduct(x: Expr, y: Expr): Expr = (x, y) match {
    case (Num(0), _) => Num(0)
    case (_, Num(0)) => Num(0)
    case (Num(1), _) => y
    case (_, Num(1)) => x
    case (Num(xx), Num(yy)) => Num(xx * yy)
    case _ => Prod(x, y)
  }

  def deriv(exp: Expr, v: Var): Expr = exp match {
    case Num(_) => Num(0)
    case Var(x) => if (exp == v) Num(1) else Num(0)
    case Sum(x, y) => makeSum(deriv(x, v), deriv(y, v))
    case Prod(x, y) => {
      makeSum(
        makeProduct(deriv(x, v), y),
        makeProduct(x, deriv(y, v))
      )
    }
  }
}
import Expr._
val xCubic: Expr = makeProduct('x, makeProduct('x, 'x))
val xPlus1Cubic: Expr = makeProduct(makeSum('x, 1), makeProduct(makeSum('x, 1), makeSum('x, 1)))
deriv(xCubic, 'x)
deriv(xPlus1Cubic, 'x)
