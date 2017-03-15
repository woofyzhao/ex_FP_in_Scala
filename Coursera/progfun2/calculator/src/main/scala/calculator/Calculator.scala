package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues(sig => Signal {
      eval(sig(), namedExpressions)
    })
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def evalInner(expr: Expr, firingRefs: Set[String]): Double = {
      expr match {
        case Literal(v) => v
        //case Ref(name) => references.get(name).map(sig => eval(sig(), references)).getOrElse(Double.NaN)
        case Ref(name) => {
          if (firingRefs.contains(name)) Double.NaN
          else evalInner(getReferenceExpr(name, references), firingRefs + name)
        }
        case Plus(a, b) => evalInner(a, firingRefs) + evalInner(b, firingRefs)
        case Minus(a, b) => evalInner(a, firingRefs) - evalInner(b, firingRefs)
        case Times(a, b) => evalInner(a, firingRefs) * evalInner(b, firingRefs)
        case Divide(a, b) => evalInner(a, firingRefs) / evalInner(b, firingRefs)
      }
    }

    evalInner(expr, Set())
  }

  /**
   * Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
    references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
