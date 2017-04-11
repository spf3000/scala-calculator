package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
      for (namedExpression <- namedExpressions)
       yield (namedExpression._1, Signal(eval(namedExpression._2(), namedExpressions)))
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    Signal(expr).apply()
    expr match{
      case Literal(v) => v
      case Ref(n) => {
        if (references.keySet.contains(n)) {
          eval(getReferenceExpr(n, references), references - n)
        }
        else Double.NaN
      }
      case Plus(a,b) => eval(a, references) + eval(b, references)
      case Minus(a,b) => eval(a, references) - eval(b, references)
      case Times(a,b) => eval(a, references) * eval(b, references)
      case Divide(a,b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
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
