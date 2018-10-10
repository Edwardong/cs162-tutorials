
package edu.ucsb.cs.cs162.tuts.calculator

// A mathematical expression.
sealed trait Expr

// A variable expression with a name.
final case class Var(name: String) extends Expr

// A number expression with a numeric value.
final case class Num(value: Double) extends Expr

// A unary operation expression (eg. -5 is UnOp("-", Num(5))).
final case class UnOp(op: String, value: Expr) extends Expr

// A binary operation expression (eg. 2+3 is BinOp("+", Num(2), Num(3))))
final case class BinOp(op: String, left: Expr, right: Expr) extends Expr

// The calculator object.
object Calculator {

  // Simplifies the head of the expression (should not simplify recursively!).  
  def simplifyHead(expr: Expr): Expr = expr match {
      case UnOp("-", UnOp("-", e)) => e
      case BinOp("+", e, Num(0)) => e
      case BinOp("+", Num(0), e) => e
      case BinOp("*", e, Num(1)) => e
      case BinOp("*", Num(1), e) => e
      case BinOp("*", _, Num(0)) => Num(0)
      case BinOp("*", Num(0), _) => Num(0)
      case BinOp("-", _, _) => Num(0)
      case _ => expr
  }
  
  // Evaluates the expression to a numeric value.
  def evaluate(expr: Expr): Double = expr match {
    case Num(a) => a
    case Var(_) => 1
    case UnOp(o, _) if o != "-" => 1
    case UnOp(o, a) if o == "-" => -evaluate(a)
    case BinOp(o, _, _) if (o != "+" && o != "-" && o != "*") => 1
    case BinOp(o, a, b) if o == "+" => {
      if (a == Var("DUP")) {
        return evaluate(b) + evaluate(b)
      }
      if (b == Var("DUP")) {
        return evaluate(a) + evaluate(a)
      }
      return evaluate(a) + evaluate(b)
    }
    case BinOp(o, a, b) if o == "-" => {
      if (a == Var("DUP")) {
        return evaluate(b) - evaluate(b)
      }
      if (b == Var("DUP")) {
        return evaluate(a) - evaluate(a)
      }
      return evaluate(a) - evaluate(b)
    }
    case BinOp(o, a, b) if o == "*" => {
      if (a == Var("DUP")) {
        return evaluate(b) * evaluate(b)
      }
      if (b == Var("DUP")) {
        return evaluate(a) * evaluate(a)
      }
      return evaluate(a) * evaluate(b)
    }
  }
}
