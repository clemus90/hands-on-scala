import ammonite.util.Ex
sealed trait Expr
case class  BinOp(left: Expr, op: String, right: Expr) extends Expr
case class  Literal(value: Int) extends Expr
case class  Variable(name: String) extends Expr

def stringify(expr: Expr): String = expr match {
  case BinOp(left, op, right) => s"(${stringify(left)} $op ${stringify(right)})"
  case Literal(value) => value.toString
  case Variable(name) => name
}

def simplify(expr: Expr): Expr = {
  val res = expr match {
    case BinOp(Literal(value1), "+", Literal(value2)) => Literal(value1 + value2)
    case BinOp(Literal(value1), "-", Literal(value2)) => Literal(value1 - value2)
    case BinOp(Literal(value1), "*", Literal(value2)) => Literal(value1 * value2)

    case BinOp(Literal(0), "+", right) => simplify(right)
    case BinOp(left, "+", Literal(0)) => simplify(left)

    case BinOp(left, "-", Literal(0)) => simplify(left)

    case BinOp(Literal(0), "*", _) => Literal(0)
    case BinOp(_, "*", Literal(0)) => Literal(0)
    case BinOp(Literal(1), "*", right) => simplify(right)
    case BinOp(left, "*", Literal(1)) => simplify(left)

    case BinOp(left, op, right) => BinOp(simplify(left), op, simplify(right))
    case _ => expr
  }
  if(res == expr) res
  else simplify(res)
}