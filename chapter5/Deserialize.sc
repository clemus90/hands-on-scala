trait StrParser[T] { def parse(s: String): T }
object StrParser {
    implicit object ParseInt extends StrParser[Int] {
        def parse(s: String): Int = s.toInt
    }
    
    implicit object ParseBoolean extends StrParser[Boolean] {
        def parse(s: String): Boolean = s.toBoolean
    }
    
    implicit object ParseDouble extends StrParser[Double] {
        def parse(s: String): Double = s.toDouble
    }

    implicit def ParseSeq[T](implicit p: StrParser[T]) = new StrParser[Seq[T]] {
        def parse(s: String): Seq[T] = splitExpression(s).map(p.parse)
    }

    implicit def ParseTuple[T, U](implicit p1: StrParser[T], p2: StrParser[U]) = new StrParser[(T, U)] {
        def parse(s: String): (T, U) = {
            val Seq(left, right) = splitExpression(s)
            (p1.parse(left), p2.parse(right))
        }
    }
}

def parseFromString[T](s: String)(implicit parser: StrParser[T]) = parser.parse(s)

case class ExpressionAcc(expSeq: List[String], currStr: String, openSeq: Int)
def splitExpression(s: String): Seq[String] = {
    val acc = s.substring(1, s.length()- 1).foldLeft(ExpressionAcc(Nil, "", 0)){ (acc, char) => 
        (char, acc.openSeq) match {
            case ('[', n) => acc.copy(currStr = acc.currStr + "[", openSeq = n + 1)
            case (']', n) => acc.copy(currStr = acc.currStr + "]", openSeq = n - 1)
            case (',', 0) => acc.copy(expSeq = acc.expSeq :+ acc.currStr, currStr = "")
            case (c, _) => acc.copy(currStr = acc.currStr + c)
        }
    }
    acc.expSeq :+ acc.currStr
}