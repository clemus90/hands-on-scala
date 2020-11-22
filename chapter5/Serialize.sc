trait JsonEncoder[T] { def encode(data: T): String }
object JsonEncoder {
    implicit object EncodeInt extends JsonEncoder[Int] {
        def encode(data: Int): String = data.toString
    }

    implicit object EncodeBoolean extends JsonEncoder[Boolean] {
        def encode(data: Boolean): String = data.toString
    }

    implicit object EncodeDouble extends JsonEncoder[Double] {
        def encode(data: Double): String = data.toString
    }

    implicit def encodeSeq[T](implicit enc: JsonEncoder[T]) = new JsonEncoder[Seq[T]] {
       def encode(data: Seq[T]): String = data.map(enc.encode(_)).mkString("[",",","]") 
    }

    implicit def encodeTuple[T, U](implicit enc1: JsonEncoder[T], enc2: JsonEncoder[U]) = new JsonEncoder[(T, U)] {
        def encode(data: (T, U)): String = {
            val (left, right) = data
            List(enc1.encode(left), enc2.encode(right)).mkString("[", ",", "]")
        }
    }
}

def writeToString[T](data: T)(implicit enc: JsonEncoder[T]) = enc.encode(data)