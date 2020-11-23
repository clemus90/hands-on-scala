def binarySearch[T: Ordering](arr: IndexedSeq[T], item: T): Boolean = {
  def binSearchIdx(from: Int, to: Int): Option[Int] = {
    if(to < from) {
      None
    } else {
      val mid = (from + to) / 2
      Ordering[T].compare(item, arr(mid)) match {
        case 0 => Some(mid)
        case n if n > 0 => binSearchIdx(mid + 1, to)
        case _ => binSearchIdx(from, mid - 1)
      }
    }
  }

  binSearchIdx(0, arr.length - 1).exists(_ => true)
}