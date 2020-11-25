// scala 2.13.0

def searchPaths[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = {
  val seen = collection.mutable.Map(start -> List(start))
  val stack = collection.mutable.ArrayDeque(start -> List(start))
  while (stack.nonEmpty) {
    val (current, path) = stack.removeLast()
    for {
      next <- graph(current)
      if !seen.contains(next) || seen(next).length > path.length + 1
     } {
      val newPath = next :: path
      seen(next) = newPath
      stack.append((next, newPath))
    }
  }
  seen.toMap
}

def shortestPath[T](start: T, dest: T, graph: Map[T, Seq[T]]): Seq[T] = {
  val shortestReversedPaths = searchPaths(start, graph)
  shortestReversedPaths(dest).reverse
}