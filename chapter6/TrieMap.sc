class Trie[T]() {
  class Node(var value: Option[T],
             val children: collection.mutable.Map[Char, Node] = collection.mutable.Map())
  val root = new Node(None)
  def add(s: String, value: T) = {
    var current = root
    for (c <- s) current = current.children.getOrElseUpdate(c, new Node(None))
    current.value = Some(value)
  }
  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    (for {
      node <- current
      innerValue <- node.value
    } yield innerValue).nonEmpty
  }
  def prefixesMatchingString0(s: String): Set[(Int, T)] = {
    var current = Option(root)
    val output = Set.newBuilder[(Int, T)]
    for ((c, i) <- s.zipWithIndex if current.nonEmpty) {
      for {
        node <- current
        innerValue <- node.value
      } output += i -> innerValue 
      current = current.get.children.get(c)
    }
    for {
      node <- current
      innerValue <- node.value
    } output += s.length -> innerValue
    output.result()
  }
  def prefixesMatchingString(s: String): Map[String, T] = {
    val output = Map.newBuilder[String, T]
    for ((idx, value) <- prefixesMatchingString0(s)) {
      output += ((s.substring(0, idx), value))
    }
    output.result()
  }
  def stringsMatchingPrefix(s: String): Map[String, T] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Map()
    else {
      val output = Map.newBuilder[String, T]
      def recurse(current: Node, path: List[Char]): Unit = {
        for (innerValue <- current.value) output += ((s + path.reverse.mkString, innerValue))
        for ((c, n) <- current.children) recurse(n, c :: path)
      }
      recurse(current.get, Nil) // recursive walk
      output.result()
    }
  }

  def get(s: String): Option[T] = {
    var current = Option(root)
    for(c <- s if current.nonEmpty) current = current.get.children.get(c)
    for {
      node <- current
      value <- node.value
    } yield value
  }
}