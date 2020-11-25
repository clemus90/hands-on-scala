class ImmutableTrie(strs: Seq[String]) { trie => 
  case class Node(val hasValue: Boolean, val children: Map[Char, Node])
  object Node {
    def fromString(str: Seq[Char], original: Option[Node]): Node = {
      val originalChildren = original.map(_.children).getOrElse(Map())
      val originalHasValue = original.map(_.hasValue).getOrElse(false)
      str match {
        case Nil => new Node(true, originalChildren)
        case head :: tail => new Node(originalHasValue, originalChildren ++ Map(head -> fromString(tail, originalChildren.get(head))))
      }
    }
  }

  val root = strs.foldLeft(new Node(false, Map()))((node, str) => Node.fromString(str.toList, Some(node)))

  // BOOK CODE
  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.exists(_.hasValue)
  }
  def prefixesMatchingString0(s: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for ((c, i) <- s.zipWithIndex if current.nonEmpty) {
      if (current.get.hasValue) output += i
      current = current.get.children.get(c)
    }
    if (current.exists(_.hasValue)) output += s.length
    output.result()
  }
  def prefixesMatchingString(s: String): Set[String] = {
    prefixesMatchingString0(s).map(s.substring(0, _))
  }
  def stringsMatchingPrefix(s: String): Set[String] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]
      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.hasValue) output += (s + path.reverse.mkString)
        for ((c, n) <- current.children) recurse(n, c :: path)
      }
      recurse(current.get, Nil) // recursive walk
      output.result()
    }
  }
}