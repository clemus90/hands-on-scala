class Msg(val id: Int, val parent: Option[Int], val txt: String)

def printMessages(messages: Array[Msg]): Unit = {
  def rec(currentParent: Option[Int], level: Int, idx: Int): Int = {
    if(idx >= messages.length) idx
    else if (currentParent == messages(idx).parent) {
      println(" " * (level * 4) + s"#$idx ${messages(idx).txt}")
      val advancedIdx = rec(Some(messages(idx).id), level + 1, idx + 1)
      rec(currentParent, level, advancedIdx)
    }
    else {
      idx
    }
    
  }

  rec(None, 0, 0)
}

printMessages(Array(
  new Msg(0, None, "Hello"),
  new Msg(1, Some(0), "World"),
  new Msg(2, None, "I am Cow"),
  new Msg(3, Some(2), "Hear me moo"),
  new Msg(4, Some(2), "Here I stand"),
  new Msg(5, Some(2), "I am Cow"),
  new Msg(6, Some(5), "Here me moo, moo"),
  new Msg(7, Some(2), "another message"),
  new Msg(8, None, "top level message"),

))