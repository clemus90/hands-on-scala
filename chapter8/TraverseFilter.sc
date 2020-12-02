val jsonString = os.read(os.pwd / "ammonite-releases.json")

val data = ujson.read(jsonString)

def traverse(v: ujson.Value): Iterable[String] = v match {
    case a: ujson.Arr => a.arr.flatMap(traverse)
    case o: ujson.Obj => o.obj.values.flatMap(traverse)
    case s: ujson.Str => Seq(s.str)
    case _ => Nil
}

def traverseFilter(v: ujson.Value): ujson.Value = {
    val noHttps = (s: String) => !s.contains("https://")
    v match {
        case a: ujson.Arr => ujson.Arr(a.arr.map(traverseFilter).filter(_ != ujson.Null))
        case o: ujson.Obj => ujson.Obj(o.obj.map { case (k,v) => (k, traverseFilter(v))}.retain { (k,v) => v != ujson.Null })
        case s: ujson.Str => noHttps(s.str) match {
            case true => s
            case false => ujson.Null
        }
        case other => other
    }
}

val newData = traverseFilter(data)

assert(data.toString.contains("https://"))

assert(!newData.toString.contains("https://"))