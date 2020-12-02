class Foo(val i: Int, val s: String)

implicit val fooRw = upickle.default.readwriter[ujson.Value].bimap[Foo](
    f => ujson.Obj("i" -> f.i, "s" -> f.s),
    obj => new Foo(obj("i").num.toInt, obj("s").str)
)

val foo = new Foo(1337, "mooo")

val serialized = upickle.default.write(foo)

assert(serialized == """{"i":1337,"s":"mooo"}""")

val deserialized = upickle.default.read[Foo](serialized)

assert(foo.i == deserialized.i)
assert(foo.s == deserialized.s)