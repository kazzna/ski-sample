package ski.raw

import ski.UnitSpec

class InputSpec extends UnitSpec {
  "Input" should {
    "return 1 group" in {
      Input("ab") should be(Group(List(Simple('a'), Simple('b'))))
    }

    "return Group of Groups" when {
      "() is given" in {
        Input("(ab)") should be(Group(List(Group(List(Simple('a'), Simple('b'))))))
      }
    }

    "returns Dot for '.'" in {
      Input("a.b") should be(Group(List(Simple('a'), Dot, Simple('b'))))
    }

    """returns BackSlash for '\'!""" in {
      Input("a\\b") should be(Group(List(Simple('a'), BackSlash, Simple('b'))))
    }

    """parse complex one""" in {
      val f = Simple('f')
      val a = Simple('a')
      val b = Simple('b')
      Input("""\fab.(fa(fb))""") should be(Group(List(BackSlash, f, a, b, Dot, Group(List(f, a, Group(List(f, b)))))))
      Input("""\a.a""") should be(Group(List(BackSlash, a, Dot, a)))
    }
  }
}
