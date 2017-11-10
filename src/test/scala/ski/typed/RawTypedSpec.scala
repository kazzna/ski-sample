package ski.typed

import ski.UnitSpec
import ski.raw.Input

class RawTypedSpec extends UnitSpec {
  "SimpleType" should {
    "return one lambda" when {
      "a lambda statement is given" in {
        val x = Variable('x')
        val y = Variable('y')
        val z = Variable('z')
        Input("""\xyz.xz(yz)""").toSimpleTyped should be(Lambda(List(x, y, z), Block(x, z, Block(y, z))))
      }
    }

    "be convertible a complicated lambda" in {
      val simple = Input("""\nfx.f(nfx)""").toSimpleTyped
      simple.toString should be("""λnfx.(f(nfx))""")
    }

    "unwrap doubled parentheses" in {
      val wrapped = Input("(((a)))").toSimpleTyped
      val unwrapped = Input("((a))").toSimpleTyped
      wrapped should be(unwrapped)
    }
  }
}

