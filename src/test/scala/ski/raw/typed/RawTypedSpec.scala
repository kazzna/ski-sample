package ski.raw.typed

import ski.UnitSpec
import ski.raw.Input

class RawTypedSpec extends UnitSpec {
  "SimpleType" should {
    "return one lambda" when {
      "a lambda statement is given" in {
        val x = Variable('x')
        val y = Variable('y')
        val z = Variable('z')
        Input("""\xyz.xz(yz)""").toTyped should be(Lambda(List(x, y, z), SimpleTyped.block(x, z, SimpleTyped.block(y, z))))
      }
    }

    "be convertible a complicated lambda" in {
      val simple = Input("""\nfx.f(nfx)""").toTyped
      simple.toString should be("""λnfx.f(nfx)""")
    }

    "unwrap doubled parentheses" in {
      val wrapped = Input("(((a)))").toTyped
      val unwrapped = Input("((a))").toTyped
      wrapped should be(unwrapped)
    }

    "wrap lambda in group" in {
      val nested = Input("""\n.n(\x.(\tf.f))(\tf.t)""").toTyped

      nested.toString should be("λn.n(λx.(λtf.f))(λtf.t)")
    }

    "wrap single lambda in lambda body" in {
      val lambda = Input("""\l.l(\xy.(\tf.f))""").toTyped

      lambda.toString should be("λl.l(λxy.(λtf.f))")
    }
  }
}
