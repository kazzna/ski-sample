package ski

class PythonFormatSpec extends UnitSpec {
  "PythonFormat.toString" should {
    "return I" when {
      "I is given" in {
        PythonFormat.toString(I) should be(Some("I"))
      }
    }

    "return K" when {
      "K has no parameter" in {
        val k = K(None)
        PythonFormat.toString(k) should be(Some("K"))
      }
    }

    "return K($param)" when {
      "K has 1 parameter" in {
        val k = K(Some(I))
        PythonFormat.toString(k) should be(Some("K(I)"))

        val k2 = K(Some(K(None)))

        PythonFormat.toString(k2) should be(Some("K(K)"))
      }
    }

    "return S" when {
      "S has no parameter" in {
        val s = S(None)
        PythonFormat.toString(s) should be(Some("S"))
      }
    }

    "return S($param)" when {
      "S has 1 parameter" in {
        val s1 = S(Some((I, None)))
        PythonFormat.toString(s1) should be(Some("S(I)"))
      }
    }

    "return S($param1)($param2)" when {
      "S has 2 parameters" in {
        val s1 = S(Some((I, Some(I))))
        PythonFormat.toString(s1) should be(Some("S(I)(I)"))
      }
    }
  }
}
