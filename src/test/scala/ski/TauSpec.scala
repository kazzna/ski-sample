package ski

import ski.raw.Input
import ski.raw.typed.{Lambda, SimpleTyped, Variable}

class TauSpec extends UnitSpec {
  "Tau#convertOne" should {
    "convert to Variable" when {
      "Tau was just a variable(T[x] = x)" in {

        Input("a").toTyped.toTau.convert should be(Constant('a'))
      }
    }

    "convert to a pair of Tau" when {
      "the function application was given(T[x y] = T[x] T[y])" in {
        val exec = Input("""xz(yz)""").toTyped.toTau
        val left = Input("xz").toTyped.toTau
        val right = Input("yz").toTyped.toTau

        exec.convert.toString should be("(T[xz] T[yz])")

      }
    }

    "convert to K T[a]" when {
      """"\f.a" was given(T[λx.a] = K T[a])""" in {
        val input = Input("""\f.a""").toTyped.toTau

        input.convert.toString should be("(K T[a])")
        input.convert.convert.toString should be("(K a)")
      }
    }

    "convert to I" when {
      "Tau was a ID function(T[λx.x] = I)" in {
        val id = Input("\\a.a").toTyped.toTau

        id.toString should be("T[λa.a]")
        id.convert.toString should be("I")
      }
    }

    "convert to curried lambda" when {
      "two or more parameters were given(T[λxy.a] = T[λx.T[λy.a]]" in {
        val in = Input("""\abc.b(ac)""").toTyped.toTau
        val body = Input("""\c.b(ac)""").toTyped

        in.convert.toString should be("T[λab.T[λc.b(ac)]]")
      }
    }

    "split to S and Ts" when {
      "tow or more body were given(T[λx.(a b)] = S T[λx.a] T[λx.b])" in {
        val in = Input("\\x.ab").toTyped.toTau
        val xa = Lambda(List(Variable('x')), SimpleTyped.block(Variable('a'))).toTau
        val xb = Lambda(List(Variable('x')), SimpleTyped.block(Variable('b'))).toTau

        in.convert.toString should be("(S T[λx.a] T[λx.b])")
      }
    }

    "convert inner lambda" when {
      "T has just one param lambda" in {
        val in = Input("""\a.\b.ba""").toTyped.toTau
        in.toString should be("T[λa.λb.ba]")

        val ll = in.convert
        ll.toString should be("T[λa.T[λb.ba]]")

        ll.convert.toString should be("T[λa.(S T[λb.b] T[λb.a])]")
      }
    }

    "convert inner block of lambda apply" when {
      "lambda has lambda apply in body" in {
        val in = Input("""\f.\x.f(nfx)""").toTyped.toTau
        val ll = in.convert.convert

        ll.toString should be("T[λf.(S T[λx.f] T[λx.nfx])]")
        ll.convert.toString should be("T[λf.(S T[λx.f] (S T[λx.nf] T[λx.x]))]")
      }
    }

    "convert last one only" when {
      "the group was given" in {
        val t = Input("\\x.(xz)(yz)").toTyped.toTau
        t.toString should be("T[λx.(xz)(yz)]")

        val t2 = t.convert
        t2.toString should be("(S T[λx.xz] T[λx.yz])")

        val out = t2.convert
        out.toString should be("(S T[λx.xz] (S T[λx.y] T[λx.z]))")
      }
    }

    "convert 3 params lambda" in {
      val t = Input("\\nfx.f(nfx)").toTyped.toTau

      val t2 = t.convert
      t2.toString should be("T[λnf.T[λx.f(nfx)]]")

      val t3 = t2.convert
      t3.toString should be("T[λn.T[λf.T[λx.f(nfx)]]]")

    }

    "convert lambda" when {
      "body was already converted" in {
        val in = Input("\\x.\\y.yx").toTyped.toTau

        val m1 = in.convert
        m1.toString should be("T[λx.T[λy.yx]]")

        val m2 = m1.convert
        m2.toString should be("T[λx.(S T[λy.y] T[λy.x])]")

        val m3 = m2.convert
        m3.toString should be("T[λx.(S T[λy.y] (K T[x]))]")

        val m4 = m3.convert
        m4.toString should be("T[λx.(S T[λy.y] (K x))]")

        val m5 = m4.convert
        m5.toString should be("T[λx.(S I (K x))]")

        val m6 = m5.convert
        m6.toString should be("(S T[λx.(S I)] T[λx.(K x)])")
      }
    }
  }

  "convert succ" when {
    """input is "\nfx.f(nfx)"""" in {
      val in = Input("\\nfx.f(nfx)").toTyped.toTau

      def f(in: Convertible)(result: Seq[String]): Unit = if (result.isEmpty) () else {
        in.toString should be(result.head)
        f(in.convert)(result.tail)
      }

      f(in) {
        Seq(
          "T[λnfx.f(nfx)]",
          "T[λnf.T[λx.f(nfx)]]",
          "T[λn.T[λf.T[λx.f(nfx)]]]",
          "T[λn.T[λf.(S T[λx.f] T[λx.nfx])]]",
          "T[λn.T[λf.(S T[λx.f] (S T[λx.nf] T[λx.x]))]]",
          "T[λn.T[λf.(S T[λx.f] (S T[λx.nf] I))]]",
          "T[λn.T[λf.(S T[λx.f] (S (S T[λx.n] T[λx.f]) I))]]",
          "T[λn.T[λf.(S T[λx.f] (S (S T[λx.n] (K T[f])) I))]]",
          "T[λn.T[λf.(S T[λx.f] (S (S T[λx.n] (K f)) I))]]",
          "T[λn.T[λf.(S T[λx.f] (S (S (K T[n]) (K f)) I))]]",
          "T[λn.T[λf.(S T[λx.f] (S (S (K n) (K f)) I))]]",
          "T[λn.T[λf.(S (K T[f]) (S (S (K n) (K f)) I))]]",
          "T[λn.T[λf.(S (K f) (S (S (K n) (K f)) I))]]",
          "T[λn.(S T[λf.(S (K f))] T[λf.(S (S (K n) (K f)) I)])]",
          "T[λn.(S T[λf.(S (K f))] (S T[λf.(S (S (K n) (K f)))] T[λf.I]))]",
          "T[λn.(S T[λf.(S (K f))] (S T[λf.(S (S (K n) (K f)))] (K T[I])))]",
          "T[λn.(S T[λf.(S (K f))] (S T[λf.(S (S (K n) (K f)))] (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] T[λf.(S (K n) (K f))]) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S T[λf.(S (K n))] T[λf.(K f)])) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S T[λf.(S (K n))] (S T[λf.K] T[λf.f]))) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S T[λf.(S (K n))] (S T[λf.K] I))) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S T[λf.(S (K n))] (S (K T[K]) I))) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S T[λf.(S (K n))] (S (K K) I))) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S T[λf.(S (K n))] K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S T[λf.S] T[λf.(K n)]) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S T[λf.S] (S T[λf.K] T[λf.n])) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S T[λf.S] (S T[λf.K] (K T[n]))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S T[λf.S] (S T[λf.K] (K n))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S T[λf.S] (S (K T[K]) (K n))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S T[λf.S] (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S (K T[S]) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S T[λf.S] (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S (K T[S]) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S T[λf.(S (K f))] (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S T[λf.S] T[λf.(K f)]) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S T[λf.S] (S T[λf.K] T[λf.f])) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S T[λf.S] (S T[λf.K] I)) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S T[λf.S] (S (K T[K]) I)) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S T[λf.S] (S (K K) I)) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S T[λf.S] K) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S (K T[S]) K) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "T[λn.(S (S (K S) K) (S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I)))]",
          "(S T[λn.(S (S (K S) K))] T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)) (K I))])",
          "(S T[λn.(S (S (K S) K))] (S T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)))] T[λn.(K I)]))",
          "(S T[λn.(S (S (K S) K))] (S T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)))] (S T[λn.K] T[λn.I])))",
          "(S T[λn.(S (S (K S) K))] (S T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)))] (S T[λn.K] (K T[I]))))",
          "(S T[λn.(S (S (K S) K))] (S T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)))] (S T[λn.K] (K I))))",
          "(S T[λn.(S (S (K S) K))] (S T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)))] (S (K T[K]) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S T[λn.(S (S (K S) (S (S (K S) (S (K K) (K n))) K)))] (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] T[λn.(S (K S) (S (S (K S) (S (K K) (K n))) K))]) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] T[λn.(S (S (K S) (S (K K) (K n))) K)])) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (S (K S) (S (K K) (K n))))] T[λn.K]))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (S (K S) (S (K K) (K n))))] (K T[K])))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (S (K S) (S (K K) (K n))))] (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] T[λn.(S (K S) (S (K K) (K n)))]) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] T[λn.(S (K K) (K n))])) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (K K))] T[λn.(K n)]))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (K K))] (S T[λn.K] T[λn.n])))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (K K))] (S T[λn.K] I)))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (K K))] (S (K T[K]) I)))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (K K))] (S (K K) I)))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S T[λn.(S (K K))] K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] T[λn.(K K)]) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.K] T[λn.K])) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.K] (K T[K]))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.K] (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (K T[K]) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S (K T[S]) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S T[λn.S] T[λn.(K S)]) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] T[λn.S])) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] (K T[S]))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S T[λn.S] (S (K T[K]) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S T[λn.S] (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S (K T[S]) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S T[λn.S] (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S (K T[S]) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S T[λn.(S (K S))] (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S T[λn.S] T[λn.(K S)]) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] T[λn.S])) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] (K T[S]))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S T[λn.S] (S (K T[K]) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S T[λn.S] (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S (K T[S]) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S T[λn.S] (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S (K T[S]) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S T[λn.(S (S (K S) K))] (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] T[λn.(S (K S) K)]) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S T[λn.(S (K S))] T[λn.K])) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S T[λn.(S (K S))] (K T[K]))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S T[λn.(S (K S))] (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S T[λn.S] T[λn.(K S)]) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] T[λn.S])) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] (K T[S]))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S T[λn.S] (S T[λn.K] (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S T[λn.S] (S (K T[K]) (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S T[λn.S] (S (K K) (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S (K T[S]) (S (K K) (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S T[λn.S] (S (S (K S) (S (K K) (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S (K T[S]) (S (S (K S) (S (K K) (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
          "(S (S (K S) (S (S (K S) (S (K K) (K S))) (K K))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) K))) (K K)))) (S (K K) (K I))))",
        )
        // "(S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) (K I)))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (S (K S) (S (K K) (K S))) (S (S (K S) (S (K K) (K K))) (S (K K) I)))) (S (S (K S) (S (K K) (K K))) (K I))))) (S (K K) (K I))))"
      }
    }
  }
}
