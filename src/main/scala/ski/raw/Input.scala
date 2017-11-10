package ski.raw

import ski.typed.{Block, Lambda, SimpleTyped, Variable}

/**
  * 入力データ
  */
sealed trait Input {
  def toSimpleTyped = SimpleTyped(this)
}

/**
  * '\'
  */
case object BackSlash extends Input

/**
  * '.'
  */
case object Dot extends Input

/**
  * 変数
  *
  * @param value
  */
case class Simple(value: Char) extends Input

/**
  * 変数グループ
  *
  * @param values
  */
case class Group(values: List[Input]) extends Input

object Group {
  def apply(vs: Input*): Group = new Group(vs.toList)
}

object Input {
  def apply(input: String): Input = {
    def f(cs: List[Char], acc: List[Input]): (Input, List[Char]) = if (cs.isEmpty) {
      (Group(acc.reverse), cs)
    } else {
      cs.head match {
        case '\\' => f(cs.tail, BackSlash +: acc)
        case '.' => f(cs.tail, Dot +: acc)
        case '(' => {
          val (input, rest) = f(cs.tail, List.empty)
          f(rest, input +: acc)
        }
        case ')' => (Group(acc.reverse), cs.tail)
        case c => f(cs.tail, Simple(c) +: acc)
      }
    }

    val (res, _) = f(normalize(input).toCharArray.toList, List.empty)
    res
  }

  private def normalize(input: String): String = {
    input.replace("\t", " ").replace(" ", "")
  }
}
