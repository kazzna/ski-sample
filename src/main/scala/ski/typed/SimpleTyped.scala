package ski.typed

import ski.raw._
import ski.{RawLambdaTau, Tau, Typed}

sealed trait SimpleTyped extends Typed {
  def toTau: Tau = Tau(this)
}

object SimpleTyped {
  def apply(input: Input): SimpleTyped = input match {
    case Simple(a) => Variable(a)
    case Group(last :: Nil) => SimpleTyped(last)
    case Group(BackSlash :: tail) => {
      val p = tail.takeWhile(_ != Dot).map(_.asInstanceOf[Simple])
      val v = tail.dropWhile(_ != Dot).tail
      Lambda(p.map(s => Variable(s.value)), SimpleTyped(Group(v)))
    }
    case Group(vs) => Block(vs.map(v => SimpleTyped(v)).toVector).flatten
    case _ => ???
  }
}

final case class Lambda(ps: List[Variable], b: SimpleTyped) extends SimpleTyped {
  override def toString = s"λ${ps.map(_.toString).mkString}.$b"
}

final case class Variable(value: Char) extends SimpleTyped {
  override def toString = value.toString

  //override def toTau = ??? // Simple(this)
}

final case class Block(values: Vector[SimpleTyped]) extends SimpleTyped {
  override def toString = s"(${values.map(_.toString).mkString})"

  def flatten: SimpleTyped = if (values.length == 1) values.head match {
    case b: Block => b.flatten
    case a => a
  } else this

  def init: SimpleTyped = Block(values.init).flatten
  def last: SimpleTyped = values.last
}

object Block {
  def apply(xs: SimpleTyped*): Block = new Block(xs.toVector)
}
