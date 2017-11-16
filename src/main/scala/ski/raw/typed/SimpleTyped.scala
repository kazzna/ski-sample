package ski.raw.typed

import ski.{Tau, Typed}

sealed trait SimpleTyped extends Typed {
  def toTau: Tau = Tau(this)
}

object SimpleTyped {
  def block(xs: SimpleTyped*): SimpleTyped = new Block(xs.toVector) {}.flatten
}

final case class Lambda(ps: List[Variable], body: SimpleTyped) extends SimpleTyped {
  override def toString: String = s"λ${ps.map(_.toString).mkString}.$bodyString"

  private val bodyString: String = body match {
    case l: Lambda => s"(${l.toString})"
    case _ => body.toString
  }
}

final case class Variable(value: Char) extends SimpleTyped {
  override def toString: String = value.toString
}

sealed abstract case class Block(values: Vector[SimpleTyped]) extends SimpleTyped {
  override def toString: String =
    if (values.length == 1) values.head.toString
    else values.map(_ match {
      case b: Block => s"(${b.flatten.toString})"
      case l: Lambda => s"(${l.toString})"
      case a => a.toString
    }).mkString

  def flatten: SimpleTyped = if (values.length == 1) values.head match {
    case b: Block => b.flatten
    case v => v
  } else this

  def init: SimpleTyped = SimpleTyped.block(values.init: _*)

  def last: SimpleTyped = values.last
}
