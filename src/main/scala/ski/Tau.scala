package ski

import ski.raw.typed
import ski.raw.typed.{Block, Lambda, SimpleTyped, Variable}

/**
  * 変換処理中間状態
  */
trait Tau extends Typed with Convertible

object Tau {
  def apply(st: SimpleTyped): Tau = st match {
    case a: Variable => VariableTau(a)
    case b: Block => BlockTau(b)
    case l: Lambda => RawLambdaTau(l.ps, l.body).toLambdaTau
  }

  def apply(ski: SKI): Tau = ski match {
    case s: S => STau(s)
    case k: K => KTau(k)
    case I => ITau
  }
}

case class VariableTau(value: Variable) extends Tau {
  override def toString: String = s"T[$value]"

  override def convert = Constant(value.value)

  override def isConvertible = true
}

case class BlockTau(block: Block) extends Tau {
  override def toString: String = s"T[$block]"

  override def convert: Convertible =
    if (block.values.length == 1) Tau(block.values.head)
    else ConvertibleList(Tau(block.init), Tau(block.last))

  override def isConvertible = true
}

case class ConvertibleList(taus: List[Convertible]) extends Convertible {
  override def toString: String = s"(${taus.map(_.toString).mkString(" ")})"

  override def convert: Convertible = {
    val rev = taus.reverse
    val done = rev.takeWhile(!_.isConvertible)
    val unDone = rev.dropWhile(!_.isConvertible)
    unDone.headOption.map { head =>
      val ts = done ++ (head.convert +: unDone.tail)
      ConvertibleList(ts.reverse)
    }.getOrElse(this)
  }

  override def isConvertible: Boolean = taus.foldLeft(false)(_ || _.isConvertible)

  def split: (ConvertibleList, Option[Convertible]) =
    if (taus.length > 1) (ConvertibleList(taus.init), Some(taus.last))
    else (this, None)
}

object ConvertibleList {
  def apply(values: Convertible*): ConvertibleList = new ConvertibleList(values.toList)
}


trait LambdaTau extends Tau {
  override final def isConvertible: Boolean = true
}

case class RawLambdaTau(params: List[Variable], body: SimpleTyped) {
  override def toString: String = s"T[λ${params.map(_.toString).mkString}.$body]"

  def toLambdaTau: LambdaTau = (params, body) match {
    case (p :: Nil, b: Variable) => SimpleLambdaTau(p, b)
    case (p :: Nil, b) => CurriedRawLambdaTau(p, b)
    case (ps, b) => UncurriedRawLambdaTau(ps.init, ps.last, b)
    case _ =>
      println((params, body))
      ???
  }
}

case class SimpleLambdaTau(p: Variable, b: Variable) extends LambdaTau {
  override def toString: String = s"T[λ$p.$b]"

  override def convert: Convertible = if (p == b) I else K(Some(b.toTau))
}

case class CurriedRawLambdaTau(param: Variable, body: SimpleTyped) extends Tau with LambdaTau {
  override def toString: String = s"T[λ$param.$body]"

  override def convert: Convertible = body match {
    case bs: Block => S(Some((CurriedRawLambdaTau(param, bs.init), Some(CurriedRawLambdaTau(param, bs.last)))))
    case l: Lambda => CurriedLambdaTau(param, l.toTau)
    case v: Variable if param == v => I
    case v: Variable => K(Some(v.toTau))
    case _ =>
      println(body)
      ???
  }
}

case class CurriedLambdaTau(param: Variable, body: Convertible) extends LambdaTau {
  override def toString: String = s"T[λ$param.$body]"

  override def convert: Convertible =
    if (body.isConvertible) this.copy(body = body.convert)
    else body match {
      case Constant(c) if param.value == c => I
      case Constant(c) => K(Some(Tau(Variable(c))))
      case S(Some((p, Some(q)))) =>
        S(Some((CurriedLambdaTau(param, S(Some((p, None)))), Some(CurriedLambdaTau(param, q)))))
      case S(Some((p, None))) =>
        S(Some((CurriedLambdaTau(param, S(None)), Some(CurriedLambdaTau(param, p)))))
      case s: S => K(Some(Tau(s)))
      case K(Some(p)) =>
        S(Some((CurriedLambdaTau(param, K(None)), Some(CurriedLambdaTau(param, p)))))
      case k: K => K(Some(Tau(k)))
      case I => K(Some(ITau))
      case _ =>
        println(body)
        ???
    }
}

case class UncurriedRawLambdaTau(init: List[Variable], last: Variable, body: SimpleTyped) extends LambdaTau {
  override def toString: String = s"T[λ${init.map(_.toString).mkString}$last.$body]"

  private def newBody: Lambda = typed.Lambda(List(last), body)

  override def convert: Convertible =
    if (init.length == 1) CurriedRawLambdaTau(init.head, newBody)
    else UncurriedLambdaTau(init.init, init.last, newBody.toTau)
}

case class UncurriedLambdaTau(init: List[Variable], last: Variable, body: Tau) extends LambdaTau {
  override def toString: String = s"T[λ${init.map(_.toString).mkString}$last.$body]"

  private def newBody: Tau = CurriedLambdaTau(last, body)

  override def convert: Convertible =
    if (init.length == 1) CurriedLambdaTau(init.head, newBody)
    else UncurriedLambdaTau(init.init, init.last, newBody)
}

case class STau(s: S) extends Tau {
  override def toString: String = s"T[$s]"

  override def convert: Convertible = s

  override def isConvertible: Boolean = true
}

case class KTau(k: K) extends Tau {
  override def toString: String = s"T[$k]"

  override def convert: Convertible = k

  override def isConvertible: Boolean = true
}

case object ITau extends Tau {
  override def toString: String = s"T[I]"

  override def convert: Convertible = I

  override def isConvertible: Boolean = true
}

/**
  * 変換終了状態の定数
  *
  * @param v 定数文字
  */
case class Constant(v: Char) extends Tau {
  override def toString: String = v.toString

  override def convert: Constant = this

  override def isConvertible: Boolean = false
}

