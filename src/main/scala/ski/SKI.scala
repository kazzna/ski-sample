package ski

sealed trait SKI extends Convertible

/**
  * S combinator
  * S x y z = xz(yz)
  */
case class S(params: Option[(Convertible, Option[Convertible])]) extends SKI {
  override def toString: String = params.map { case (t, p2) => p2.map { t2 => s"(S $t $t2)" }.getOrElse(s"(S $t)") }.getOrElse("S")

  /**
    * 変換処理を1度実行する
    *
    * @return
    */
  override def convert: S = params match {
    case Some((t, Some(t2: Convertible))) if t2.isConvertible =>
      S(Some((t, Some(t2.convert))))
    case Some((t: Convertible, a)) if t.isConvertible => S(Some(t.convert, a))
    case _ => this
  }

  override def isConvertible: Boolean = params match {
    case Some((t1: Convertible, Some(t2: Convertible))) => t2.isConvertible || t1.isConvertible
    case Some((t1: Convertible, None)) => t1.isConvertible
    case _ => false
  }
}

case class K(param: Option[Convertible]) extends SKI {
  override def convert: Convertible =
    param.map(p => if (p.isConvertible) K(Some(p.convert)) else this).getOrElse(this)

  override def isConvertible: Boolean = param match {
    case Some(c) => c.isConvertible
    case _ => false
  }

  override def toString: String = param.map(p => s"(K $p)").getOrElse("K")
}

case object I extends SKI {
  override def convert: Convertible = this

  override def isConvertible: Boolean = false

  override def toString = "I"
}
