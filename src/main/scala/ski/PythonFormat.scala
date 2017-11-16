package ski

object PythonFormat {
  def toString(convertible: Convertible): Option[String] = convertible match {
    case S(None) => Some("S")
    case S(Some((p, None))) => PythonFormat.toString(p).map(q => s"S($q)")
    case S(Some((p1, Some(p2)))) =>
      for {
        q1 <- PythonFormat.toString(p1)
        q2 <- PythonFormat.toString(p2)
      } yield s"S($q1)($q2)"
    case K(None) => Some("K")
    case K(Some(p)) => PythonFormat.toString(p).map(q => s"K($q)")
    case I => Some("I")
    case _ => None
  }
}
