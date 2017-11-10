package ski

trait Convertible extends Typed {
  /**
    * 変換処理を1度実行する
    */
  def convert: Convertible

  /**
    * 変換処理を可能な限り実行する
    */
  def convertAll: Convertible = {
    def f(c: Convertible): Convertible =
      if (c.isConvertible) f(c.convert)
      else c

    f(this)
  }

  /**
    * 変換可能
    */
  def isConvertible: Boolean
}
