package org.enso.syntax.text.ast.opr

sealed trait Assoc
object Assoc {
  case object Left  extends Assoc
  case object Right extends Assoc

  private val applicativePat = "<?[+*$]>?".r

  private def isApplicative(s: String) = s match {
    case applicativePat() => s.length > 1
    case _                => false
  }

  private def charAssoc(c: Char) = c match {
    case '=' => -1
    case ',' => -1
    case '>' => -1
    case '<' => 1
    case _   => 0
  }

  def of(op: String): Assoc =
    if (isApplicative(op)) Assoc.Left
    else if (op.foldLeft(0)(_ + charAssoc(_)) >= 0) Assoc.Left
    else Assoc.Right
}
