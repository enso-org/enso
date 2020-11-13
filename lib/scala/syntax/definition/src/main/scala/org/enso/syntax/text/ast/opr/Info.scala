package org.enso.syntax.text.ast.opr

object Info {
  val map: Map[String, (Int, Assoc)] = Prec.map.map {
    case (name, prec) => name -> ((prec, Assoc.of(name)))
  }
  def of(op: String) =
    map.getOrElse(op, (Prec.default, Assoc.of(op)))
}
