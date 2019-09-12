package org.enso.syntax.text.ast.opr

object Prec {
  val hierarchy = List(
    List("=", "#="),
    List("->", "<-"),
    List("~>", "<~"),
    List("|"),
    List("&"),
    List("!", "?", "~"),
    List("<*", "<*>", "*>", "<$", "<$>", "$>", "<+", "<+>", "+>"),
    List("<", ">"),
    List(":", ","),
    List("+", "-"),
    List("*", "/", "\\", "%"),
    List("^"),
    List("."),
    List(" ")
  )

  val map: Map[String, Int] =
    hierarchy.zipWithIndex.flatMap {
      case (ops, prec) => ops.map(_ -> prec)
    }.toMap

  val default = map.getOrElse("^", 0)
}
