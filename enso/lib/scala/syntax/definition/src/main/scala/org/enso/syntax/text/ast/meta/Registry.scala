package org.enso.syntax.text.ast.meta

import org.enso.data
import org.enso.data.List1
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Macro.Definition

final case class Registry() {
  var tree: Registry.Tree = data.Tree()

  override def toString: String =
    tree.toString

  def insert(defn: Definition): Unit =
    tree += defn.path.toList -> defn

  def get(path: List1[AST]): Option[Definition] =
    tree.getValue(path.toList)
}

object Registry {
  type Tree = data.Tree[AST, Definition]
  def apply(defs: Definition*): Registry = {
    val registry = new Registry()
    defs.foreach(registry.insert)
    registry
  }
}
