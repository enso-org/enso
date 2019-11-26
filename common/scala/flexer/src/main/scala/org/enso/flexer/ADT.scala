package org.enso.flexer

import scala.reflect.macros.blackbox.Context

object ADT {
  def constructors[T]: Set[T] = macro constructorsImpl[T]

  def constructorsImpl[T: c.WeakTypeTag](c: Context): c.Expr[Set[T]] = {
    import c.universe._

    val subs = c.weakTypeTag[T].tpe.typeSymbol.asClass.knownDirectSubclasses.map {
      symbol =>
        q"${c.mirror.staticModule(symbol.fullName)}"
    }
    c.Expr[Set[T]](q"Set(..$subs)")
  }
}
