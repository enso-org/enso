package org.enso.syntax.text.ast

import org.enso.syntax.text.AST

object DSL {
  import AST.conversions._

  implicit final class ASTHelper(self: AST) {
    private def smartApp(off: Int, r: AST): AST = self match {
      case AST.App.Section.Left.any(t) =>
        AST.App.Infix(t.arg, t.off, t.opr, off, r)
      case _ => smartAppRaw(off, r)
    }

    private def smartAppRaw(off: Int, r: AST): AST = (self, r) match {
      case (l, AST.Opr.any(r)) => AST.App.Left(l, off, r)
      case (AST.Opr.any(l), r) => AST.App.Right(l, off, r)
      case (l, r)              => AST.App.Prefix(l, off, r)
    }

    def $(t: AST)    = smartApp(0, t)
    def $_(t: AST)   = smartApp(1, t)
    def $__(t: AST)  = smartApp(2, t)
    def $___(t: AST) = smartApp(3, t)

    def $$(t: AST)    = smartAppRaw(0, t)
    def $$_(t: AST)   = smartAppRaw(1, t)
    def $$__(t: AST)  = smartAppRaw(2, t)
    def $$___(t: AST) = smartAppRaw(3, t)
  }

  implicit final class StringHelpers(self: String) {
    def $(t: AST)    = (self: AST) $ t
    def $_(t: AST)   = (self: AST) $_ t
    def $__(t: AST)  = (self: AST) $__ t
    def $___(t: AST) = (self: AST) $___ t

    def $$(t: AST)    = (self: AST) $$ t
    def $$_(t: AST)   = (self: AST) $$_ t
    def $$__(t: AST)  = (self: AST) $$__ t
    def $$___(t: AST) = (self: AST) $$___ t
  }
}
