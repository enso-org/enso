package org.enso.syntax.text

/** This object contains view patterns that allow matching on the parser AST for
  * more sophisticated constructs.
  *
  * These view patterns are implemented as custom unapply methods that only
  * return [[Some]] when more complex conditions are met.
  */

object View {
  object Assignment {
    val assignmentOpSym = AST.Ident.Opr("=")

    def unapply(ast: AST): Option[(AST, AST)] = {
      ast match {
        case AST.App.Infix.any(ast) => {
          val left  = ast.larg
          val op    = ast.opr
          val right = ast.rarg

          if (op == assignmentOpSym) {
            Some((left, right))
          } else {
            None
          }
        }
        case _ => None
      }
    }
  }
}
