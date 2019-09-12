package org.enso.flexer.spec

import org.enso.flexer.Parser
import org.enso.lint.Unused

import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.universe

// FIXME: Needs to be refactored. Contains deprecated API usage
object Macro {

  def compileImpl[T: c.WeakTypeTag, P: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[P])(ev: c.Expr[P <:< Parser[T]]): c.Expr[() => P] = {
    import c.universe._
    Unused(ev)
    val tree   = p.tree
    val expr   = q"$tree"
    val parser = c.eval(c.Expr[Parser[T]](c.untypecheck(expr.duplicate)))
    val groups = c.internal
      .createImporter(universe)
      .importTree(universe.Block(parser.state.registry.map(_.generate()): _*))

    val (superClassName, tree2) = tree match {
      case Apply(Select(tree2 @ Select(_, name), _), _) => (name, tree2)
      case _ =>
        throw new Error(
          s""" ERROR: Wrong shape
             | Expected Apply(Select(Select(_, name), _), _), got:
             | ${showRaw(tree)}
             |""".stripMargin
        )
    }

    val groupsRebind = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(Ident(base), name) =>
          val base2 = if (base == superClassName) q"this" else Ident(base)
          super.transform(Select(base2, name))
        case node => super.transform(node)
      }
    }

    val reboundGroups = groupsRebind.transform(groups)

    val addGroupDefs = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val exprs = q"..$reboundGroups;None".asInstanceOf[Block].stats
          Template(parents, self, body ++ exprs)
        case node => super.transform(node)
      }
    }

    val clsDef = c.parse(s"final class __Parser__ extends $tree2")
    val tgtDef = addGroupDefs.transform(clsDef)
    c.Expr[() => P](q"$tgtDef; () => { new __Parser__ () }")
  }

}
