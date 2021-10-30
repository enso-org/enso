package org.enso.flexer.spec

import org.enso.flexer.Parser

import scala.annotation.{nowarn, unused}
import scala.reflect.macros.blackbox.Context

// FIXME: Needs to be refactored. Contains deprecated API usage
object Macro {

  def print(c: Context, msg: String) = c.echo(c.enclosingPosition, msg)

  def runRule(c: Context)(program: c.Tree): c.Tree = {
    import c.universe._
    val tree = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(This(TypeName(_)), name) =>
          super.transform(Ident(name))
        case node => super.transform(node)
      }
    }.transform(program)

    c.macroApplication match {
      case Apply(Select(lhs, _), _) => q"$lhs.run(${showCode(tree)})"
      case _                        => throw new Error("Unsupported shape")
    }
  }

  @nowarn("msg=parameter value evidence")
  def compileImpl[T: c.WeakTypeTag, P: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[P])(@unused ev: c.Expr[P <:< Parser[T]]): c.Expr[() => P] = {
    import c.universe._
    val tree   = p.tree
    val expr   = q"$tree"
    val parser = c.eval(c.Expr[Parser[T]](c.untypecheck(expr.duplicate)))
    val groups = q"..${parser.state.registry.map(_.generate(c))}"
    val tree2 = tree match {
      case Apply(Select(tree2 @ Select(_, _), _), _) => tree2
      case _ =>
        throw new Error(
          s""" ERROR: Wrong shape
             | Expected Apply(Select(Select(_, name), _), _), got:
             | ${showRaw(tree)}
             |""".stripMargin
        )
    }

    val addGroupDefs = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val exprs = q"..$groups;None".asInstanceOf[Block].stats
          Template(parents, self, body ++ exprs)
        case node => super.transform(node)
      }
    }

    val clsDef = c.parse(s"final class __Parser__ extends $tree2")
    val tgtDef = addGroupDefs.transform(clsDef)

    val finalCode = q"$tgtDef; () => { new __Parser__() }"

    c.Expr[() => P](finalCode)
  }
}
