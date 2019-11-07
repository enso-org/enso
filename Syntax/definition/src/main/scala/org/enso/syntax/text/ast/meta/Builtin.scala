package org.enso.syntax.text.ast.meta

import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Macro.Definition
import org.enso.syntax.text.AST.Opr
import org.enso.syntax.text.AST.Var
import org.enso.syntax.text.ast.Repr

import scala.annotation.tailrec

/** It contains definitions of built-in macros, like if-then-else or (-). These
  * macros might get moved to stdlib in the future.
  */
object Builtin {

  val registry: Registry = {

    def internalError = throw new Error("Internal error")

    val group = Definition(Opr("(") -> Pattern.Expr().opt, Opr(")")) { ctx =>
      ctx.body match {
        case List(st1, _) =>
          st1.body.toStream match {
            case List()  => AST.Group()
            case List(t) => AST.Group(t)
            case _       => internalError
          }
        case _ => internalError
      }
    }

    val defn = Definition(Var("type") -> {
      val head = Pattern.Cons().or("missing name").tag("name")
      val args =
        Pattern.NonSpacedExpr_().tag("parameter").many.tag("parameters")
      val body = Pattern.Block().tag("body").opt
      head :: args :: body
    }) { ctx =>
      ctx.body match {
        case List(st1) =>
          import Pattern.Match._
          st1.body match {
            case Seq(_, (namePat, Seq(_, (argsPat, bodyPat)))) =>
              val args = argsPat.toStream.map(_.el)
              val body = bodyPat.toStream match {
                case List(Shifted(_, AST.Block.any(block))) => Some(block)
                case List()                                 => None
                case _                                      => internalError
              }
              namePat.toStream match {
                case List(Shifted(_, AST.Cons.any(n))) => AST.Def(n, args, body)
                case _                                 => internalError
              }
            case _ => internalError
          }
      }
    }

    val imp = Definition(
      Var("import") -> Pattern
        .SepList(Pattern.Cons(), AST.Opr("."): AST, "expected module name")
    ) { ctx =>
      ctx.body match {
        case List(s1) =>
          import Pattern.Match._
          s1.body match {
            case Seq(_, (headMatch, Many(_, tailMatch))) =>
              def unwrapSeg(lseg: Pattern.Match): AST.Cons =
                lseg.toStream match {
                  case List(Shifted(_, AST.Cons.any(t))) => t
                  case _                                 => internalError
                }

              val head = unwrapSeg(headMatch)
              val tail = tailMatch.map {
                case Seq(_, (Tok(_, Shifted(_, AST.Opr("."))), seg)) =>
                  unwrapSeg(seg)
                case _ => internalError
              }
              AST.Import(head, tail)
          }
        case _ => internalError
      }
    }

    val if_then = Definition(
      Var("if")   -> Pattern.Expr(),
      Var("then") -> Pattern.Expr()
    ) { ctx =>
      ctx.body match {
        case List(s1, s2) =>
          (s1.body.toStream, s2.body.toStream) match {
            case (List(t1), List(t2)) =>
              AST.Mixfix(List1(s1.head, s2.head), List1(t1.el, t2.el))
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val if_then_else = Definition(
      Var("if")   -> Pattern.Expr(),
      Var("then") -> Pattern.Expr(),
      Var("else") -> Pattern.Expr()
    ) { ctx =>
      ctx.body match {
        case List(s1, s2, s3) =>
          (s1.body.toStream, s2.body.toStream, s3.body.toStream) match {
            case (List(t1), List(t2), List(t3)) =>
              AST.Mixfix(
                List1(s1.head, s2.head, s3.head),
                List1(t1.el, t2.el, t3.el)
              )
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val nonSpacedExpr = Pattern.Any(Some(false)).many1.build

    val arrow = Definition(
      Some(nonSpacedExpr.or(Pattern.ExprUntilOpr("->"))),
      Opr("->") -> Pattern.NonSpacedExpr().or(Pattern.Expr())
    ) { ctx =>
      (ctx.prefix, ctx.body) match {
        case (Some(pfx), List(s1)) =>
          (pfx.toStream, s1.body.toStream) match {
            case (List(l), List(r)) => AST.App.Infix(l.el, Opr("->"), r.el)
            case _                  => internalError
          }
      }
    }

    val foreign = Definition(
      Var("foreign") -> (Pattern.Cons() :: Pattern.Block())
    ) { ctx =>
      ctx.body match {
        case List(s1) =>
          s1.body.toStream match {
            case List(langAST, Shifted(_, AST.Block.any(bodyAST))) =>
              val indent     = bodyAST.indent
              val lang       = langAST.el.show()
              val body       = bodyAST.show()
              val bodyLines  = body.split("\\r?\\n").toList.drop(1)
              val bodyLines2 = bodyLines.map(_.drop(indent))
              AST.Foreign(indent, lang, bodyLines2)
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val skip = Definition(
      Var("skip") -> Pattern.Expr()
    ) { ctx =>
      ctx.body match {
        case List(s1) =>
          s1.body.toStream match {
            case List(Shifted(_, body: AST)) =>
              @tailrec
              def go(t: AST): AST = t match {
                case AST.App.Prefix(_, arg)    => arg
                case AST.App.Infix(self, _, _) => go(self)
                case AST.Macro.Match.any(m)    => go(m.resolved)
                case AST.Group(None)           => t
                case AST.Group(Some(s))        => go(s)
                case _                         => t
              }

              go(body)
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val freeze = Definition(
      Var("freeze") -> Pattern.Expr()
    ) { ctx =>
      ctx.body match {
        case List(s1) =>
          s1.body.toStream match {
            case List(Shifted(_, _)) =>
              // TODO: Ability to do parsing here
              Var(s"Save to file using ${ctx.id}")
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val docComment = Definition(
      Opr("##") -> Pattern
        .Any()
        .many
        .fromBegin
        .or(Pattern.Any().but(Pattern.Block()).many)
        .tag("comment")
    ) { ctx =>
      ctx.body match {
        case List(s1) =>
          val stream = s1.body.toStream
          val indent = 2
          val text   = Repr(stream).build()
          val lines  = text.split("\n").toList
          val lines2 = lines.head :: lines.tail.map(_.drop(indent))
          AST.Comment(lines2)
        case _ => internalError
      }
    }

    // TODO
    // We may want to better represent empty AST. Moreover, there should be a
    // way to generate multiple top-level entities from macros (like multiple
    // atom definitions). One of the solutions to consider is to make AST
    // instance of Monoid, add a `Nothing` node, and replace all lines in a
    // block with a `Seq` node. This would allow us here to return `Nothing`,
    // and also return many top-level defs connected with a `Seq`.
    val disableComment =
      Definition(Opr("#") -> Pattern.Expr().tag("disable")) { _ =>
        AST.Blank()
      }

    Registry(
      group,
      if_then,
      if_then_else,
      imp,
      defn,
      arrow,
      foreign,
      docComment,
      disableComment,
      skip,
      freeze
    )
  }

}
