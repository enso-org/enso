package org.enso.syntax.text.ast.meta

import org.enso.data.{List1, Shifted}
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Macro.Definition
import org.enso.syntax.text.AST.{Opr, Var}
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.meta.Pattern.Match

import scala.annotation.{nowarn, tailrec}

/** It contains definitions of built-in macros, like if-then-else or (-). These
  * macros might get moved to stdlib in the future.
  */
@nowarn("cat=unused")
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

    val sequenceLiteral = {
      val items = Pattern.SepList(Pattern.ExprUntilOpr(","), Opr(",")).opt
      Definition(
        Opr("[") -> items,
        Opr("]")
      ) { ctx =>
        ctx.body match {
          case List(items, _) =>
            val realItems =
              items.body.toStream
                .map(_.wrapped)
                .filterNot(AST.Ident.Opr.unapply(_).contains(","))
            AST.SequenceLiteral(realItems)
          case _ => internalError
        }
      }
    }

    val typesetLiteral = {
      val expression = Pattern.Expr(allowBlocks = false).opt
      Definition(
        Opr("{") -> expression,
        Opr("}")
      ) { ctx =>
        ctx.body match {
          case List(seg1, _) =>
            val body = seg1.body.toStream.map(_.wrapped)
            body match {
              case List(expr) =>
                AST.TypesetLiteral(Some(expr))
              case _ => AST.TypesetLiteral(None)
            }
          case _ => internalError
        }
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
              val args = argsPat.toStream.map(_.wrapped)
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
        case _ => internalError
      }
    }

    val polyglotJavaImport = {
      val javaQualName =
        Pattern.SepList(Pattern.Cons().or(Pattern.Var()), AST.Opr("."))
      val rename = Var("as") :: Pattern.Cons()
      Definition(
        Var("polyglot") -> Pattern.fromAST(Var("java")),
        Var("import")   -> (javaQualName :: rename.opt)
      ) { ctx =>
        ctx.body match {
          case List(_, nameAndRename) =>
            val (nameMatch, renameMatch) =
              nameAndRename.body.asInstanceOf[Match.Seq[Shifted[AST]]].elem
            val nonDot: List[AST.Ident] =
              nameMatch.toStream.map(_.wrapped).collect {
                case AST.Ident.Var.any(v)  => v: AST.Ident
                case AST.Ident.Cons.any(c) => c: AST.Ident
              }
            val rename: Option[AST.Ident.Cons] = {
              renameMatch.toStream.map(_.wrapped).collectFirst {
                case AST.Ident.Cons.any(n) => n
              }
            }
            // The optional unwrap is safe by construction - the pattern
            // guarantees at least one Var or Cons in the match result.
            AST.JavaImport(
              List1.fromListOption(nonDot).getOrElse(internalError),
              rename
            )
          case _ => internalError
        }
      }
    }

    val if_then = Definition(
      Var("if")   -> Pattern.Expr(allowBlocks = false),
      Var("then") -> Pattern.Expr()
    ) { ctx =>
      ctx.body match {
        case List(s1, s2) =>
          (s1.body.toStream, s2.body.toStream) match {
            case (List(t1), List(t2)) =>
              AST.Mixfix(List1(s1.head, s2.head), List1(t1.wrapped, t2.wrapped))
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val if_then_else = Definition(
      Var("if")   -> Pattern.Expr(allowBlocks = false),
      Var("then") -> Pattern.Expr(),
      Var("else") -> Pattern.Expr()
    ) { ctx =>
      ctx.body match {
        case List(s1, s2, s3) =>
          (s1.body.toStream, s2.body.toStream, s3.body.toStream) match {
            case (List(t1), List(t2), List(t3)) =>
              AST.Mixfix(
                List1(s1.head, s2.head, s3.head),
                List1(t1.wrapped, t2.wrapped, t3.wrapped)
              )
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val case_of = Definition(
      Var("case") -> Pattern.Expr(allowBlocks = false),
      Var("of")   -> Pattern.Block()
    ) { ctx =>
      ctx.body match {
        case List(scrutineePart, branchesPart) =>
          (scrutineePart.body.toStream, branchesPart.body.toStream) match {
            case (List(scrutinee), List(branches)) =>
              AST.Mixfix(
                List1(scrutineePart.head, branchesPart.head),
                List1(scrutinee.wrapped, branches.wrapped)
              )
            case _ => internalError
          }
        case _ => internalError
      }
    }

    val nonSpacedExpr = Pattern.Any(Some(false)).many1.build

    // NOTE: The macro engine currently resolves ahead of all operators, meaning
    // that `->` doesn't obey the right precedence (e.g. with respect to `:`).
    val arrow = Definition(
      Some(nonSpacedExpr.or(Pattern.ExprUntilOpr("->"))),
      Opr("->") -> Pattern.NonSpacedExpr().or(Pattern.Expr())
    ) { ctx =>
      (ctx.prefix, ctx.body) match {
        case (Some(pfx), List(s1)) =>
          (pfx.toStream, s1.body.toStream) match {
            case (List(l), List(r)) =>
              AST.App.Infix(l.wrapped, Opr("->"), r.wrapped)
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
              def go(t: AST): AST =
                t match {
                  case AST.App.Prefix(_, arg)    => arg
                  case AST.App.Infix(self, _, _) => go(self)
                  case AST.Macro.Match.any(m)    => go(m.resolved.orNull)
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
      Var("freeze") -> (Pattern.Var() :: Pattern.Block())
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

    val (qualifiedImport, itemsImport, qualifiedExport, itemsExport) = {
      val qualNamePat = (Pattern.fromAST(Var("project")) | ((Pattern
        .Var() | Pattern.Cons()) :: Opr(".") :: Pattern.Cons())) :: (Opr(
        "."
      ) :: Pattern.SepList(Pattern.Cons(), Opr("."))).opt
      val rename = Var("as") :: Pattern.Cons()

      def extractRename(
        matched: Pattern.Match
      ): (List1[AST.Ident], Option[AST.Ident.Cons]) = {
        val (nameMatch, renameMatch) =
          matched.asInstanceOf[Match.Seq[Shifted[AST]]].elem
        val name: List1[AST.Ident] =
          List1(
            nameMatch.toStream
              .map(_.wrapped)
              .collect {
                case AST.Ident.Cons.any(n) => n
                case AST.Ident.Var.any(n)  => n
              }
          ).get
        val rename: Option[AST.Ident.Cons] = {
          renameMatch.toStream.map(_.wrapped).collectFirst {
            case AST.Ident.Cons.any(n) => n
          }
        }
        (name, rename)
      }

      val itemsImport = {
        val all: Pattern = Var("all")
        val items        = Pattern.SepList(Pattern.Cons(), Opr(","))
        val hiding       = Var("hiding") :: items

        Definition(
          Var("from")   -> (qualNamePat :: rename.opt),
          Var("import") -> ((all :: hiding.opt) | items)
        ) { ctx =>
          ctx.body match {
            case List(imp, itemsMatch) =>
              val (name, rename) = extractRename(imp.body)
              val (hiding, items) = itemsMatch.body match {
                case Match.Or(_, Left(hidden)) =>
                  val hiddenItems = hidden.toStream
                    .map(_.wrapped)
                    .flatMap(AST.Ident.Cons.any.unapply)
                  (List1(hiddenItems), None)

                case Match.Or(_, Right(imported)) =>
                  val importedItems = imported.toStream
                    .map(_.wrapped)
                    .flatMap(AST.Ident.Cons.any.unapply)
                  (None, List1(importedItems))
                case _ => internalError
              }
              AST.Import(name, rename, true, items, hiding)
            case _ => internalError
          }

        }
      }

      val itemsExport = {
        val all: Pattern = Var("all")
        val items        = Pattern.SepList(Pattern.Cons() | Pattern.Var(), Opr(","))
        val hiding       = Var("hiding") :: items

        Definition(
          Var("from")   -> (qualNamePat :: rename.opt),
          Var("export") -> ((all :: hiding.opt) | items)
        ) { ctx =>
          ctx.body match {
            case List(imp, itemsMatch) =>
              val (name, rename) = extractRename(imp.body)
              val (hiding, items) = itemsMatch.body match {
                case Match.Or(_, Left(hidden)) =>
                  val hiddenItems = hidden.toStream
                    .map(_.wrapped)
                    .drop(2)
                    .collect {
                      case AST.Ident.Var.any(v)  => v: AST.Ident
                      case AST.Ident.Cons.any(c) => c: AST.Ident
                    }
                  (List1(hiddenItems), None)

                case Match.Or(_, Right(imported)) =>
                  val importedItems = imported.toStream
                    .map(_.wrapped)
                    .collect {
                      case AST.Ident.Var.any(v)  => v: AST.Ident
                      case AST.Ident.Cons.any(c) => c: AST.Ident
                    }
                  (None, List1(importedItems))
                case _ => internalError
              }
              AST.Export(name, rename, true, items, hiding)
            case _ => internalError
          }

        }
      }

      def qualifiedConstruct(
        constructName: String,
        constructFactory: (
          List1[AST.Ident],
          Option[AST.Ident.Cons],
          Boolean,
          Option[List1[AST.Ident.Cons]],
          Option[List1[AST.Ident.Cons]]
        ) => AST
      ): Definition = {
        Definition(
          Var(
            constructName
          ) -> (qualNamePat :: rename.opt)
        ) { ctx =>
          ctx.body match {
            case List(s1) =>
              val (name, rename) = extractRename(s1.body)
              constructFactory(name, rename, false, None, None)
            case _ => internalError
          }
        }
      }

      (
        qualifiedConstruct("import", AST.Import.apply),
        itemsImport,
        qualifiedConstruct("export", AST.Export.apply),
        itemsExport
      )
    }

    val privateDef = {
      Definition(Var("private") -> Pattern.Expr()) { ctx =>
        ctx.body match {
          case List(s1) =>
            s1.body.toStream match {
              case List(expr) =>
                AST.Modified("private", expr.wrapped)
              case _ => internalError
            }
          case _ => internalError
        }
      }
    }

    val unsafeDef = {
      Definition(Var("unsafe") -> Pattern.Expr()) { ctx =>
        ctx.body match {
          case List(s1) =>
            s1.body.toStream match {
              case List(expr) =>
                AST.Modified("unsafe", expr.wrapped)
              case _ => internalError
            }
          case _ => internalError
        }
      }
    }

    val fromKeyword = {
      Definition(Var("from") -> Pattern.Nothing()) { ctx =>
        ctx.body match {
          case List(_) => AST.Ident.Var("from")
          case _       => internalError
        }
      }
    }

    val unsafeKeyword = {
      Definition(Var("unsafe") -> Pattern.Nothing()) { ctx =>
        ctx.body match {
          case List(_) => AST.Ident.Var("unsafe")
          case _       => internalError
        }
      }
    }

    val privateKeyword = {
      Definition(Var("private") -> Pattern.Nothing()) { ctx =>
        ctx.body match {
          case List(_) => AST.Ident.Var("private")
          case _       => internalError
        }
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
      Definition(Opr("#") -> Pattern.Expr().tag("disable")) { _ => AST.Blank() }

    Registry(
      privateDef,
      unsafeDef,
      group,
      sequenceLiteral,
      typesetLiteral,
      case_of,
      if_then_else,
      if_then,
      polyglotJavaImport,
      itemsImport,
      qualifiedImport,
      itemsExport,
      qualifiedExport,
      defn,
      arrow,
      docComment,
      disableComment,
      skip,
      freeze,
      fromKeyword,
      unsafeKeyword,
      privateKeyword
    )
  }

}
