package org.enso.compiler.test.codegen

import org.enso.compiler.codegen.AstView
import org.enso.compiler.test.CompilerTest
import org.enso.data.List1
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Module
import org.scalatest.Inside
import org.scalatest.matchers.{BeMatcher, MatchResult}

class MatchersTest extends CompilerTest with Inside {
  implicit class ToSimpleAST(source: String) {

    /** Produces the [[AST]] representation of [[source]] and strips the outer Module.
      *
      * @return [[source]] as an AST expression
      */
    def toSimpleAst: AST = {
      val ast = source.toAst

      inside(ast) { case Module(List1(line, _)) =>
        line.elem.get
      }
    }
  }

  class GroupMatcher extends BeMatcher[AST] {
    def apply(left: AST) =
      MatchResult(
        AST.Group.unapply(left).flatten.isDefined,
        left.toString + " was not a group",
        left.toString + " was a group"
      )
  }

  val group = new GroupMatcher

  "Parensed" should {
    "match a single paren" in {
      inside("(x)".toSimpleAst) { case AstView.Parensed(_) => }
    }

    "not match an expression without parentheses" in {
      "x".toSimpleAst match {
        case AstView.Parensed(_) => fail("should not match")
        case _                   =>
      }
    }

    "peel-off exactly one paren" in {
      val twoParens = "((x))".toSimpleAst
      inside(twoParens) { case AstView.Parensed(oneParen) =>
        inside(oneParen) { case AstView.Parensed(zeroParens) =>
          zeroParens should not be group
        }
      }
    }
  }

  "MaybeParensed" should {
    "match a single paren" in {
      inside("(x)".toSimpleAst) { case AstView.MaybeParensed(_) => }
    }

    "also match an expression without parens" in {
      inside("x".toSimpleAst) { case AstView.MaybeParensed(_) => }
    }

    "peel-off at-most one paren" in {
      val twoParens = "((x))".toSimpleAst
      inside(twoParens) { case AstView.MaybeParensed(oneParen) =>
        oneParen shouldBe group
        inside(oneParen) { case AstView.MaybeParensed(zeroParens) =>
          zeroParens should not be group
        }
      }
    }
  }

  "ManyParensed" should {
    "match a single paren" in {
      inside("(x)".toSimpleAst) { case AstView.ManyParensed(_) => }
    }

    "not match an expression without parentheses" in {
      "x".toSimpleAst match {
        case AstView.ManyParensed(_) => fail("should not match")
        case _                       =>
      }
    }

    "peel-off all parens" in {
      val twoParens = "((x))".toSimpleAst
      inside(twoParens) { case AstView.ManyParensed(zeroParens) =>
        zeroParens should not be group
      }
    }
  }

  "MaybeManyParensed" should {
    "match a single paren" in {
      inside("(x)".toSimpleAst) { case AstView.MaybeManyParensed(_) => }
    }

    "also match an expression without parens" in {
      inside("x".toSimpleAst) { case AstView.MaybeManyParensed(_) => }
    }

    "peel-off all parens" in {
      val twoParens = "((x))".toSimpleAst
      inside(twoParens) { case AstView.MaybeManyParensed(zeroParens) =>
        zeroParens.shape should not be group
      }
    }
  }
}
