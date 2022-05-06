package org.enso.compiler.test.semantic

import org.enso.compiler.core.IR
import org.enso.compiler.pass.resolve.TypeSignatures
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class TypeSignaturesTest extends AnyWordSpecLike with Matchers {
  private val ctx = new InterpreterContext()
  private val langCtx = ctx.ctx
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[Context]()

  private val Module = QualifiedName(List("Unnamed"), "Test")

  implicit private class PreprocessModule(code: String) {
    def preprocessModule: IR.Module = {
      val module = new runtime.Module(Module, null, code)
      langCtx.getCompiler.run(module)
      module.getIr
    }
  }

  def typeAs(sig: Sig): TypeMatcher = TypeMatcher(sig)

  case class TypeMatcher(sig: Sig) extends Matcher[IR.Expression] {
    private def findInequalityWitness(
      sig: Sig,
      expr: IR.Expression
    ): Option[(Sig, IR.Expression)] = (sig, expr) match {
      case (Name(n), t: IR.Name.Literal) if n == t.name => None
      case (Fn(args, res), t: IR.Type.Function) =>
        if (args.length != t.args.length) {
          return Some((sig, expr))
        }
        args
          .lazyZip(t.args)
          .flatMap(findInequalityWitness)
          .headOption
          .orElse(findInequalityWitness(res, t.result))
      case _ => Some((sig, expr))
    }

    override def apply(left: IR.Expression): MatchResult = {
      findInequalityWitness(sig, left) match {
        case Some((s, t)) =>
          MatchResult(
            matches = false,
            s"""
               |$left does not match $sig.
               |Analysis:
               |sub-expression $t did not match fragment $s.
               |""".stripMargin,
            "The type matched the matcher, but it should not."
          )
        case _ => MatchResult(matches = true, "", "")
      }
    }
  }

  sealed trait Sig {
    def ->(that: Sig): Sig = that match {
      case Fn(args, r) => Fn(this :: args, r)
      case _           => Fn(List(this), that)
    }
  }
  case class Name(name: String)               extends Sig
  case class Fn(args: List[Sig], result: Sig) extends Sig

  implicit private def toName(str: String): Name = Name(str)

  private def getSignature(
    module: IR.Module,
    methodName: String
  ): IR.Expression = {
    val m = module.bindings.find {
      case m: IR.Module.Scope.Definition.Method =>
        m.methodName.name == methodName
      case _ => false
    }.get
    m.unsafeGetMetadata(TypeSignatures, "come on").signature
  }

  "Type Signatures" should {
    "be parsed in a simple scenario" in {
      val code =
        """
          |foo : Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule
      getSignature(module, "foo") should typeAs("Text" -> "Number")
    }
  }

}
