package org.enso.compiler.test.semantic

import org.enso.compiler.core.IR
import org.enso.compiler.pass.resolve.TypeSignatures
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.Context
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames}
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

  sealed private trait Sig {
    def ->(that: Sig): Sig = that match {
      case Fn(args, r) => Fn(this :: args, r)
      case _           => Fn(List(this), that)
    }
  }
  private case class Name(name: String)               extends Sig
  private case class Fn(args: List[Sig], result: Sig) extends Sig
  private case class Unknown(ir: IR.Expression)       extends Sig

  implicit private def toName(str: String): Name = Name(str)

  private def simpl(expr: IR.Expression): Sig = expr match {
    case fn: IR.Type.Function => Fn(fn.args.map(simpl), simpl(fn.result))
    case n: IR.Name           => Name(n.name)
    case _                    => Unknown(expr)
  }

  private def getSignature(module: IR.Module, methodName: String): Sig = {
    val m = module.bindings.find {
      case m: IR.Module.Scope.Definition.Method =>
        m.methodName.name == methodName
      case _ => false
    }.get
    val sig = m.unsafeGetMetadata(TypeSignatures, "come on").signature
    simpl(sig)
  }

  "Type Signatures" should {
    "be parsed in a simple scenario" in {
      val code =
        """
          |foo : Text -> Number
          |foo a = 42""".stripMargin
      val module = code.preprocessModule
      getSignature(module, "foo") shouldEqual ("Text" -> "Number")
    }
  }

}
