package org.enso.compiler.refactoring

import org.enso.compiler.core.IR
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class IRUtilsTest extends AnyWordSpecLike with Matchers with OptionValues {
  private val ctx = new InterpreterContext()
  private val langCtx = ctx.ctx
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  implicit private class PreprocessModule(code: String) {

    private val Module = QualifiedName(List("Unnamed"), "Test")

    def preprocessModule(name: QualifiedName): IR.Module = {
      val module = new runtime.Module(
        name,
        null,
        code.stripMargin.linesIterator.mkString("\n")
      )
      langCtx.getCompiler.run(module)
      module.getIr
    }

    def preprocessModule: IR.Module =
      preprocessModule(Module)

  }

  private def findUsagesOfLiteral(
    module: IR.Module,
    ir: IR
  ): Option[Set[IR.Name.Literal]] = {
    ir match {
      case literal: IR.Name.Literal =>
        IRUtils.findLocalUsages(module, literal)
      case _ =>
        fail(s"Trying to find literal usages of [${ir.getClass}]: [$ir]")
    }
  }

  private def findUsagesOfStaticMethod(
    moduleName: QualifiedName,
    module: IR.Module,
    ir: IR
  ): Option[Set[IR.Name.Literal]] = {
    ir match {
      case methodRef: IR.Name.MethodReference
          if methodRef.typePointer.isEmpty =>
        IRUtils.findModuleMethodUsages(
          moduleName,
          module,
          methodRef.methodName
        )
      case _ =>
        fail(s"Trying to find method usages of [${ir.getClass}]: [$ir]")
    }
  }

  "IRUtils" should {

    "find usages of a literal in expression" in {
      val uuid1 = new UUID(0, 1)
      val code =
        s"""main =
           |    operator1 = 41
           |    operator2 = operator1 + 1
           |    operator2
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 11}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfLiteral(module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a literal in a complex expression" in {
      val uuid1 = new UUID(0, 1)
      val code =
        s"""main =
           |    operator1 = 41
           |    operator2 = operator1 + operator1 + 1
           |    operator2
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 11}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfLiteral(module, operator1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a literal in a lambda" in {
      val uuid1 = new UUID(0, 1)
      val code =
        s"""main =
           |    operator1 = 41
           |    operator2 = "".map (x -> x + operator1)
           |    operator2
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 11}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfLiteral(module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static method call in main body" in {
      val uuid1      = new UUID(0, 1)
      val moduleName = QualifiedName(List("Unnamed"), "Test")
      val code =
        s"""function1 x = x + 1
           |
           |main =
           |    operator1 = 41
           |    operator2 = Test.function1 operator1
           |    operator2
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule(moduleName)
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfStaticMethod(moduleName, module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static call in lambda" in {
      val uuid1      = new UUID(0, 1)
      val moduleName = QualifiedName(List("Unnamed"), "Test")
      val code =
        s"""function1 x = x
           |
           |main =
           |    operator1 = 41
           |    operator2 = Test.function1 (x -> Test.function1 x)
           |    operator2
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule(moduleName)
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfStaticMethod(moduleName, module, operator1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static method call in presence of an instance method" in {
      val uuid1      = new UUID(0, 1)
      val moduleName = QualifiedName(List("Unnamed"), "Test")
      val code =
        s"""function1 x = x
           |
           |main =
           |    operator1 = 41
           |    operator2 = Test.function1 operator1
           |    operator3 = operator2.function1
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule(moduleName)
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfStaticMethod(moduleName, module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static method call in presence of a type method" in {
      val uuid1      = new UUID(0, 1)
      val moduleName = QualifiedName(List("Unnamed"), "Test")
      val code =
        s"""function1 x = x
           |
           |type A
           |    function1 x = x
           |
           |main =
           |    operator1 = 41
           |    operator2 = Test.function1 operator1
           |    operator3 = A.function1
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule(moduleName)
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfStaticMethod(moduleName, module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: IR.Name.Literal => succeed
        case ir                 => fail(s"Not a literal: $ir")
      }
    }

  }
}
