package org.enso.compiler.test.refactoring

import org.enso.common.{LanguageInfo, MethodNames}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{Module, Name}
import org.enso.compiler.refactoring.IRUtils
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID

class IRUtilsTest extends AnyWordSpecLike with Matchers with OptionValues {
  private val ctx = new InterpreterContext()
  private val langCtx = ctx
    .ctx()
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  private val moduleName = QualifiedName.simpleName("Test")

  implicit private class PreprocessModule(code: String) {

    def preprocessModule(): Module = {
      val module = new runtime.Module(
        moduleName,
        null,
        code.stripMargin.linesIterator.mkString("\n")
      )
      langCtx.getCompiler.run(module.asCompilerModule())
      module.getIr
    }
  }

  private def findUsagesOfLiteral(
    module: Module,
    ir: IR
  ): Option[Set[Name.Literal]] = {
    ir match {
      case literal: Name.Literal =>
        IRUtils.findLocalUsages(module, literal)
      case _ =>
        fail(s"Trying to find literal usages of [${ir.getClass}]: [$ir]")
    }
  }

  private def findUsagesOfModuleMethod(
    moduleName: QualifiedName,
    module: Module,
    ir: IR
  ): Option[Set[Name.Literal]] = {
    ir match {
      case methodRef: Name.MethodReference if methodRef.typePointer.isEmpty =>
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

      val module    = code.preprocessModule()
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfLiteral(module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
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

      val module    = code.preprocessModule()
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfLiteral(module, operator1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
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

      val module    = code.preprocessModule()
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfLiteral(module, operator1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static method call in main body" in {
      val uuid1 = new UUID(0, 1)
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

      val module    = code.preprocessModule()
      val function1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfModuleMethod(moduleName, module, function1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

    "find usages of unqualified method call in main body" in {
      val uuid1 = new UUID(0, 1)
      val code =
        s"""function1 = ""
           |
           |main =
           |    operator1 = Test.function1
           |    operator2 = function1
           |    operator3 = operator2.function1
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule()
      val function1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfModuleMethod(moduleName, module, function1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a qualified static call in lambda" in {
      val uuid1 = new UUID(0, 1)
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

      val module    = code.preprocessModule()
      val function1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfModuleMethod(moduleName, module, function1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

    "find usages of unqualified static call in lambda" in {
      val uuid1 = new UUID(0, 1)
      val code =
        s"""function1 x = x
           |
           |main =
           |    operator2 = function1 (x -> function1 x)
           |    operator2
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule()
      val function1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfModuleMethod(moduleName, module, function1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static method call in presence of an instance method" in {
      val uuid1 = new UUID(0, 1)
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

      val module    = code.preprocessModule()
      val function1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfModuleMethod(moduleName, module, function1)

      usages.value.size shouldEqual 1
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

    "find usages of a static method call in presence of a type method" in {
      val uuid1 = new UUID(0, 1)
      val code =
        s"""function1 x = x
           |
           |type A
           |    function1 x = x
           |
           |main =
           |    operator1 = 41
           |    operator2 = Test.function1 operator1
           |    operator3 = function1 operator1
           |    operator4 = A.function1
           |
           |
           |#### METADATA ####
           |[[{"index": {"value": 0}, "size": {"value": 9}}, "$uuid1"]]
           |[]
           |""".stripMargin

      val module    = code.preprocessModule()
      val operator1 = IRUtils.findByExternalId(module, uuid1).get
      val usages    = findUsagesOfModuleMethod(moduleName, module, operator1)

      usages.value.size shouldEqual 2
      usages.value.foreach {
        case _: Name.Literal => succeed
        case ir              => fail(s"Not a literal: $ir")
      }
    }

  }
}
