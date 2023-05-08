package org.enso.compiler.test.semantic

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.EnsoContext
import org.enso.interpreter.test.InterpreterContext
import org.enso.pkg.QualifiedName
import org.enso.interpreter.runtime
import org.enso.polyglot.{LanguageInfo, MethodNames, RuntimeOptions}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.annotation.unused

/**
 * Tests a single package with multiple modules for import/export resolution.
 * Checks whether the exported symbols and defined entities metadata of the modules
 * are correct.
 */
class ImportExportTest extends AnyWordSpecLike with Matchers {
  private val interpreterCtx = new InterpreterContext(
    contextModifiers = ctxBuilder => {
      ctxBuilder
        .option(RuntimeOptions.LOG_LEVEL, "WARNING")
    }
  )
  private val langCtx = interpreterCtx.ctx
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  private val namespace = "my_pkg"
  private val packageName = "My_Package"
  private val packageQualifiedName =
    QualifiedName.fromString(namespace + "." + packageName)

  langCtx.getPackageRepository.registerSyntheticPackage(namespace, packageName)

  implicit private class CreateMainModule(code: String) {
    def createMainModule: IR.Module = {
      val module = new runtime.Module(
        packageQualifiedName.createChild("Main"),
        null,
        code
      )
      langCtx.getPackageRepository.registerModuleCreatedInRuntime(module)
      langCtx.getCompiler.run(module)
      module.getIr
    }
  }

  implicit private class ProcessModule(moduleCode: String) {
    def processModule(moduleName: QualifiedName): IR.Module = {
      val module = new runtime.Module(moduleName, null, moduleCode)
      langCtx.getPackageRepository.registerModuleCreatedInRuntime(module)
      langCtx.getCompiler.run(module)
      module.getIr
    }
  }

  implicit private class UnwrapBindingMap(moduleIr: IR.Module) {
    def unwrapBindingMap = {
      moduleIr.unsafeGetMetadata(BindingAnalysis, "Should be present")
    }
  }

  "Import resolution" should {
    "bla" in {
      val moduleCode = """
        |type Other_Module_Type
        |    Constructor
        |""".stripMargin
      val otherModuleName = packageQualifiedName.createChild("Other_Module")
      val moduleIr = moduleCode.processModule(otherModuleName)
      moduleIr.unwrapBindingMap.definedEntities.size shouldEqual 1
      moduleIr.unwrapBindingMap.definedEntities.head.name shouldEqual "Other_Module_Type"
      val otherTypeDefinedEntity = moduleIr.unwrapBindingMap.definedEntities.head

      val mainCode =
        s"""
          |from $namespace.$packageName.Other_Module import Other_Module_Type
          |
          |main = Other_Module_Type.Constructor
          |""".stripMargin
      val mainIr = mainCode.processModule(packageQualifiedName.createChild("Main"))
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head match {
        case importErr: IR.Error.ImportExport =>
          fail(s"Import should be resolved, but instead produced IR.Error.ImportExport with ${importErr.reason.message}")
        case _ => ()
      }
      val mainBindingsMap = mainIr.unwrapBindingMap
      mainBindingsMap.resolvedImports.size shouldEqual 1
      mainBindingsMap.resolvedImports.head.target match {

      }
      mainIr shouldNot be(null)
    }
  }
}
