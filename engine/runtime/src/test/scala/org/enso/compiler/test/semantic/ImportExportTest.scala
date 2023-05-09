package org.enso.compiler.test.semantic

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames, RuntimeOptions}
import org.graalvm.polyglot.{Context, Engine, Value}
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.ByteArrayOutputStream
import java.nio.file.Paths

/**
 * Tests a single package with multiple modules for import/export resolution.
 * Checks whether the exported symbols and defined entities metadata of the modules
 * are correct.
 */
class ImportExportTest extends AnyWordSpecLike with Matchers with BeforeAndAfter {
  private val out = new ByteArrayOutputStream()

  private val engine = Engine
    .newBuilder("enso")
    .allowExperimentalOptions(true)
    .build()

  private val ctx = Context
    .newBuilder(LanguageInfo.ID)
    .engine(engine)
    .allowExperimentalOptions(true)
    .allowAllAccess(true)
    .allowCreateThread(false)
    .out(out)
    .err(out)
    .option(RuntimeOptions.LOG_LEVEL, "WARNING")
    .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
    .logHandler(System.err)
    .option(
      RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
      Paths
        .get("../../test/micro-distribution/component")
        .toFile
        .getAbsolutePath
    )
    .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
    .build()

  private val langCtx: EnsoContext = ctx
    .getBindings(LanguageInfo.ID)
    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
    .asHostObject[EnsoContext]()

  private val namespace = "my_pkg"
  private val packageName = "My_Package"
  private val packageQualifiedName =
    QualifiedName.fromString(namespace + "." + packageName)

  langCtx.getPackageRepository.registerSyntheticPackage(namespace, packageName)

  implicit private class CreateModule(moduleCode: String) {
    def createModule(moduleName: QualifiedName): runtime.Module = {
      val module = new runtime.Module(moduleName, null, moduleCode)
      langCtx.getPackageRepository.registerModuleCreatedInRuntime(module)
      langCtx.getCompiler.run(module)
      module
    }
  }

  implicit private class UnwrapBindingMap(moduleIr: IR.Module) {
    def unwrapBindingMap: BindingsMap = {
      moduleIr.unsafeGetMetadata(BindingAnalysis, "Should be present")
    }
  }

  // TODO: Add method to execute module
  private def executeModule(module: runtime.Module): Value = {
    ctx.eval(module.getSource)
    module.getSource
  }


  before {
    ctx.enter()
  }

  after {
    ctx.leave()
    out.reset()
  }

  "Import/export resolution with just two modules in one library" should {
    "resolve one import symbol from a module" in {
      val moduleCode =
        """
          |type Other_Module_Type
          |    Constructor
          |""".stripMargin
      val moduleIr = moduleCode.createModule(
        packageQualifiedName.createChild("Other_Module")
      ).getIr
      moduleIr.unwrapBindingMap.definedEntities.size shouldEqual 1
      moduleIr.unwrapBindingMap.definedEntities.head.name shouldEqual "Other_Module_Type"
      val otherTypeDefinedEntity = moduleIr.unwrapBindingMap.definedEntities.head.asInstanceOf[BindingsMap.Type]
      otherTypeDefinedEntity.members.size shouldEqual 1
      otherTypeDefinedEntity.members.head.name shouldEqual "Constructor"

      val mainCode =
        s"""
           |from $namespace.$packageName.Other_Module import Other_Module_Type
           |
           |main = Other_Module_Type.Constructor
           |""".stripMargin
      val mainIr = mainCode.createModule(packageQualifiedName.createChild("Main")).getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head match {
        case importErr: IR.Error.ImportExport =>
          fail(s"Import should be resolved, but instead produced IR.Error.ImportExport with ${importErr.reason.message}")
        case _ => ()
      }
      val mainBindingsMap = mainIr.unwrapBindingMap
      mainBindingsMap.resolvedImports.size shouldEqual 2
      mainBindingsMap.resolvedImports(0).target.isInstanceOf[BindingsMap.ResolvedModule] shouldBe true
      mainBindingsMap.resolvedImports(1).target.asInstanceOf[BindingsMap.ResolvedType].tp shouldEqual otherTypeDefinedEntity
    }

    "resolve two types from a module" in {
      val moduleIr =
        """
          |type Other_Module_Type
          |type Another_Type
          |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Other_Module"))
          .getIr
      moduleIr.unwrapBindingMap.definedEntities.size shouldEqual 2
      moduleIr.unwrapBindingMap.definedEntities(0).name shouldEqual "Other_Module_Type"
      moduleIr.unwrapBindingMap.definedEntities(1).name shouldEqual "Another_Type"

      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module import Other_Module_Type, Another_Type
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.unwrapBindingMap.resolvedImports.size shouldEqual 3
      mainIr.unwrapBindingMap.resolvedImports(0).target.isInstanceOf[BindingsMap.ResolvedModule] shouldBe true
      mainIr.unwrapBindingMap.resolvedImports(1).target.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "Other_Module_Type"
      mainIr.unwrapBindingMap.resolvedImports(2).target.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "Another_Type"
    }

    "resolve a constructor of a type" in {
      """
        |type Other_Module_Type
        |    Constructor
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import Constructor
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      // TODO: The second resolved import should be ResolvedConstructor
      mainIr.unwrapBindingMap.resolvedImports.size shouldEqual 2
    }

    "result in error when importing mixture of existing and non-existing symbols" in {
      """
        |type Existing_Type
        |    Constructor
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module import Existing_Type, Non_Existing_Symbol
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
    }

    "[1] result in error when importing a method from type" in {
      """
        |type Other_Type
        |    method self = 42
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Type import method
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      // TODO: Specify the message
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
    }

    "resolve static method from a module" in {
      """
        |static_method =
        |    42
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      val bBindingMap =
        s"""
           |import $namespace.$packageName.A_Module.static_method
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("B_Module"))
          .getIr
          .unwrapBindingMap
      val mainBindingMap =
        s"""
           |from $namespace.$packageName.Other_Module import static_method
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
          .unwrapBindingMap

      mainBindingMap.resolvedImports.size shouldEqual 2
      mainBindingMap.resolvedImports(0).target.asInstanceOf[BindingsMap.ResolvedModule].module.getName.path.last shouldEqual "A_Module"
      mainBindingMap.resolvedImports(1).target.asInstanceOf[BindingsMap.ResolvedMethod].method.name shouldEqual "static_method"
      // TODO: The same should be true for bBindingMap
    }

    "??" in {
      """
        |type Other_Module_Type
        |static_method =
        |    42
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainBindingMap =
        s"""
           |from $namespace.$packageName.Other_Module import all
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
          .unwrapBindingMap
      mainBindingMap.resolvedImports.size shouldEqual 3
      mainBindingMap.resolvedImports(0).target.asInstanceOf[BindingsMap.ResolvedModule].module.getName.path.last shouldEqual "Other_Module"
      mainBindingMap.resolvedImports(1).target.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "Other_Module_Type"
      mainBindingMap.resolvedImports(2).target.asInstanceOf[BindingsMap.ResolvedMethod].method.name shouldEqual "static_method"
    }

    "result in an error when importing all from a type that has some methods" in {
      """
        |type Other_Module_Type
        |    Constructor
        |    method self = 42
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module import all
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      // TODO: Check for a message
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
    }

    "result in error when trying to import all from a non-type" in {
      """
        |static_method =
        |    42
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.static_method import all
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      // TODO: Check for a message
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
    }

    "resolve all constructors from a type" in {
      """
        |type Other_Module_Type
        |    Constructor_1
        |    Constructor_2 val1 val2
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainBindingMap =
        s"""
           |from $namespace.$packageName.A_Module import all
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
          .unwrapBindingMap
      mainBindingMap.resolvedImports.size shouldEqual 3
      mainBindingMap.resolvedImports(0).target.asInstanceOf[BindingsMap.ResolvedModule].module.unsafeAsModule().getName.path.last shouldEqual "Other_Module"
      mainBindingMap.resolvedImports(1).target.asInstanceOf[BindingsMap.ResolvedConstructor].cons.name shouldEqual "Constructor_1"
      mainBindingMap.resolvedImports(2).target.asInstanceOf[BindingsMap.ResolvedConstructor].cons.name shouldEqual "Constructor_2"
    }
  }


  "Transitive import/export resolution with multiple modules in one library" should {
    "not resolve symbol that is not explicitly exported" in {
      """
        |type A_Type
        |    a_method self = 1
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      val moduleB =
        s"""
           |import $namespace.$packageName.A_Module.A_Type
           |
           |type B_Type
           |    b_method self = 2
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("B_Module"))
      val mainModule =
        s"""
           |from $namespace.$packageName.B_Module import A_Type
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
      moduleB.getIr.unwrapBindingMap.resolvedImports.size shouldEqual 1
      moduleB.getIr.unwrapBindingMap.resolvedImports.head.target.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "A_Type"
      mainModule.getIr.imports.size shouldEqual 1
      mainModule.getIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
      mainModule.getIr.imports.head.asInstanceOf[IR.Error.ImportExport].reason.message should include("A_Type")
    }

    "resolve symbol exported from a different module" in {
      """
        |type A_Type
        |    a_method self = 1
        |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      val bModule = s"""
         |import $namespace.$packageName.A_Module.A_Type
         |export $namespace.$packageName.A_Module.A_Type
         |
         |type B_Type
         |    b_method self = 2
         |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
      val mainModule =
        s"""
           |from $namespace.$packageName.B_Module import A_Type
           |"""
          .stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
      mainModule.getIr.imports.size shouldEqual 1
      mainModule.getIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      mainModule.getIr.unwrapBindingMap.resolvedImports.size shouldEqual 2
      mainModule.getIr.unwrapBindingMap.resolvedImports(0).target.asInstanceOf[BindingsMap.ResolvedModule].module.unsafeAsModule() shouldEqual bModule
      mainModule.getIr.unwrapBindingMap.resolvedImports(1).target.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "A_Type"
    }
  }

  "export is not transitive" in {
    s"""
      |import $namespace.$packageName.A_Module.A_Type
      |export $namespace.$packageName.A_Module.A_Type
      |
      |type A_Type
      |    a_method self = 1
      |"""
      .stripMargin
      .createModule(packageQualifiedName.createChild("A_Module"))
    s"""
       |import $namespace.$packageName.A_Module.A_Type
       |
       |type B_Type
       |    b_method self = 2
       |"""
      .stripMargin
      .createModule(packageQualifiedName.createChild("B_Module"))
    val mainModule =
      s"""
         |from $namespace.$packageName.B_Module import A_Type
         |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Main"))

    mainModule.getIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
    mainModule.getIr.imports.head.asInstanceOf[IR.Error.ImportExport].reason.asInstanceOf[IR.Error.ImportExport.SymbolsDoNotExist].symbolNames shouldEqual List("A_Type")
  }

  "TODO: Transitive import?" in {
    s"""
       |type A_Type
       |"""
      .stripMargin
      .createModule(packageQualifiedName.createChild("A_Module"))
    s"""
       |import $namespace.$packageName.A_Module.A_Type
       |export $namespace.$packageName.A_Module.A_Type
       |"""
      .stripMargin
      .createModule(packageQualifiedName.createChild("B_Module"))
    val mainBindingMap =
      s"""
         |from $namespace.$packageName.B_Module import A_Type
         |"""
        .stripMargin
        .createModule(packageQualifiedName.createChild("Main"))
        .getIr
        .unwrapBindingMap
    mainBindingMap.resolvedImports.size shouldEqual 2
    mainBindingMap.resolvedImports.head.target.asInstanceOf[BindingsMap.ResolvedModule].module.unsafeAsModule().getName.path.last shouldEqual "B_Module"
    mainBindingMap.resolvedImports.head.target.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "A_Type"
  }
}
