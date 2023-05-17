package org.enso.compiler.test.semantic

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime
import org.enso.interpreter.runtime.EnsoContext
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{LanguageInfo, MethodNames, RuntimeOptions}
import org.graalvm.polyglot.{Context, Engine}
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.io.ByteArrayOutputStream
import java.nio.file.Paths

/** Tests a single package with multiple modules for import/export resolution.
  * Checks whether the exported symbols and defined entities metadata of the modules
  * are correct.
  */
class ImportExportTest
    extends AnyWordSpecLike
    with Matchers
    with BeforeAndAfter {
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

  private val namespace   = "my_pkg"
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

  before {
    ctx.enter()
  }

  after {
    ctx.leave()
    out.reset()
  }

  "Import resolution with just two modules" should {
    "resolve one import symbol from a module" in {
      val moduleCode =
        """
          |type Other_Module_Type
          |    Constructor
          |""".stripMargin
      val moduleIr = moduleCode
        .createModule(
          packageQualifiedName.createChild("Other_Module")
        )
        .getIr
      moduleIr.unwrapBindingMap.definedEntities.size shouldEqual 1
      moduleIr.unwrapBindingMap.definedEntities.head.name shouldEqual "Other_Module_Type"
      val otherTypeDefinedEntity =
        moduleIr.unwrapBindingMap.definedEntities.head
          .asInstanceOf[BindingsMap.Type]
      otherTypeDefinedEntity.members.size shouldEqual 1
      otherTypeDefinedEntity.members.head.name shouldEqual "Constructor"

      val mainCode =
        s"""
           |from $namespace.$packageName.Other_Module import Other_Module_Type
           |
           |main = Other_Module_Type.Constructor
           |""".stripMargin
      val mainIr =
        mainCode.createModule(packageQualifiedName.createChild("Main")).getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head match {
        case importErr: IR.Error.ImportExport =>
          fail(
            s"Import should be resolved, but instead produced IR.Error.ImportExport with ${importErr.reason.message}"
          )
        case _ => ()
      }
      val mainBindingsMap = mainIr.unwrapBindingMap
      mainBindingsMap.resolvedImports.size shouldEqual 2
      mainBindingsMap
        .resolvedImports(0)
        .target
        .isInstanceOf[BindingsMap.ResolvedModule] shouldBe true
      mainBindingsMap
        .resolvedImports(1)
        .target
        .asInstanceOf[BindingsMap.ResolvedType]
        .tp shouldEqual otherTypeDefinedEntity
    }

    "resolve two types from a module" in {
      val moduleIr =
        """
          |type Other_Module_Type
          |type Another_Type
          |""".stripMargin
          .createModule(packageQualifiedName.createChild("Other_Module"))
          .getIr
      moduleIr.unwrapBindingMap.definedEntities.size shouldEqual 2
      moduleIr.unwrapBindingMap
        .definedEntities(0)
        .name shouldEqual "Other_Module_Type"
      moduleIr.unwrapBindingMap
        .definedEntities(1)
        .name shouldEqual "Another_Type"

      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module import Other_Module_Type, Another_Type
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.unwrapBindingMap.resolvedImports.size shouldEqual 3
      mainIr.unwrapBindingMap
        .resolvedImports(0)
        .target
        .isInstanceOf[BindingsMap.ResolvedModule] shouldBe true
      mainIr.unwrapBindingMap
        .resolvedImports(1)
        .target
        .asInstanceOf[BindingsMap.ResolvedType]
        .tp
        .name shouldEqual "Other_Module_Type"
      mainIr.unwrapBindingMap
        .resolvedImports(2)
        .target
        .asInstanceOf[BindingsMap.ResolvedType]
        .tp
        .name shouldEqual "Another_Type"
    }

    "resolve a constructor of a type" in {
      """
        |type Other_Module_Type
        |    Constructor
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import Constructor
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      // TODO: The second resolved import should be ResolvedConstructor
      mainIr.unwrapBindingMap.resolvedImports.size shouldEqual 2
    }

    "result in error when importing mixture of existing and non-existing symbols" in {
      """
        |type Existing_Type
        |    Constructor
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module import Existing_Type, Non_Existing_Symbol
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
    }

    "result in error when importing a method from type" in {
      """
        |type Other_Type
        |    method self = 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Type import method
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head
        .asInstanceOf[IR.Error.ImportExport]
        .reason
        .asInstanceOf[
          IR.Error.ImportExport.NoSuchConstructor
        ] shouldEqual IR.Error.ImportExport
        .NoSuchConstructor("Other_Type", "method")
    }

    "resolve static method from a module" in {
      """
        |static_method =
        |    42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      val bIr =
        s"""
           |import $namespace.$packageName.A_Module.static_method
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("B_Module"))
          .getIr
      val mainIr =
        s"""
           |from $namespace.$packageName.A_Module import static_method
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr

      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      bIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val bBindingMap    = bIr.unwrapBindingMap
      mainBindingMap.resolvedImports.size shouldEqual 2
      mainBindingMap
        .resolvedImports(0)
        .target
        .asInstanceOf[BindingsMap.ResolvedModule]
        .module
        .getName
        .item shouldEqual "A_Module"
      mainBindingMap
        .resolvedImports(1)
        .target
        .asInstanceOf[BindingsMap.ResolvedMethod]
        .method
        .name shouldEqual "static_method"
      // In B_Module, we only have ResolvedMethod in the resolvedImports, there is no ResolvedModule
      // But that does not matter.
      bBindingMap.resolvedImports.size shouldEqual 1
      bBindingMap
        .resolvedImports(0)
        .target
        .asInstanceOf[BindingsMap.ResolvedMethod]
        .method
        .name shouldEqual "static_method"
    }

    "resolve types and static module methods when importing all from a module" in {
      """
        |type Other_Module_Type
        |static_method =
        |    42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainBindingMap =
        s"""
           |from $namespace.$packageName.Other_Module import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
          .unwrapBindingMap

      mainBindingMap.resolvedImports.filter(imp => {
        imp.target match {
          case BindingsMap.ResolvedType(_, tp)
              if tp.name == "Other_Module_Type" =>
            true
          case BindingsMap.ResolvedMethod(_, method)
              if method.name == "static_method" =>
            true
          case _ => false
        }
      }) should have size 2
    }

    "resolve only constructors when importing all symbols from a type (1)" in {
      """
        |type Other_Module_Type
        |    Constructor
        |    method self = 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainBindingMap =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
          .unwrapBindingMap

      mainBindingMap.resolvedImports.filter(imp => {
        imp.target match {
          case BindingsMap.ResolvedConstructor(_, ctor)
              if ctor.name == "Constructor" =>
            true
          case _ => false
        }
      }) should have size 1
    }

    "resolve only constructors when importing all symbols from a type (2)" in {
      """
        |type Other_Module_Type
        |    Constructor_1
        |    Constructor_2 val1 val2
        |    method self = 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr

      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      mainBindingMap.resolvedImports.filter(imp => {
        imp.target match {
          case BindingsMap.ResolvedConstructor(_, ctor)
              if ctor.name == "Constructor_1" || ctor.name == "Constructor_2" =>
            true
          case _ => false
        }
      }) should have size 2
    }

    "resolve all constructors from a type" in {
      """
        |type Other_Module_Type
        |    Constructor_1
        |    Constructor_2 val1 val2
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      mainBindingMap.resolvedImports.filter(imp => {
        imp.target match {
          case BindingsMap.ResolvedConstructor(_, ctor)
              if ctor.name == "Constructor_1" || ctor.name == "Constructor_2" =>
            true
          case _ => false
        }
      }) should have size 2
    }

    "resolve exactly two constructors from a type" in {
      """
        |type Other_Module_Type
        |    Constructor_1
        |    Constructor_2 val1 val2
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import Constructor_1, Constructor_2
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap        = mainIr.unwrapBindingMap
      val resolvedImportTargets = mainBindingMap.resolvedImports.map(_.target)
      resolvedImportTargets
        .collect { case rc: BindingsMap.ResolvedConstructor => rc }
        .map(_.cons.name) should contain only ("Constructor_1", "Constructor_2")
    }

    "result in error when trying to import mix of constructors and method from a type" in {
      """
        |type Other_Module_Type
        |    Constructor_1
        |    Constructor_2 val1 val2
        |    method self = 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Other_Module_Type import Constructor_1, method, Constructor_2
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
      mainIr.imports.head
        .asInstanceOf[IR.Error.ImportExport]
        .reason
        .asInstanceOf[IR.Error.ImportExport.NoSuchConstructor] shouldEqual
      IR.Error.ImportExport.NoSuchConstructor("Other_Module_Type", "method")
    }

    "result in error when trying to import all from a non-type" in {
      """
        |static_method =
        |    42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.static_method import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.head
        .asInstanceOf[IR.Error.ImportExport]
        .reason
        .asInstanceOf[IR.Error.ImportExport.TypeDoesNotExist]
        .typeName shouldEqual "static_method"
    }

    "result in error when trying to import anything from a non-existing symbol" in {
      """
        |# Left blank on purpose
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("Other_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.Other_Module.Non_Existing_Symbol import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.head
        .asInstanceOf[IR.Error.ImportExport]
        .reason
        .asInstanceOf[IR.Error.ImportExport.ModuleDoesNotExist]
        .name should include("Non_Existing_Symbol")
    }

    // TODO[pm]: Fix in https://github.com/enso-org/enso/issues/6669
    "resolve all symbols from transitively exported type" ignore {
      """
        |type A_Type
        |    A_Constructor
        |    a_method self = 1
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      s"""
         |import $namespace.$packageName.A_Module.A_Type
         |export $namespace.$packageName.A_Module.A_Type
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.B_Module.A_Type import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val resolvedImportTargets =
        mainBindingMap.resolvedImports.map(_.target)
      resolvedImportTargets
        .collect { case c: BindingsMap.ResolvedConstructor => c }
        .map(_.cons.name) should contain theSameElementsAs List("A_Constructor")
    }

    // TODO[pm]: Fix in https://github.com/enso-org/enso/issues/6669
    "resolve constructor from transitively exported type" ignore {
      """
        |type A_Type
        |    A_Constructor
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      s"""
         |import $namespace.$packageName.A_Module.A_Type
         |export $namespace.$packageName.A_Module.A_Type
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
      val mainIr =
        s"""
           |from $namespace.$packageName.B_Module.A_Type import A_Constructor
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val resolvedImportTargets =
        mainBindingMap.resolvedImports.map(_.target)
      resolvedImportTargets
        .collect { case c: BindingsMap.ResolvedConstructor => c }
        .map(_.cons.name) should contain theSameElementsAs List("A_Constructor")
    }

    "export is not transitive" in {
      s"""
         |import $namespace.$packageName.A_Module.A_Type
         |export $namespace.$packageName.A_Module.A_Type
         |
         |type A_Type
         |    a_method self = 1
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
      s"""
         |import $namespace.$packageName.A_Module.A_Type
         |
         |type B_Type
         |    b_method self = 2
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
      val mainModule =
        s"""
           |from $namespace.$packageName.B_Module import A_Type
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))

      mainModule.getIr.imports.head
        .isInstanceOf[IR.Error.ImportExport] shouldBe true
      mainModule.getIr.imports.head
        .asInstanceOf[IR.Error.ImportExport]
        .reason
        .asInstanceOf[IR.Error.ImportExport.SymbolsDoNotExist]
        .symbolNames shouldEqual List("A_Type")
    }
  }

  "Export resolution" should {
    "not export module when exporting a type (1)" in {
      """
        |type A_Type
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
        .getIr
        .unwrapBindingMap

      // B_Module is exporting just A_Type, not A_Module
      val bindingMap = s"""
         |import $namespace.$packageName.A_Module.A_Type
         |export $namespace.$packageName.A_Module.A_Type
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr
        .unwrapBindingMap

      val resolvedImportTargets =
        bindingMap.resolvedImports
          .map(_.target)
      val resolvedImportNames =
          resolvedImportTargets
          .map(_.qualifiedName.item)
      bindingMap.exportedSymbols.get("A_Module") shouldBe None
      bindingMap.exportedSymbols.get("A_Type") shouldNot be(None)
      resolvedImportNames should contain theSameElementsAs List("A_Type")
    }

    "not export module when exporting a type (2)" in {
      """
        |type A_Type
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
        .getIr
        .unwrapBindingMap

      // B_Module is exporting just A_Type, not A_Module
      val bindingMap =
        s"""
           |import $namespace.$packageName.A_Module
           |from $namespace.$packageName.A_Module export A_Type
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("B_Module"))
          .getIr
          .unwrapBindingMap

      val resolvedImportNames =
        bindingMap.resolvedImports
          .map(_.target)
          .map(_.qualifiedName.item)
      bindingMap.exportedSymbols.get("A_Module") should be(None)
      bindingMap.exportedSymbols.get("A_Type") shouldNot be(None)
      resolvedImportNames should contain theSameElementsAs List("A_Module")
    }

    "not export module when exporting a type with same name" in {
      """
        |type A_Module
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      val bindingMap =
        s"""
           |import $namespace.$packageName.A_Module
           |from $namespace.$packageName.A_Module export A_Module
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("B_Module"))
          .getIr
          .unwrapBindingMap

      val exportedModule = bindingMap.exportedSymbols("A_Module")
      exportedModule.size shouldEqual 1
      exportedModule.head.asInstanceOf[BindingsMap.ResolvedType].tp.name shouldEqual "A_Module"
    }

    "import re-exported constructor" in {
      val aBindingMap = s"""
        |from $namespace.$packageName.A_Module.A_Type import A_Constructor
        |from $namespace.$packageName.A_Module.A_Type export A_Constructor
        |
        |type A_Type
        |    A_Constructor
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))
        .getIr
        .unwrapBindingMap

      val bBindingMap = s"""
         |import $namespace.$packageName.A_Module
         |from $namespace.$packageName.A_Module export A_Constructor
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr
        .unwrapBindingMap

      val cBindingMap = s"""
         |from $namespace.$packageName.B_Module import A_Constructor
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("C_Module"))
        .getIr
        .unwrapBindingMap

      val dBindingMap =
        s"""
           |from $namespace.$packageName.B_Module import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("D_Module"))
          .getIr
          .unwrapBindingMap

      aBindingMap.exportedSymbols.get("A_Constructor") shouldNot be(None)

      val bResolvedImportNames = bBindingMap.resolvedImports
        .map(_.target)
        .map(_.qualifiedName.item)
      bResolvedImportNames should contain theSameElementsAs List("A_Module")
      bBindingMap.exportedSymbols.size shouldEqual 1
      bBindingMap.exportedSymbols("A_Constructor").size shouldEqual 1
      bBindingMap.exportedSymbols("A_Constructor").head.asInstanceOf[BindingsMap.ResolvedConstructor].cons.name shouldEqual "A_Constructor"

      val cResolvedImportNames = cBindingMap.resolvedImports
        .map(_.target)
        .map(_.qualifiedName.item)
      cResolvedImportNames should contain theSameElementsAs List("B_Module", "A_Constructor")
      cBindingMap.exportedSymbols shouldBe empty

      val dResolvedImportNames = dBindingMap.resolvedImports
        .map(_.target)
        .map(_.qualifiedName.item)
      dResolvedImportNames should contain theSameElementsAs List("B_Module", "A_Constructor")
      dBindingMap.exportedSymbols shouldBe empty
    }

    "findExportedSymbolsFor method works for all import target types" in {
      s"""
         |type A_Type
         |    A_Constructor
         |    instance_method self = 42
         |
         |static_method =
         |    local_var = 42
         |    local_var
         |
         |# Is not really a variable - it is a method returning a constant, so
         |# it is also considered a static module method
         |glob_var = 42
         |
         |# This is also a static method
         |foreign js js_function = \"\"\"
         |    return 42
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      val bindingMap = s"""
         |from $namespace.$packageName.A_Module import all
         |from $namespace.$packageName.A_Module.A_Type import A_Constructor
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr
        .unwrapBindingMap

      val resolvedImportTargets =
        bindingMap.resolvedImports.map(_.target)

      val resolvedAType = resolvedImportTargets.collect { case t: BindingsMap.ResolvedType => t }.head
      resolvedAType.tp.name shouldEqual "A_Type"
      resolvedAType.findExportedSymbolsFor("A_Type") should be(Nil)
      resolvedAType.findExportedSymbolsFor("A_Constructor").head.qualifiedName.item shouldEqual "A_Constructor"
      resolvedAType.findExportedSymbolsFor("NonExistingSymbol") should be(Nil)

      val resolvedStaticMethod = resolvedImportTargets.collect { case m: BindingsMap.ResolvedMethod => m }.head
      resolvedStaticMethod.method.name shouldEqual "static_method"
      resolvedStaticMethod.findExportedSymbolsFor("static_method") should be(Nil)
      resolvedStaticMethod.findExportedSymbolsFor("NonExistingSymbol") should be(Nil)

      val resolvedConstructor = resolvedImportTargets.collect { case c: BindingsMap.ResolvedConstructor => c }.head
      resolvedConstructor.cons.name shouldEqual "A_Constructor"
      resolvedConstructor.findExportedSymbolsFor("A_Constructor") should be(Nil)
      resolvedConstructor.findExportedSymbolsFor("NonExistingSymbol") should be(Nil)
    }

    "findExportedSymbolsFor method works for ResolvedModule" in {
      s"""
         |type A_Type
         |    A_Constructor
         |    instance_method self = 42
         |
         |static_method =
         |    local_var = 42
         |    local_var
         |
         |# Is not really a variable - it is a method returning a constant, so
         |# it is also considered a static module method
         |glob_var = 42
         |
         |# This is also a static method
         |foreign js js_function = \"\"\"
         |    return 42
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      val bindingMap =
        s"""
           |import $namespace.$packageName.A_Module
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("B_Module"))
          .getIr
          .unwrapBindingMap

      val resolvedImportTargets =
        bindingMap.resolvedImports.map(_.target)

      val resolvedModule = resolvedImportTargets.collect { case m: BindingsMap.ResolvedModule => m }.head
      resolvedModule.module.getName.item shouldEqual "A_Module"
      resolvedModule.findExportedSymbolsFor("static_method").head.asInstanceOf[BindingsMap.ResolvedMethod].method.name shouldEqual "static_method"
      resolvedModule.findExportedSymbolsFor("glob_var").head.asInstanceOf[BindingsMap.ResolvedMethod].method.name shouldEqual "glob_var"
      resolvedModule.findExportedSymbolsFor("js_function").head.asInstanceOf[BindingsMap.ResolvedMethod].method.name shouldEqual "js_function"
      resolvedModule.findExportedSymbolsFor("NonExistingSymbol") should be(Nil)
      resolvedModule.findExportedSymbolsFor("instance_method") should be(Nil)
      resolvedModule.findExportedSymbolsFor("A_Constructor") should be(Nil)
    }
  }

  "Import resolution for three modules" should {
    "resolve all imported symbols in B_Module from A_Module" in {
      s"""
         |type A_Type
         |    A_Constructor
         |    instance_method self = 42
         |
         |static_method =
         |    local_var = 42
         |    local_var
         |
         |# Is not really a variable - it is a method returning a constant, so
         |# it is also considered a static module method
         |glob_var = 42
         |
         |# This is also a static method
         |foreign js js_function = \"\"\"
         |    return 42
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      val mainIr =
        s"""
           |from $namespace.$packageName.A_Module import all
           |from $namespace.$packageName.A_Module export static_method
           |
           |type B_Type
           |    B_Constructor val
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main_Module"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val resolvedImportTargets =
        mainBindingMap.resolvedImports.map(_.target)
      resolvedImportTargets
        .collect { case rt: BindingsMap.ResolvedType => rt }
        .map(_.tp.name) shouldEqual Iterable("A_Type")
      resolvedImportTargets
        .collect { case rc: BindingsMap.ResolvedConstructor =>
          rc
        } shouldBe empty
      resolvedImportTargets
        .collect { case meth: BindingsMap.ResolvedMethod => meth }
        .map(
          _.method.name
        ) should contain allOf ("static_method", "glob_var", "js_function")
      resolvedImportTargets
        .collect { case rm: BindingsMap.ResolvedModule => rm }
        .map(_.module.getName.item) shouldEqual Iterable("A_Module")
    }

    "resolve foreign static module method" in {
      s"""
         |type A_Type
         |    A_Constructor
         |    instance_method self = 42
         |
         |static_method =
         |    local_var = 42
         |    local_var
         |
         |# Is not really a variable - it is a method returning a constant, so
         |# it is also considered a static module method
         |glob_var = 42
         |
         |# This is also a static method
         |foreign js js_function = \"\"\"
         |    return 42
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      val mainBindingMap =
        s"""
           |from $namespace.$packageName.A_Module import js_function
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
          .unwrapBindingMap
      val resolvedImportTargets =
        mainBindingMap.resolvedImports.map(_.target)
      resolvedImportTargets
        .collect { case meth: BindingsMap.ResolvedMethod => meth }
        .map(_.method.name) shouldEqual Iterable("js_function")
      resolvedImportTargets
        .collect { case mod: BindingsMap.ResolvedModule => mod }
        .map(_.module.getName.item) shouldEqual Iterable("A_Module")
    }

    "not resolve symbol that is not explicitly exported" in {
      """
        |type A_Type
        |    A_Constructor
        |    instance_method self = 42
        |
        |static_method =
        |    local_var = 42
        |    local_var
        |
        |# Is not really a variable - it is a method returning a constant, so
        |# it is also considered a static module method
        |glob_var = 42
        |
        |# This is also a static method
        |foreign js js_function = \"\"\"
        |    return 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      s"""
         |from $namespace.$packageName.A_Module import all
         |from $namespace.$packageName.A_Module export static_method
         |
         |type B_Type
         |    B_Constructor val
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr

      val mainIr =
        s"""
           |from $namespace.$packageName.B_Module import A_Type
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe true
      mainIr.imports.head
        .asInstanceOf[IR.Error.ImportExport]
        .reason
        .message should include("A_Type")
    }

    "resolve all symbols (types and static module methods) from the module" in {
      s"""
         |type A_Type
         |    A_Constructor
         |    instance_method self = 42
         |
         |static_method =
         |    local_var = 42
         |    local_var
         |
         |# Is not really a variable - it is a method returning a constant, so
         |# it is also considered a static module method
         |glob_var = 42
         |
         |# This is also a static method
         |foreign js js_function = \"\"\"
         |    return 42
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      s"""
         |from $namespace.$packageName.A_Module import all
         |from $namespace.$packageName.A_Module export static_method
         |
         |type B_Type
         |    B_Constructor val
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr

      val mainIr =
        s"""
           |from $namespace.$packageName.A_Module import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val resolvedImportSymbols: List[String] =
        mainBindingMap.resolvedImports.map(_.target.qualifiedName.item)
      resolvedImportSymbols should contain theSameElementsAs List(
        "A_Module",
        "A_Type",
        "static_method",
        "glob_var",
        "js_function"
      )
    }

    "resolve re-exported symbol" in {
      """
        |type A_Type
        |    A_Constructor
        |    instance_method self = 42
        |
        |static_method =
        |    local_var = 42
        |    local_var
        |
        |# Is not really a variable - it is a method returning a constant, so
        |# it is also considered a static module method
        |glob_var = 42
        |
        |# This is also a static method
        |foreign js js_function = \"\"\"
        |    return 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      s"""
         |from $namespace.$packageName.A_Module import all
         |from $namespace.$packageName.A_Module export static_method
         |
         |type B_Type
         |    B_Constructor val
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr

      val mainIr =
        s"""
           |from $namespace.$packageName.B_Module import static_method
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val resolvedImportSymbols: List[String] =
        mainBindingMap.resolvedImports.map(_.target.qualifiedName.item)
      resolvedImportSymbols should contain only ("B_Module", "static_method")
    }

    "resolve re-exported symbol along with all other symbols" in {
      """
        |type A_Type
        |    A_Constructor
        |    instance_method self = 42
        |
        |static_method =
        |    local_var = 42
        |    local_var
        |
        |# Is not really a variable - it is a method returning a constant, so
        |# it is also considered a static module method
        |glob_var = 42
        |
        |# This is also a static method
        |foreign js js_function = \"\"\"
        |    return 42
        |""".stripMargin
        .createModule(packageQualifiedName.createChild("A_Module"))

      s"""
         |from $namespace.$packageName.A_Module import all
         |from $namespace.$packageName.A_Module export static_method
         |
         |type B_Type
         |    B_Constructor val
         |""".stripMargin
        .createModule(packageQualifiedName.createChild("B_Module"))
        .getIr

      val mainIr =
        s"""
           |from $namespace.$packageName.B_Module import all
           |""".stripMargin
          .createModule(packageQualifiedName.createChild("Main"))
          .getIr
      mainIr.imports.size shouldEqual 1
      mainIr.imports.head.isInstanceOf[IR.Error.ImportExport] shouldBe false
      val mainBindingMap = mainIr.unwrapBindingMap
      val resolvedImportSymbols: List[String] =
        mainBindingMap.resolvedImports.map(_.target.qualifiedName.item)
      resolvedImportSymbols should contain theSameElementsAs List(
        "B_Module",
        "B_Type",
        "static_method"
      )
    }
  }
}
