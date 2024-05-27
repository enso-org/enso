package org.enso.interpreter.test

import org.enso.common.CompilationStage
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.SymbolRestriction.{
  AllowedResolution,
  Intersect,
  Only,
  Union
}
import org.enso.compiler.phase.{ExportsResolution, ImportResolver}
import org.enso.pkg.QualifiedName
import org.enso.polyglot.{PolyglotContext, RuntimeOptions}
import org.enso.test.utils.{SourceModule, TestUtils}
import org.graalvm.polyglot.Context
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfter

import java.nio.file.{Files, Path}

class ExportResolutionTest
    extends AnyWordSpecLike
    with Matchers
    with BeforeAndAfter {

  var ctx: Context                                              = null
  var projDir: Path                                             = null
  var importsResolver: ImportResolver                           = null
  var exportsResolver: ExportsResolution                        = null
  var mainMod: org.enso.compiler.context.CompilerContext.Module = null
  var aMod: org.enso.compiler.context.CompilerContext.Module    = null
  var bMod: org.enso.compiler.context.CompilerContext.Module    = null
  var dataMod: org.enso.compiler.context.CompilerContext.Module = null
  var aModAllowedRes: AllowedResolution                         = null
  var dataModAllowedRes: AllowedResolution                      = null

  before {
    val aModSrc =
      """
        |type T
        |""".stripMargin
    val aSrcMod =
      new SourceModule(QualifiedName.fromString("Data.A_Module"), aModSrc)
    val bModSrc =
      """
        |from project.Data.A_Module export T
        |""".stripMargin
    val bSrcMod =
      new SourceModule(QualifiedName.fromString("Data.B_Module"), bModSrc)
    val mainSrc =
      """
        |import project.Data
        |import project.Data.B_Module
        |export project.Data
        |export project.Data.B_Module
        |""".stripMargin
    val mainSrcMod = new SourceModule(QualifiedName.fromString("Main"), mainSrc)
    this.projDir = Files.createTempDirectory("export-resolution-test")
    TestUtils.createProject(
      "Proj",
      java.util.Set.of(aSrcMod, bSrcMod, mainSrcMod),
      projDir
    )
    this.ctx = TestUtils
      .defaultContextBuilder()
      .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath.toString)
      .build()
    val ensoCtx  = TestUtils.leakContext(ctx)
    val compiler = ensoCtx.getCompiler
    this.importsResolver = new ImportResolver(compiler)
    this.exportsResolver = new ExportsResolution(compiler.context)
    this.mainMod =
      ensoCtx.getPackageRepository.getLoadedModule("local.Proj.Main").get;
    mainMod.getCompilationStage shouldEqual CompilationStage.INITIAL
    val polyCtx = new PolyglotContext(ctx)
    polyCtx.getTopScope.compile(true)
    mainMod.getCompilationStage shouldEqual CompilationStage.AFTER_CODEGEN
    this.aMod = ensoCtx.getPackageRepository
      .getLoadedModule("local.Proj.Data.A_Module")
      .get
    this.bMod = ensoCtx.getPackageRepository
      .getLoadedModule("local.Proj.Data.B_Module")
      .get
    this.dataMod = ensoCtx.getPackageRepository
      .getLoadedModule("local.Proj.Data")
      .get
    aMod.getCompilationStage shouldEqual CompilationStage.AFTER_CODEGEN
    bMod.getCompilationStage shouldEqual CompilationStage.AFTER_CODEGEN
    dataMod.getCompilationStage shouldEqual CompilationStage.AFTER_CODEGEN

    this.aModAllowedRes = AllowedResolution(
      "a_module",
      Some(
        BindingsMap.ResolvedModule(
          BindingsMap.ModuleReference.Concrete(aMod)
        )
      )
    )
    this.dataModAllowedRes = AllowedResolution(
      "data",
      Some(
        BindingsMap.ResolvedModule(
          BindingsMap.ModuleReference.Concrete(dataMod)
        )
      )
    )
  }

  after {
    TestUtils.deleteRecursively(projDir)
    ctx.close()
  }

  "Symbol restrictions" should {
    "union optimization" in {
      val union = Union(
        List(
          Only(Set(aModAllowedRes))
        )
      )
      val optimizedUnion = union.optimize
      val allowedSyms    = optimizedUnion.asInstanceOf[Only].symbols
      allowedSyms.size shouldEqual 1
      allowedSyms.head.symbol shouldEqual "a_module"
      allowedSyms.head.resolution shouldBe defined
      allowedSyms.head.resolution.get.qualifiedName.item shouldEqual "A_Module"
    }

    "union with two only syms optimization" in {
      val union = Union(
        List(
          Only(Set(aModAllowedRes)),
          Only(Set(dataModAllowedRes))
        )
      )
      val optimizedUnion = union.optimize
      optimizedUnion.isInstanceOf[Only] shouldBe true
      val syms = optimizedUnion.asInstanceOf[Only].symbols
      syms.size shouldBe 2
      syms.head.symbol shouldEqual "a_module"
      syms.last.symbol shouldEqual "data"
    }

    "empty intersect optimization" in {
      val intersect = Intersect(
        List(Only(Set(dataModAllowedRes)), Only(Set(aModAllowedRes)))
      )
      val optimized = intersect.optimize
      optimized.isInstanceOf[Only] shouldBe true
      optimized.asInstanceOf[Only].symbols.isEmpty shouldBe true
    }
  }

  "Export resolution module order" should {
    "correctly sort modules after export resolution" in {
      // The import resolver has to run first. Without it, export resolution would not
      // work at all.
      val res =
        importsResolver.mapImports(mainMod, bindingsCachingEnabled = false)
      withClue("bindings caching is disabled") {
        res._2.isEmpty shouldBe true
      }
      val modulesWithResolvedImps = res._1
      val modulesToCompile        = exportsResolver.run(modulesWithResolvedImps)
      modulesToCompile.isEmpty shouldBe false
      val modNames = modulesToCompile.map(_.getName.item)
      // B_Module export A_Module
      // Data export List(B_Module, A_Module)
      // Main exports Data
      modNames should contain theSameElementsInOrderAs List(
        "A_Module",
        "B_Module",
        "Data",
        "Main"
      )
    }
  }

}
