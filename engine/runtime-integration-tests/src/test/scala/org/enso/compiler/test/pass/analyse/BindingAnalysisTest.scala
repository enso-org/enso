package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Module
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.{
  Argument,
  Cons,
  ConversionMethod,
  ExtensionMethod,
  ModuleMethod,
  PolyglotSymbol,
  ResolvedExtensionMethod,
  Type
}
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest
import org.enso.persist.Persistance

class BindingAnalysisTest extends CompilerTest {

  // === Test Setup ===========================================================

  def mkModuleContext: ModuleContext =
    buildModuleContext(
      freshNameSupply = Some(new FreshNameSupply)
    )

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(BindingAnalysis).get

  val passConfiguration: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method to analyse an Enso module.
    *
    * @param ir the ir to analyse
    */
  implicit class AnalyseModule(ir: Module) {

    /** Performs tail call analysis on [[ir]].
      *
      * @param context the module context in which analysis takes place
      * @return [[ir]], with tail call analysis metadata attached
      */
    def analyse(implicit context: ModuleContext): Module = {
      BindingAnalysis.runModule(ir, context)
    }
  }

  // === The Tests ============================================================

  "Module binding resolution" should {
    "extension method is a defined entity" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |My_Type.extension_method = 42
          |""".stripMargin.preprocessModule.analyse
      val metadata = ir.unsafeGetMetadata(BindingAnalysis, "Should exist.")

      metadata.definedEntities should contain theSameElementsAs List(
        Type("My_Type", List(), List(), false),
        ExtensionMethod("extension_method", "My_Type")
      )

      metadata.resolveName("extension_method") shouldEqual Right(
        List(
          ResolvedExtensionMethod(
            ctx.moduleReference(),
            ExtensionMethod("extension_method", "My_Type")
          )
        )
      )
    }

    "extension methods are defined entities" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type My_Type
          |type Other_Type
          |My_Type.extension_method = 42
          |Other_Type.extension_method = 42
          |""".stripMargin.preprocessModule.analyse
      val metadata = ir.unsafeGetMetadata(BindingAnalysis, "Should exist.")

      metadata.definedEntities should contain theSameElementsAs List(
        Type("My_Type", List(), List(), false),
        Type("Other_Type", List(), List(), false),
        ExtensionMethod("extension_method", "My_Type"),
        ExtensionMethod("extension_method", "Other_Type")
      )

      metadata.resolveName("extension_method") shouldBe Right(
        List(
          ResolvedExtensionMethod(
            ctx.moduleReference(),
            ExtensionMethod("extension_method", "My_Type")
          ),
          ResolvedExtensionMethod(
            ctx.moduleReference(),
            ExtensionMethod("extension_method", "Other_Type")
          )
        )
      )
    }

    "conversion method is a defined entity" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |type Source
          |type Target
          |Target.from (that:Source) = 42
          |Source.from (that:Target) = 42
          |""".stripMargin.preprocessModule.analyse
      val metadata = ir.unsafeGetMetadata(BindingAnalysis, "Should exist.")
      metadata.definedEntities should contain theSameElementsAs List(
        Type("Source", List(), List(), false),
        Type("Target", List(), List(), false),
        ConversionMethod("from", "Source", "Target"),
        ConversionMethod("from", "Target", "Source")
      )
    }

    "discover all atoms, methods, and polyglot symbols in a module" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val ir =
        """
          |polyglot java import foo.bar.baz.MyClass
          |polyglot java import foo.bar.baz.OtherClass as Renamed_Class
          |
          |type Foo
          |    Mk_Foo a b c
          |type Bar
          |type Baz x y
          |
          |Baz.foo = 123
          |Bar.baz = Baz 1 2 . foo
          |
          |from (_ : Bar) = Foo 0 0 0
          |from (that : Baz) = Foo that.x that.x that.y
          |
          |Foo.from (_ : Bar) = undefined
          |
          |foo = 123
          |""".stripMargin.preprocessModule.analyse

      val metadata = ir.unsafeGetMetadata(BindingAnalysis, "Should exist.")

      metadata.definedEntities should contain theSameElementsAs List(
        PolyglotSymbol("MyClass"),
        PolyglotSymbol("Renamed_Class"),
        Type(
          "Foo",
          List(),
          List(
            Cons(
              "Mk_Foo",
              List(
                Argument(
                  "a",
                  hasDefaultValue = false,
                  Persistance.Reference.none()
                ),
                Argument(
                  "b",
                  hasDefaultValue = false,
                  Persistance.Reference.none()
                ),
                Argument(
                  "c",
                  hasDefaultValue = false,
                  Persistance.Reference.none()
                )
              ),
              isProjectPrivate = false
            )
          ),
          builtinType = false
        ),
        Type("Bar", List(), List(), builtinType         = false),
        Type("Baz", List("x", "y"), List(), builtinType = false),
        ExtensionMethod("foo", "Baz"),
        ExtensionMethod("baz", "Bar"),
        ConversionMethod("from", "Bar", "Foo"),
        ModuleMethod("foo")
      )
      metadata.currentModule shouldEqual ctx.moduleReference()
    }

    "properly assign module-level methods when a type with the same name as module is defined" in {
      implicit val ctx: ModuleContext = mkModuleContext
      val moduleName                  = ctx.getName().item
      val ir =
        s"""
           |type $moduleName
           |    type $moduleName
           |    foo x = x + 1
           |
           |bar x = x + 1
           |
           |$moduleName.baz = 65
           |""".stripMargin.preprocessModule.analyse
      ir.getMetadata(BindingAnalysis)
        .get
        .definedEntities
        .filter(
          _.isInstanceOf[BindingsMap.Method]
        ) should contain theSameElementsAs List(
        ExtensionMethod("foo", moduleName),
        ModuleMethod("bar"),
        ExtensionMethod("baz", moduleName)
      )

    }
  }
}
