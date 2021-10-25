package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.pass.resolve.ModuleAnnotations
import org.enso.compiler.test.CompilerTest

class ModuleAnnotationsTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(ModuleAnnotations).get

  val passConfiguration: PassConfiguration = PassConfiguration();

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfiguration)

  /** Adds an extension method for running module-level annotations resolution
    * on the input IR.
    *
    * @param ir the IR to resolve
    */
  implicit class ResolveModule(ir: IR.Module) {

    /** Runs annotation resolution on an [[IR.Module]].
      *
      * @param moduleContext the module context in which the resolution takes
      *                      place
      * @return [[ir]], with all module-level annotations resolved
      */
    def resolve(implicit moduleContext: ModuleContext): IR.Module = {
      ModuleAnnotations.runModule(ir, moduleContext)
    }
  }

  /** Makes a module context.
    *
    * @return a fresh module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext()
  }

  // === The Tests ============================================================

  "Annotation desugaring at the top level" should {
    implicit val moduleContext: ModuleContext = mkModuleContext

    "associate annotations with atom definitions" in {
      val ir =
        """@My_Annotation_1
          |@My_Annotation_2
          |type My_Atom
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.Atom]
      val anns =
        ir.bindings.head.unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 2
      anns.head.asInstanceOf[IR.Name].name shouldEqual "@My_Annotation_1"
      anns(1).asInstanceOf[IR.Name].name shouldEqual "@My_Annotation_2"
    }

    "associate annotations with complex type definitions" in {
      val ir =
        """@My_Annotation_1
          |@My_Annotation_2
          |type Foo
          |  type Bar
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.Type]
      val anns =
        ir.bindings.head.unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 2
      anns.head.asInstanceOf[IR.Name].name shouldEqual "@My_Annotation_1"
      anns(1).asInstanceOf[IR.Name].name shouldEqual "@My_Annotation_2"
    }

    "associate annotations with method definitions" in {
      val ir =
        """@My_Annotation_1
          |@My_Annotation_2
          |My_Type.add a b = this.frob(a + b)
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.Method.Binding]
      val anns =
        ir.bindings.head.unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 2
      anns.head.asInstanceOf[IR.Name].name shouldEqual "@My_Annotation_1"
      anns(1).asInstanceOf[IR.Name].name shouldEqual "@My_Annotation_2"
    }

    "not associate annotations with comments" in {
      val ir =
        """@My_Annotation
          |## My doc comment
          |type Foo
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 2
      ir.bindings(1) shouldBe a[Definition.Atom]
      val anns =
        ir.bindings(1).unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 1
      anns.head.asInstanceOf[IR.Name].name shouldEqual "@My_Annotation"
    }
  }

  "Annotation desugaring in complex types" should {
    implicit val moduleContext: ModuleContext = mkModuleContext

    "associate annotations with atom definitions" in {
      val ir =
        """@My_Annotation
          |type Foo
          |    @My_Annotation
          |    type Bar
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.Type]
      val typ = ir.bindings.head.asInstanceOf[Definition.Type]
      typ.body.length shouldEqual 1
      typ.body.head shouldBe a[Definition.Atom]
      typ.body.head
        .unsafeGetMetadata(ModuleAnnotations, "")
        .annotations
        .head
        .asInstanceOf[IR.Name]
        .name shouldEqual "@My_Annotation"
    }

    "associate annotations with method definitions" in {
      val ir =
        """type Foo
          |    type Foo
          |
          |    @My_Annotation
          |    my_method a = a
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.Type]
      val typ = ir.bindings.head.asInstanceOf[Definition.Type]
      typ.body.length shouldEqual 2
      typ.body(1) shouldBe an[IR.Function.Binding]
      typ
        .body(1)
        .unsafeGetMetadata(ModuleAnnotations, "")
        .annotations
        .head
        .asInstanceOf[IR.Name]
        .name shouldEqual "@My_Annotation"
    }

    "not associate annotations with comments" in {
      val ir =
        """
          |type Foo
          |    @My_Annotation
          |    ## Doc comment
          |    type Foo
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.Type]
      val typ = ir.bindings.head.asInstanceOf[Definition.Type]
      typ.body.length shouldEqual 2
      typ.body.head shouldBe an[IR.Comment]
      typ.body(1) shouldBe a[Definition.Atom]
      typ
        .body(1)
        .unsafeGetMetadata(ModuleAnnotations, "")
        .annotations
        .head
        .asInstanceOf[IR.Name]
        .name shouldEqual "@My_Annotation"
    }
  }
}
