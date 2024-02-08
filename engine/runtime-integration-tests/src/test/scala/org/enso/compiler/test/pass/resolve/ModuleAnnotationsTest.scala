package org.enso.compiler.test.pass.resolve

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.Function
import org.enso.compiler.core.ir.Module
import org.enso.compiler.core.ir.Name
import org.enso.compiler.core.ir.expression.Comment
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
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
  implicit class ResolveModule(ir: Module) {

    /** Runs annotation resolution on an [[Module]].
      *
      * @param moduleContext the module context in which the resolution takes
      *                      place
      * @return [[ir]], with all module-level annotations resolved
      */
    def resolve(implicit moduleContext: ModuleContext): Module = {
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
      ir.bindings.head shouldBe a[Definition.SugaredType]
      val anns =
        ir.bindings.head.unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 2
      anns.head.asInstanceOf[Name].name shouldEqual "@My_Annotation_1"
      anns(1).asInstanceOf[Name].name shouldEqual "@My_Annotation_2"
    }

    "associate annotations with complex type definitions" in {
      val ir =
        """@My_Annotation_1
          |@My_Annotation_2
          |type Foo
          |  type Bar
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.SugaredType]
      val anns =
        ir.bindings.head.unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 2
      anns.head.asInstanceOf[Name].name shouldEqual "@My_Annotation_1"
      anns(1).asInstanceOf[Name].name shouldEqual "@My_Annotation_2"
    }

    "associate annotations with method definitions" in {
      val ir =
        """@My_Annotation_1
          |@My_Annotation_2
          |My_Type.add a b = this.frob(a + b)
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[definition.Method.Binding]
      val anns =
        ir.bindings.head.unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 2
      anns.head.asInstanceOf[Name].name shouldEqual "@My_Annotation_1"
      anns(1).asInstanceOf[Name].name shouldEqual "@My_Annotation_2"
    }

    "not associate annotations with comments" in {
      val ir =
        """@My_Annotation
          |## My doc comment
          |type Foo
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 2
      ir.bindings(1) shouldBe a[Definition.SugaredType]
      val anns =
        ir.bindings(1).unsafeGetMetadata(ModuleAnnotations, "").annotations
      anns.length shouldEqual 1
      anns.head.asInstanceOf[Name].name shouldEqual "@My_Annotation"
    }

    "not associate generic annotations with method definitions" in {
      val ir =
        """@a expr_1
          |@b expr_2
          |My_Type.add a b = this.frob(a + b)
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 3
      ir.bindings(0) shouldBe a[Name.GenericAnnotation]
      ir.bindings(1) shouldBe a[Name.GenericAnnotation]
    }

  }

  "Annotation desugaring in complex types" should {
    implicit val moduleContext: ModuleContext = mkModuleContext

    "not associate generic annotations with atom definitions" in {
      val ir =
        """@My_Annotation
          |type Foo
          |    @my annotation
          |    Bar
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.SugaredType]
      val typ = ir.bindings.head.asInstanceOf[Definition.SugaredType]
      typ.body.length shouldEqual 2
      typ.body(0) shouldBe an[Name.GenericAnnotation]
      typ.body(1) shouldBe a[Definition.Data]
    }

    "not associate generic annotations with method definitions" in {
      val ir =
        """type Foo
          |    Foo
          |
          |    @a expr
          |    my_method a = a
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.SugaredType]
      val typ = ir.bindings.head.asInstanceOf[Definition.SugaredType]
      typ.body.length shouldEqual 3
      typ.body(1) shouldBe an[Name.GenericAnnotation]
      typ.body(2) shouldBe an[Function.Binding]
    }

    "not associate annotations with comments" in {
      val ir =
        """
          |type Foo
          |    @my annotation
          |    ## Doc comment
          |    Foo
          |""".stripMargin.preprocessModule.resolve

      ir.bindings.length shouldEqual 1
      ir.bindings.head shouldBe a[Definition.SugaredType]
      val typ = ir.bindings.head.asInstanceOf[Definition.SugaredType]
      typ.body.length shouldEqual 3
      typ.body(0) shouldBe an[Name.GenericAnnotation]
      typ.body(1) shouldBe an[Comment]
      typ.body(2) shouldBe a[Definition.Data]
    }
  }
}
