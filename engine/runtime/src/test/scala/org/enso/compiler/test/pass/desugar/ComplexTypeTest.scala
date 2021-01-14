package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.pass.desugar.ComplexType
import org.enso.compiler.pass.resolve.ModuleAnnotations
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ComplexTypeTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes

  val precursorPasses: PassGroup = passes.getPrecursors(ComplexType).get

  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run complex type desugaring on an
    * [[IR.Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: IR.Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any complex type definitions desugared
      */
    def desugar(implicit moduleContext: ModuleContext): IR.Module = {
      ComplexType.runModule(ir, moduleContext)
    }
  }

  /** Creates a defaulted module context.
    *
    * @return a defaulted module context
    */
  def mkModuleContext: ModuleContext = {
    buildModuleContext()
  }

  // === The Tests ============================================================

  "Valid complex types" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |type Maybe
        |    Nothing
        |    type Just a
        |
        |    invalid_sig : Int
        |
        |    is_just : this -> Boolean
        |    is_just = case this of
        |        Nothing -> false
        |        Just _  -> true
        |
        |    f : (a: Monoid) -> a
        |    f a = this + a
        |
        |    bad_trailing_sig : Double
        |""".stripMargin.preprocessModule.desugar

    "have their atoms desugared to top-level atoms" in {
      val ir =
        """
          |type MyType
          |    type Foo
          |    type Bar
          |""".stripMargin.preprocessModule.desugar

      exactly(2, ir.bindings) shouldBe a[Definition.Atom]
      ir.bindings.head.asInstanceOf[Definition.Atom].name.name shouldEqual "Foo"
      ir.bindings(1).asInstanceOf[Definition.Atom].name.name shouldEqual "Bar"
    }

    "have annotations on the type desugared to annotations on the defined" in {
      val ir =
        """@Builtin_Type
          |type My_Type
          |    Foo
          |    type Bar
          |""".stripMargin.preprocessModule.desugar

      exactly(1, ir.bindings) shouldBe a[Definition.Atom]
      ir.bindings.head
        .asInstanceOf[Definition.Atom]
        .unsafeGetMetadata(ModuleAnnotations, "")
        .annotations
        .head
        .name shouldEqual "@Builtin_Type"
    }

    "have their methods desugared to methods on included atoms" in {
      ir.bindings(3) shouldBe an[Definition.Method.Binding]
      val justIsJust = ir.bindings(3).asInstanceOf[Definition.Method.Binding]
      justIsJust.methodName.name shouldEqual "is_just"
      justIsJust.typeName.name shouldEqual "Nothing"

      ir.bindings(7) shouldBe an[Definition.Method.Binding]
      val justF = ir.bindings(7).asInstanceOf[Definition.Method.Binding]
      justF.methodName.name shouldEqual "f"
      justF.typeName.name shouldEqual "Nothing"
    }

    "have their methods desugared to methods on the defined atoms" in {
      ir.bindings(5) shouldBe an[Definition.Method.Binding]
      val justIsJust = ir.bindings(5).asInstanceOf[Definition.Method.Binding]
      justIsJust.methodName.name shouldEqual "is_just"
      justIsJust.typeName.name shouldEqual "Just"

      ir.bindings(9) shouldBe an[Definition.Method.Binding]
      val justF = ir.bindings(9).asInstanceOf[Definition.Method.Binding]
      justF.methodName.name shouldEqual "f"
      justF.typeName.name shouldEqual "Just"
    }

    "have type signatures copied to above each method" in {
      ir.bindings(2) shouldBe an[IR.Type.Ascription]
      ir.bindings(6) shouldBe an[IR.Type.Ascription]
      ir.bindings(4) shouldBe an[IR.Type.Ascription]
      ir.bindings(8) shouldBe an[IR.Type.Ascription]

      val nothingIsJustSigName = ir
        .bindings(2)
        .asInstanceOf[IR.Type.Ascription]
        .typed
        .asInstanceOf[IR.Name.MethodReference]
      val nothingIsJustMethodName = ir
        .bindings(3)
        .asInstanceOf[Definition.Method.Binding]
        .methodReference

      assert(
        nothingIsJustSigName isSameReferenceAs nothingIsJustMethodName,
        "The type signature and method did not have the same reference."
      )

      val nothingFSigName = ir
        .bindings(6)
        .asInstanceOf[IR.Type.Ascription]
        .typed
        .asInstanceOf[IR.Name.MethodReference]
      val nothingFMethodName = ir
        .bindings(7)
        .asInstanceOf[Definition.Method.Binding]
        .methodReference

      assert(
        nothingFSigName isSameReferenceAs nothingFMethodName,
        "The type signature and method did not have the same reference."
      )

      val justIsJustSigName = ir
        .bindings(4)
        .asInstanceOf[IR.Type.Ascription]
        .typed
        .asInstanceOf[IR.Name.MethodReference]
      val justIsJustMethodName = ir
        .bindings(5)
        .asInstanceOf[Definition.Method.Binding]
        .methodReference

      assert(
        justIsJustSigName isSameReferenceAs justIsJustMethodName,
        "The type signature and method did not have the same reference."
      )

      val justFSigName = ir
        .bindings(8)
        .asInstanceOf[IR.Type.Ascription]
        .typed
        .asInstanceOf[IR.Name.MethodReference]
      val justFMethodName = ir
        .bindings(9)
        .asInstanceOf[Definition.Method.Binding]
        .methodReference

      assert(
        justFSigName isSameReferenceAs justFMethodName,
        "The type signature and method did not have the same reference."
      )
    }

    "leave un-associated signatures intact" in {
      ir.bindings(1) shouldBe an[IR.Type.Ascription]
      ir.bindings(1)
        .asInstanceOf[IR.Type.Ascription]
        .typed
        .asInstanceOf[IR.Name.Literal]
        .name shouldEqual "invalid_sig"

      ir.bindings(10) shouldBe an[IR.Type.Ascription]
      ir.bindings(10)
        .asInstanceOf[IR.Type.Ascription]
        .typed
        .asInstanceOf[IR.Name.Literal]
        .name shouldEqual "bad_trailing_sig"
    }
  }
}
