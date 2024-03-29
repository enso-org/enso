package org.enso.compiler.test.pass.desugar

import org.enso.compiler.Passes
import org.enso.compiler.context.ModuleContext
import org.enso.compiler.core.ir.{Module, Name, Type}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.pass.desugar.ComplexType
import org.enso.compiler.pass.{PassConfiguration, PassGroup, PassManager}
import org.enso.compiler.test.CompilerTest

class ComplexTypeTest extends CompilerTest {

  // === Test Setup ===========================================================

  val passes = new Passes(defaultConfig)

  val precursorPasses: PassGroup = passes.getPrecursors(ComplexType).get

  val passConfig: PassConfiguration = PassConfiguration()

  implicit val passManager: PassManager =
    new PassManager(List(precursorPasses), passConfig)

  /** Adds an extension method to run complex type desugaring on an
    * [[Module]].
    *
    * @param ir the module to run desugaring on
    */
  implicit class DesugarModule(ir: Module) {

    /** Runs desugaring on a module.
      *
      * @param moduleContext the module context in which desugaring is taking
      *                      place
      * @return [[ir]], with any complex type definitions desugared
      */
    def desugar(implicit moduleContext: ModuleContext): Module = {
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
        |    Just a
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

    "have their atoms desugared" in {
      val ir =
        """
          |type MyType
          |    Foo
          |    Bar
          |""".stripMargin.preprocessModule.desugar
      val tp = ir.bindings(0).asInstanceOf[Definition.Type]
      tp.name.name shouldEqual "MyType"
      tp.members(0).name.name shouldEqual "Foo"
      tp.members(1).name.name shouldEqual "Bar"
    }

    "have their annotations correct" in {
      val ir =
        """
          |type MyType
          |    @a 42
          |    bar self = self
          |
          |    Foo
          |""".stripMargin.preprocessModule.desugar
      ir.bindings.length shouldEqual 3

      val tp = ir.bindings(0).asInstanceOf[Definition.Type]
      tp.name.name shouldEqual "MyType"
      tp.members(0).name.name shouldEqual "Foo"

      val a = ir.bindings(1).asInstanceOf[Name.GenericAnnotation]
      a.name shouldEqual "a"

      val bar = ir.bindings(2).asInstanceOf[definition.Method.Binding]
      bar.methodName.name shouldEqual "bar"
    }

    "have their methods desugared to binding methods" in {
      ir.bindings(3) shouldBe an[definition.Method.Binding]
      val isJust = ir.bindings(3).asInstanceOf[definition.Method.Binding]
      isJust.methodName.name shouldEqual "is_just"
      isJust.typeName.get.name shouldEqual "Maybe"

      ir.bindings(5) shouldBe an[definition.Method.Binding]
      val f = ir.bindings(5).asInstanceOf[definition.Method.Binding]
      f.methodName.name shouldEqual "f"
      f.typeName.get.name shouldEqual "Maybe"
    }

    "have type signatures copied to above each method" in {
      ir.bindings(2) shouldBe an[Type.Ascription]
      ir.bindings(4) shouldBe an[Type.Ascription]

      val isJustSigName = ir
        .bindings(2)
        .asInstanceOf[Type.Ascription]
        .typed
        .asInstanceOf[Name.MethodReference]
      val isJustMethodName = ir
        .bindings(3)
        .asInstanceOf[definition.Method.Binding]
        .methodReference

      assert(
        isJustSigName isSameReferenceAs isJustMethodName,
        "The type signature and method did not have the same reference."
      )

      val fSigName = ir
        .bindings(4)
        .asInstanceOf[Type.Ascription]
        .typed
        .asInstanceOf[Name.MethodReference]
      val fMethodName = ir
        .bindings(5)
        .asInstanceOf[definition.Method.Binding]
        .methodReference

      assert(
        fSigName isSameReferenceAs fMethodName,
        "The type signature and method did not have the same reference."
      )
    }

    "leave un-associated signatures intact" in {
      ir.bindings(1) shouldBe an[Type.Ascription]
      ir.bindings(1)
        .asInstanceOf[Type.Ascription]
        .typed
        .asInstanceOf[Name.Literal]
        .name shouldEqual "invalid_sig"

      ir.bindings(6) shouldBe an[Type.Ascription]
      ir.bindings(6)
        .asInstanceOf[Type.Ascription]
        .typed
        .asInstanceOf[Name.Literal]
        .name shouldEqual "bad_trailing_sig"
    }
  }

  "Invalid complex types" should {
    implicit val ctx: ModuleContext = mkModuleContext

    val ir =
      """
        |type Foo
        |    Bar
        |    Baz
        |
        |    g a = this + a
        |
        |    f a =
        |""".stripMargin.preprocessModule.desugar

    "have their types translated untouched" in {
      ir.bindings(0) shouldBe a[Definition.Type]
      val tp = ir.bindings(0).asInstanceOf[Definition.Type]
      tp.members(1).name.name shouldEqual "Baz"
    }

    "have their errors translated untouched" in {
      ir.bindings.last shouldBe an[errors.Syntax]
      val err = ir.bindings.last.asInstanceOf[errors.Syntax]
      err.reason shouldBe an[errors.Syntax.UnexpectedDeclarationInType.type]
    }

    "have their valid methods desugared" in {
      ir.bindings(1) shouldBe a[definition.Method.Binding]
      val method = ir.bindings(1).asInstanceOf[definition.Method.Binding]
      method.typeName.get.name shouldEqual "Foo"
      method.methodName.name shouldEqual "g"
    }
  }
}
