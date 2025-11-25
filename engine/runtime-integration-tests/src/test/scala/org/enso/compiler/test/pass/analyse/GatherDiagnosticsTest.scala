package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Function,
  MetadataStorage,
  Module,
  Name
}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse.GatherDiagnostics
import org.enso.compiler.test.CompilerTest
import org.enso.persist.Persistance.Reference

class GatherDiagnosticsTest extends CompilerTest {

  "Error Gathering" should {
    val error1 = errors.Syntax(null, errors.Syntax.UnrecognizedToken)
    val plusOp = Name.Literal("+", isMethod = true, identifiedLocation = null)
    val plusApp = Application.Prefix
      .builder()
      .function(plusOp)
      .arguments(
        List(
          CallArgument.Specified
            .builder()
            .name(None)
            .value(error1)
            .isSynthetic(false)
            .build()
        )
      )
      .build()
    val lam = Function.Lambda
      .builder()
      .arguments(
        List(
          DefinitionArgument.Specified
            .builder()
            .name(
              Name.Literal("bar", isMethod = false, identifiedLocation = null)
            )
            .suspended(false)
            .build()
        )
      )
      .bodyReference(Reference.of(plusApp))
      .build()

    "work with expression flow" in {
      val result = GatherDiagnostics.runExpression(lam, buildInlineContext())
      val errors = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics

      errors.toSet shouldEqual Set(error1)
    }

    "work with module flow" in {
      val error2 = errors.Syntax(null, errors.Syntax.UnexpectedExpression)
      val error3 = errors.Syntax(null, errors.Syntax.AmbiguousExpression)

      val typeName =
        Name.Literal("Foo", isMethod = false, identifiedLocation = null)
      val method1Name =
        Name.Literal("bar", isMethod = false, identifiedLocation = null)
      val method2Name =
        Name.Literal("baz", isMethod = false, identifiedLocation = null)
      val fooName =
        Name.Literal("foo", isMethod = false, identifiedLocation = null)

      val method1Ref =
        Name.MethodReference(
          Some(Name.Qualified(List(typeName), identifiedLocation = null)),
          method1Name,
          identifiedLocation = null
        )
      val method2Ref =
        Name.MethodReference(
          Some(Name.Qualified(List(typeName), identifiedLocation = null)),
          method2Name,
          identifiedLocation = null
        )

      val module = new Module(
        List(),
        List(),
        List(
          Definition.Type
            .builder()
            .name(typeName)
            .params(
              List(
                DefinitionArgument.Specified
                  .builder()
                  .name(fooName)
                  .defaultValue(Some(error2))
                  .suspended(false)
                  .build()
              )
            )
            .build(),
          definition.Method.Explicit.fromMethodBinding(
            definition.Method.Binding
              .builder()
              .methodReference(method1Ref)
              .arguments(Nil)
              .isPrivate(false)
              .body(lam)
              .build(),
            lam
          ),
          definition.Method.Explicit.fromMethodBinding(
            definition.Method.Binding
              .builder()
              .methodReference(method2Ref)
              .arguments(Nil)
              .isPrivate(false)
              .body(error3)
              .build(),
            Function.Lambda
              .builder(lam)
              .bodyReference(Reference.of(error3))
              .build()
          )
        ),
        false,
        null,
        new MetadataStorage(),
        null
      )

      val result = GatherDiagnostics.runModule(module, buildModuleContext())
      val gatheredErros = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics

      gatheredErros.toSet shouldEqual Set(error1, error2, error3)
    }

    "work with annotations" in {
      implicit val passManager: PassManager =
        new Passes(defaultConfig).passManager

      implicit val moduleContext: ModuleContext =
        buildModuleContext(freshNameSupply = Some(new FreshNameSupply))

      val ir =
        """@x bar
          |foo x = x
          |""".stripMargin.preprocessModule
      val result = GatherDiagnostics.runModule(ir, moduleContext)
      val diagnostics = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics

      diagnostics should have size 1
      diagnostics.map(_.message(null)) should contain theSameElementsAs Seq(
        "The name `bar` could not be found"
      )
    }

    "avoid duplication" in {
      implicit val passManager: PassManager =
        new Passes(defaultConfig).passManager

      implicit val moduleContext: ModuleContext =
        buildModuleContext(freshNameSupply = Some(new FreshNameSupply))

      val ir =
        """
          |type Foo
          |    Bar1
          |    Bar2
          |
          |    foo x =
          |        unused = 0
          |        0
          |""".stripMargin.preprocessModule
      val result = GatherDiagnostics.runModule(ir, moduleContext)
      val diagnostics = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics
      diagnostics should have size 2
      diagnostics
        .map(_.message(null))
        .toSet shouldEqual Set(
        "Unused variable unused.",
        "Unused function argument x."
      )
    }
  }
}
