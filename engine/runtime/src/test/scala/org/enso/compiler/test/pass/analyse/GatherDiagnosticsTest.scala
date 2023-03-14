package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.CallArgument
import org.enso.compiler.pass.PassManager
import org.enso.compiler.pass.analyse.GatherDiagnostics
import org.enso.compiler.test.CompilerTest

class GatherDiagnosticsTest extends CompilerTest {

  "Error Gathering" should {
    val error1 = IR.Error.Syntax(null, IR.Error.Syntax.UnrecognizedToken)
    val plusOp = IR.Name.Literal("+", isMethod = true, None)
    val plusApp = IR.Application.Prefix(
      plusOp,
      List(
        CallArgument.Specified(None, error1, None)
      ),
      hasDefaultsSuspended = false,
      None
    )
    val lam = IR.Function.Lambda(
      List(
        IR.DefinitionArgument
          .Specified(
            IR.Name.Literal("bar", isMethod = false, None),
            None,
            None,
            suspended = false,
            None
          )
      ),
      plusApp,
      None
    )

    "work with expression flow" in {
      val result = GatherDiagnostics.runExpression(lam, buildInlineContext())
      val errors = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics

      errors.toSet shouldEqual Set(error1)
    }

    "work with module flow" in {
      val error2 = IR.Error.Syntax(null, IR.Error.Syntax.UnexpectedExpression)
      val error3 = IR.Error.Syntax(null, IR.Error.Syntax.AmbiguousExpression)

      val typeName =
        IR.Name.Literal("Foo", isMethod = false, None)
      val method1Name =
        IR.Name.Literal("bar", isMethod = false, None)
      val method2Name =
        IR.Name.Literal("baz", isMethod = false, None)
      val fooName =
        IR.Name.Literal("foo", isMethod = false, None)

      val method1Ref =
        IR.Name.MethodReference(
          Some(IR.Name.Qualified(List(typeName), None)),
          method1Name,
          None
        )
      val method2Ref =
        IR.Name.MethodReference(
          Some(IR.Name.Qualified(List(typeName), None)),
          method2Name,
          None
        )

      val module = IR.Module(
        List(),
        List(),
        List(
          IR.Module.Scope.Definition.Type(
            typeName,
            List(
              IR.DefinitionArgument
                .Specified(fooName, None, Some(error2), suspended = false, None)
            ),
            List(),
            None
          ),
          IR.Module.Scope.Definition.Method
            .Explicit(method1Ref, lam, None),
          IR.Module.Scope.Definition.Method
            .Explicit(method2Ref, error3, None)
        ),
        None
      )

      val result = GatherDiagnostics.runModule(module, buildModuleContext())
      val errors = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics

      errors.toSet shouldEqual Set(error1, error2, error3)
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
      diagnostics.map(_.message).toSet shouldEqual Set(
        "Unused variable unused.",
        "Unused function argument x."
      )
    }
  }
}
