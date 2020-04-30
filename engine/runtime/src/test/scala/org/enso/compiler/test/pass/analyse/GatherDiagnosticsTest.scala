package org.enso.compiler.test.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.CallArgument
import org.enso.compiler.pass.analyse.GatherDiagnostics
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.AST

class GatherDiagnosticsTest extends CompilerTest {
  "Error Gathering" should {
    val error1 = IR.Error.Syntax(
      AST.Invalid.Unrecognized("@@"),
      IR.Error.Syntax.UnrecognizedToken
    )
    val plusOp = IR.Name.Literal("+", None)
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
            IR.Name.Literal("bar", None),
            None,
            suspended = false,
            None
          )
      ),
      plusApp,
      None
    )

    "work with expression flow" in {
      val result = GatherDiagnostics.runExpression(lam, new InlineContext())
      val errors = result
        .unsafeGetMetadata[GatherDiagnostics.Diagnostics]("Impossible")
        .diagnostics

      errors.toSet shouldEqual Set(error1)
    }

    "work with module flow" in {
      val error2 = IR.Error.Syntax(
        AST.Invalid.Unexpected("whoa, that was not expected", List()),
        IR.Error.Syntax.UnexpectedExpression
      )

      val error3 = IR.Error.Syntax(
        AST.Invalid.Unexpected("whoa, that was also not expected", List()),
        IR.Error.Syntax.UnexpectedExpression
      )

      val typeName    = IR.Name.Literal("Foo", None)
      val method1Name = IR.Name.Literal("bar", None)
      val method2Name = IR.Name.Literal("baz", None)
      val fooName     = IR.Name.Literal("foo", None)

      val module = IR.Module(
        List(),
        List(
          IR.Module.Scope.Definition.Atom(
            typeName,
            List(
              IR.DefinitionArgument
                .Specified(fooName, Some(error2), suspended = false, None)
            ),
            None
          ),
          IR.Module.Scope.Definition.Method(typeName, method1Name, lam, None),
          IR.Module.Scope.Definition.Method(typeName, method2Name, error3, None)
        ),
        None
      )

      val result = GatherDiagnostics.runModule(module, ModuleContext())
      val errors = result
        .unsafeGetMetadata[GatherDiagnostics.Diagnostics]("Impossible")
        .diagnostics

      errors.toSet shouldEqual Set(error1, error2, error3)
    }
  }
}
