package org.enso.compiler.test.pass.analyse

import org.enso.compiler.Passes
import org.enso.compiler.context.{FreshNameSupply, ModuleContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Function,
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

class GatherDiagnosticsTest extends CompilerTest {

  "Error Gathering" should {
    val error1 = errors.Syntax(null, errors.Syntax.UnrecognizedToken)
    val plusOp = Name.Literal("+", isMethod = true, None)
    val plusApp = Application.Prefix(
      plusOp,
      List(
        CallArgument.Specified(None, error1, None)
      ),
      hasDefaultsSuspended = false,
      None
    )
    val lam = Function.Lambda(
      List(
        DefinitionArgument
          .Specified(
            Name.Literal("bar", isMethod = false, None),
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
      val error2 = errors.Syntax(null, errors.Syntax.UnexpectedExpression)
      val error3 = errors.Syntax(null, errors.Syntax.AmbiguousExpression)

      val typeName =
        Name.Literal("Foo", isMethod = false, None)
      val method1Name =
        Name.Literal("bar", isMethod = false, None)
      val method2Name =
        Name.Literal("baz", isMethod = false, None)
      val fooName =
        Name.Literal("foo", isMethod = false, None)

      val method1Ref =
        Name.MethodReference(
          Some(Name.Qualified(List(typeName), None)),
          method1Name,
          None
        )
      val method2Ref =
        Name.MethodReference(
          Some(Name.Qualified(List(typeName), None)),
          method2Name,
          None
        )

      val module = Module(
        List(),
        List(),
        List(
          Definition.Type(
            typeName,
            List(
              DefinitionArgument
                .Specified(fooName, None, Some(error2), suspended = false, None)
            ),
            List(),
            None
          ),
          definition.Method
            .Explicit(method1Ref, lam, None),
          definition.Method
            .Explicit(method2Ref, error3, None)
        ),
        false,
        None
      )

      val result = GatherDiagnostics.runModule(module, buildModuleContext())
      val gatheredErrros = result
        .unsafeGetMetadata(GatherDiagnostics, "Impossible")
        .diagnostics

      gatheredErrros.toSet shouldEqual Set(error1, error2, error3)
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
