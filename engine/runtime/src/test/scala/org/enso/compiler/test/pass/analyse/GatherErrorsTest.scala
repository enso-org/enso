package org.enso.compiler.test.pass.analyse

import org.enso.compiler.InlineContext
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.CallArgument
import org.enso.compiler.pass.analyse.GatherErrors
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.AST

class GatherErrorsTest extends CompilerTest {
  "Error Gathering" should {
    val error1 = IR.Error.Redefined.Argument(
      IR.DefinitionArgument
        .Specified(IR.Name.Literal("foo", None), None, false, None)
    )
    val error2 = IR.Error.Syntax(
      AST.Invalid.Unrecognized("@@"),
      IR.Error.Syntax.UnrecognizedToken
    )
    val plusOp = IR.Name.Literal("+", None)
    val plusApp = IR.Application.Prefix(
      plusOp,
      List(
        CallArgument.Specified(None, error1, None),
        CallArgument.Specified(None, error2, None)
      ),
      false,
      None
    )
    val lam = IR.Function.Lambda(
      List(
        IR.DefinitionArgument
          .Specified(IR.Name.Literal("bar", None), None, false, None)
      ),
      plusApp,
      None
    )

    "work with expression flow" in {
      val result = GatherErrors.runExpression(lam, new InlineContext())
      val errors = result
        .unsafeGetMetadata[GatherErrors.Errors]("Impossible")
        .errors

      errors.toSet shouldEqual Set(error1, error2)
    }

    "work with module flow" in {
      val error3 = IR.Error.Syntax(
        AST.Invalid.Unexpected("whoa, that was not expected", List()),
        IR.Error.Syntax.UnexpectedExpression
      )

      val error4 = IR.Error.Syntax(
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
                .Specified(fooName, Some(error3), false, None)
            ),
            None
          ),
          IR.Module.Scope.Definition.Method(typeName, method1Name, lam, None),
          IR.Module.Scope.Definition.Method(typeName, method2Name, error4, None)
        ),
        None
      )

      val result = GatherErrors.runModule(module)
      val errors = result
        .unsafeGetMetadata[GatherErrors.Errors]("Impossible")
        .errors

      errors.toSet shouldEqual Set(error1, error2, error3, error4)
    }
  }
}
