package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.compiler.core.ir.expression.errors.Redefined;
import org.enso.compiler.test.mock.DiagnosticException;
import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

public final class CompilerErrorTest {
  @Rule public final WithCompilerContext compilerCtx = WithCompilerContext.createDefault();

  @Test
  public void variablesIsRedefinedInIfBranch() {
    var modName = QualifiedName.fromString("local.Proj.Check");
    var code =
        """
        check x =
            x = 'No'
            x == 'False'
        """;
    var m = compilerCtx.createModule(modName, code);
    try {
      var res = compilerCtx.getCompiler().run(m);
      fail("Compilation shall fail, but got: " + res);
    } catch (DiagnosticException t) {
      assertSame(m, t.module);
      assertNotNull(t.diagnostic);
      assertTrue(t.diagnostic instanceof Redefined.Binding);
      var invalid = ((Redefined.Binding) t.diagnostic).invalidBinding();
      assertEquals("x", invalid.name().name());
    }
  }

  @Test
  public void cannotSpecifyTwoSelfArguments_InMethodCall() {
    var modName = QualifiedName.fromString("local.Proj.Check");
    var code =
        """
        type My_Type
            Cons data
            method self = 42

        main =
            obj = My_Type.Cons 23
            My_Type.method self=My_Type self=obj
        """;
    var mod = compilerCtx.createModule(modName, code);
    try {
      var res = compilerCtx.getCompiler().run(mod);
      fail("Compilation shall fail, but got: " + res);
    } catch (DiagnosticException t) {
      assertNotNull(t.diagnostic);
      assertTrue(t.diagnostic instanceof Redefined.SelfArg);
    }
  }
}
