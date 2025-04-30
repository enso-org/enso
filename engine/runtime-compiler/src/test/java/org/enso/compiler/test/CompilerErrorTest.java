package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.compiler.core.ir.expression.errors.Redefined;
import org.enso.compiler.test.mock.DiagnosticException;
import org.enso.compiler.test.mock.WithMockCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;
import scala.Option;

public final class CompilerErrorTest {
  @Rule public final WithMockCompilerContext compilerCtx =
      WithMockCompilerContext.newBuilder().withModifiedCompilerConfig(bldr -> bldr.dumpModuleIR(
          Option.apply("Check"))).build();

  @Test
  public void variablesIsRedefinedInIfBranch() {
    var modName = QualifiedName.fromString("local.Proj.Check");
    var code = """
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
}
