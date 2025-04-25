package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.compiler.core.ir.expression.errors.Redefined;
import org.enso.compiler.test.mock.DiagnosticException;
import org.enso.compiler.test.mock.SourceModule;
import org.enso.compiler.test.mock.WithMockCompilerContext;
import org.enso.editions.LibraryName;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

public final class CompilerErrorTest {
  @Rule public final WithMockCompilerContext compilerCtx = WithMockCompilerContext.createDefault();

  @Test
  public void variablesIsRedefinedInIfBranch() {
    var modName = QualifiedName.fromString("Check");
    var code = """
    check x =
        x = 'No'
        x == 'False'
    """;
    var pkgName = LibraryName.apply("local", "Proj");
    var pkg = compilerCtx.createPackage(pkgName, new SourceModule(modName, code));
    compilerCtx.registerMainProjectPackage(pkgName, pkg);
    var m = compilerCtx.getLoadedModules().get(0);
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
