package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.enso.compiler.Compiler;
import org.enso.compiler.core.ir.expression.errors.Redefined;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.test.mock.DiagnosticException;
import org.enso.compiler.test.mock.MockCompilerContext;
import org.enso.compiler.test.mock.MockModule;
import org.enso.compiler.test.mock.MockPackageRepository;
import org.enso.pkg.QualifiedName;
import org.junit.Test;

public final class CompilerErrorTest {
  @Test
  public void varialesIsRedefinedInIfBranch() {
    var path = "check.enso";
    var qName = QualifiedName.fromString("local.check");
    var code = """
    check x =
        x = 'No'
        x == 'False'
    """;

    var out = new ByteArrayOutputStream();
    var ps = new PrintStream(out);
    var repo = new MockPackageRepository();
    var ctx = new MockCompilerContext(repo, ps);
    var cfg =
        new CompilerConfig(
            true, true, true, true, scala.Option.empty(), true, true, scala.Option.apply(ps));
    var c = new Compiler(ctx, repo, cfg);
    var optPkg = c.getPackageRepository().getMainProjectPackage();
    var m = new MockModule(optPkg.get(), qName, path, code);
    try {
      var res = c.run(m);
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
