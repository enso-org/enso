package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;

import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

/**
 * Tests multiple project compilation with the compiler with {@link WithCompilerContext mock
 * compiler context}.
 */
public class MultipleProjectsTest {
  @Rule public WithCompilerContext compilerCtx = WithCompilerContext.createDefault();

  @Test
  public void canImportType_FromDifferentProject() {
    var libMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Lib.Main"),
            """
            type Lib_Type
            """);
    var projMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from local.Lib import Lib_Type
            """);
    var compiler = compilerCtx.getCompiler();
    var libCompilationRes = compiler.run(libMod);
    assertThat(
        String.format("Lib module compiled successfully: %s", libCompilationRes.compiledModules()),
        libCompilationRes.compiledModules().size(),
        is(greaterThan(1)));
    var projCompilationRes = compiler.run(projMod);
    assertThat(
        "Proj module compiled successfully",
        projCompilationRes.compiledModules().size(),
        is(greaterThan(1)));
  }

  @Test
  public void canImportType_FromTwoDifferentProjects() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Lib1.Main"),
        """
        type Lib1_Type
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Lib2.Main"),
        """
        type Lib2_Type
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from local.Lib1 import Lib1_Type
            from local.Lib2 import Lib2_Type
            """);
    var res = compilerCtx.getCompiler().run(mainMod);
    assertThat(
        "Main module compiled successfully", res.compiledModules().size(), is(greaterThan(1)));
  }
}
