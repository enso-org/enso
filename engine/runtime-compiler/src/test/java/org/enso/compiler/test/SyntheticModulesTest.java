package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;

import java.util.List;
import org.enso.compiler.CompilerResult;
import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

public class SyntheticModulesTest {
  @Rule public WithCompilerContext compilerCtx = WithCompilerContext.createDefault();

  @Test
  public void testCompilationOfSyntheticModules() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Proj.A.B"),
        """
        type B_Type
        """);
    var mainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            import project.A.B.B_Type
            """);
    var res = compilerCtx.getCompiler().run(mainMod);
    assertSuccessfulCompilation(res);
    var loadedModules = compilerCtx.getLoadedModules();
    var parentMod =
        loadedModules.stream()
            .filter(m -> m.getName().toString().equals("local.Proj.A"))
            .findFirst()
            .orElseThrow(
                () -> new AssertionError("Synthetic module 'local.Proj.A' must be present"));
    assertThat("Parent module is synthetic", parentMod.isSynthetic(), is(true));
    var moduleRefs = parentMod.getDirectModulesRefs();
    assertThat(
        "Has single direct module reference",
        moduleRefs,
        is(List.of(QualifiedName.fromString("local.Proj.A.B"))));
  }

  private void assertSuccessfulCompilation(CompilerResult result) {
    var compiledModules = result.compiledModules();
    assertThat("Compiled modules are not empty", compiledModules.size(), is(greaterThan(1)));
  }
}
