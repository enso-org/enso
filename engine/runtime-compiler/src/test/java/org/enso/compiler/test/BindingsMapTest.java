package org.enso.compiler.test;

import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertTrue;

import java.util.List;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.test.mock.WithCompilerContext;
import org.enso.pkg.QualifiedName;
import org.junit.Rule;
import org.junit.Test;

/**
 * Prefer to add tests for {@link org.enso.compiler.data.BindingsMap} here, as opposed to {@code
 * org.enso.compiler.test.BindingsMapResolutionTest}, which is inside {@code
 * runtime-integration-tests}. This is because this class uses proper mock compiler infrastructure,
 * unlike tests in {@code runtime-integration-tests}.
 */
public final class BindingsMapTest {
  @Rule public final WithCompilerContext compilerCtx = WithCompilerContext.createDefault();

  @Test
  public void foo() {
    compilerCtx.createModule(
        QualifiedName.fromString("local.Lib.A.A"),
        """
        static_method x = x
        """);
    compilerCtx.createModule(
        QualifiedName.fromString("local.Lib.Main"),
        """
        export project.A.A
        """);
    var projMainMod =
        compilerCtx.createModule(
            QualifiedName.fromString("local.Proj.Main"),
            """
            from local.Lib import A
            main =
                A.static_method 42
            """);
    compilerCtx.getCompiler().run(projMainMod);
    var projMainBm = projMainMod.getBindingsMap();

    var nameForResolution = asScala(List.of("local", "Lib", "A", "A", "static_method"));
    var res = projMainBm.resolveQualifiedName(nameForResolution);
    assertThat(res.isRight(), is(true));
    var resolvedNames = res.toOption().get();
    assertThat(resolvedNames.size(), is(1));
    assertTrue(resolvedNames.head() instanceof BindingsMap.ResolvedModuleMethod);
  }
}
