package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Context;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import scala.jdk.CollectionConverters;

public class BindingsMapResolutionTest {
  @ClassRule public static final TemporaryFolder TMP_DIR = new TemporaryFolder();

  @Test
  public void resolveSingleName_FromSingleImport() throws IOException {
    var src = """
        import Standard.Base.Data.Vector.Vector
        """;
    testBindingsMap(
        src,
        bindingsMap -> {
          assertThat("Has resolved import", bindingsMap.resolvedImports().size(), is(1));
          var resolution = bindingsMap.resolveName("Vector");
          assertThat("Vector is resolved", resolution.isRight(), is(true));
          var resolvedNames = resolution.toOption().get();
          assertThat("single resolution found", resolvedNames.size(), is(1));
          assertThat("is ResolvedType", resolvedNames.head() instanceof ResolvedType, is(true));
        });
  }

  @Test
  public void resolveQualifiedName_FromSingleImport() throws IOException {
    var src = """
        import Standard.Base.Data.Vector.Vector
        """;
    testBindingsMap(
        src,
        bindingsMap -> {
          assertThat("Has resolved import", bindingsMap.resolvedImports().size(), is(1));
          var resolution =
              bindingsMap.resolveQualifiedName(
                  toScalaList(List.of("Standard", "Base", "Data", "Vector", "Vector")));
          assertThat("Vector is resolved", resolution.isRight(), is(true));
          var resolvedNames = resolution.toOption().get();
          assertThat("single resolution found", resolvedNames.size(), is(1));
          assertThat("is ResolvedType", resolvedNames.head() instanceof ResolvedType, is(true));
        });
  }

  @Test
  public void resolveSingleName_FromSingleImportWithFrom() throws IOException {
    var src = """
        from Standard.Base.Data.Vector import Vector
        """;
    testBindingsMap(
        src,
        bindingsMap -> {
          assertThat("Has resolved import", bindingsMap.resolvedImports().size(), is(1));
          var resolution = bindingsMap.resolveName("Vector");
          assertThat("Vector is resolved", resolution.isRight(), is(true));
          var resolvedNames = resolution.toOption().get();
          assertThat("single resolution found", resolvedNames.size(), is(1));
          assertThat("is ResolvedType", resolvedNames.head() instanceof ResolvedType, is(true));
        });
  }

  /**
   * Compiles the given module source and gets the {@link BindingsMap} from it. Which is passed to
   * the {@code callback}.
   */
  private static void testBindingsMap(String moduleSrc, Consumer<BindingsMap> callback)
      throws IOException {
    var projDir = TMP_DIR.newFolder().toPath();
    var mainSrcMod =
        new SourceModule(
            QualifiedName.fromString("Main"),
            """
        from Standard.Base import all
        main = 42
        """);
    ProjectUtils.createProject("Proj", Set.of(mainSrcMod), projDir);
    try (var ctx = createCtx(projDir)) {
      compile(ctx);
      var bm = getBindingsMap(ctx, "local.Proj.Main");
      assertThat(bm, is(notNullValue()));
      callback.accept(bm);
    }
  }

  // TODO: Extract from ExportedSymbolsTest
  private static Context createCtx(Path projDir) {
    return ContextUtils.defaultContextBuilder()
        .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
        .build();
  }

  // TODO: Extract
  private static void compile(Context ctx) {
    new PolyglotContext(ctx).getTopScope().compile(true);
  }

  private static BindingsMap getBindingsMap(Context ctx, String moduleName) {
    var ensoCtx = ContextUtils.leakContext(ctx);
    var mod = ensoCtx.getPackageRepository().getLoadedModule(moduleName).get();
    assert mod != null;
    return mod.getBindingsMap();
  }

  private static <T> scala.collection.immutable.List<T> toScalaList(java.util.List<T> list) {
    return CollectionConverters.ListHasAsScala(list).asScala().toList();
  }
}
