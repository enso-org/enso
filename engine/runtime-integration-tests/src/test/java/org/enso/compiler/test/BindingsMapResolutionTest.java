package org.enso.compiler.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Set;
import java.util.function.Consumer;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolutionError;
import org.enso.compiler.data.BindingsMap.ResolvedName;
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
import scala.util.Either;

public class BindingsMapResolutionTest {

  @ClassRule public static final TemporaryFolder TMP_DIR = new TemporaryFolder();

  @Test
  public void resolveSingleName_FromSingleImport() throws IOException {
    var projDir = createProject("import local.Proj.My_Vector.My_Vector");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Vector");
        });
  }

  @Test
  public void resolveSingleName_FromSingleImportWithFrom() throws IOException {
    var projDir = createProject("from local.Proj.My_Vector import My_Vector");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "My_Vector");
        });
  }

  @Test
  public void resolveQualifiedName_FromSingleImport() throws IOException {
    var projDir = createProject("import local.Proj.My_Vector.My_Vector");
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "local.Proj.My_Vector.My_Vector");
        });
  }

  @Test
  public void resolveQualifiedName_FromSingleImport_TwoProjects() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    var libDir = tmpDir.toPath().resolve("Lib");
    var projDir = tmpDir.toPath().resolve("Proj");
    libDir.toFile().mkdir();
    projDir.toFile().mkdir();
    ProjectUtils.createProject(
        "Lib",
        Set.of(new SourceModule(QualifiedName.fromString("My_Vector"), "type My_Vector")),
        libDir);
    ProjectUtils.createProject(
        "Proj",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Main"), "import local.Lib.My_Vector.My_Vector")),
        projDir);
    testBindingsMap(
        projDir,
        bindingsMap -> {
          assertSingleResolvedType(bindingsMap, "local.Proj.My_Vector.My_Vector");
        });
  }

  private Path createProject(String mainModuleSrc) throws IOException {
    var projDir = TMP_DIR.newFolder().toPath();
    var modules =
        Set.of(
            new SourceModule(QualifiedName.fromString("Main"), mainModuleSrc),
            new SourceModule(
                QualifiedName.fromString("My_Vector"),
                """
                    type My_Vector
                    """));
    ProjectUtils.createProject("Proj", modules, projDir);
    return projDir;
  }

  private static void assertSingleResolvedType(BindingsMap bindingsMap, String typeName) {
    assertThat("Has resolved import", bindingsMap.resolvedImports().size(), is(1));
    Either<ResolutionError, scala.collection.immutable.List<ResolvedName>> resolution;
    if (typeName.contains(".")) {
      var fqn = Arrays.stream(typeName.split("\\.")).toList();
      resolution = bindingsMap.resolveQualifiedName(toScalaList(fqn));
    } else {
      resolution = bindingsMap.resolveName(typeName);
    }
    assertThat("Type '" + typeName + "' is resolved", resolution.isRight(), is(true));
    var resolvedNames = resolution.toOption().get();
    assertThat("single resolution found", resolvedNames.size(), is(1));
    assertThat("is ResolvedType", resolvedNames.head() instanceof ResolvedType, is(true));
  }

  private static void testBindingsMap(Path projDir, Consumer<BindingsMap> callback) {
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
