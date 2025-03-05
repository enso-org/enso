package org.enso.compiler.test.passes.resolve;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import java.util.function.Consumer;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.MetadataInteropHelpers;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.pass.resolve.FullyQualifiedNames;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.graalvm.polyglot.Context;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public final class FullyQualifiedNamesTest {

  @ClassRule public static final TemporaryFolder TMP_DIR = new TemporaryFolder();

  private Path projDir;

  @Before
  public void before() throws IOException {
    var tmpDir = TMP_DIR.newFolder();
    projDir = tmpDir.toPath().resolve("Proj");
    var wasCreated = projDir.toFile().mkdir();
    assert wasCreated;
  }

  @After
  public void after() {}

  @Test
  public void libraryNameIsResolved_InExpressionBlock() throws IOException {
    String mainSrc = """
        main =
            local.Proj.My_Module.My_Type
        """;
    createProject(Set.of(srcModule("Main", mainSrc)));
    try (var ctx = createCtx()) {
      compileAllModules(ctx);
      var modIr = getModuleIr(ctx, "local.Proj.Main");
      var location = getLocationOf(mainSrc, "Proj");
      var ir = findIrByLocation(modIr, location);
      assertHasFQNMetadata(
          ir,
          FullyQualifiedNames.ResolvedModule.class,
          meta -> {
            assertThat(meta.moduleRef().getName().toString(), is("local.Proj.Main"));
          });
    }
  }

  private <T> void assertHasFQNMetadata(IR ir, Class<T> metaTargetType, Consumer<T> callback) {
    var meta =
        MetadataInteropHelpers.getMetadataOrNull(
            ir, FullyQualifiedNames$.MODULE$, FullyQualifiedNames.FQNResolution.class);
    assertThat("Must have FullyQualifiedNames metadata", meta, is(notNullValue()));
    var isSameTargetClass = meta.target().getClass().isAssignableFrom(metaTargetType);
    assertThat(
        "Unexpected target meta type: " + meta.target().getClass().getName(),
        isSameTargetClass,
        is(true));
    var target = metaTargetType.cast(meta.target());
    callback.accept(target);
  }

  private Context createCtx() {
    return ContextUtils.defaultContextBuilder()
        .option(RuntimeOptions.PROJECT_ROOT, projDir.toFile().getAbsolutePath())
        .build();
  }

  private static SourceModule srcModule(String moduleName, String src) {
    return new SourceModule(QualifiedName.fromString(moduleName), src);
  }

  private void createProject(Set<SourceModule> srcModules) throws IOException {
    ProjectUtils.createProject("Proj", srcModules, projDir);
  }

  private void compileAllModules(Context ctx) {
    var polyCtx = new PolyglotContext(ctx);
    polyCtx.getTopScope().compile(true);
  }

  private Module getModuleIr(Context ctx, String moduleName) {
    var ensoCtx = ContextUtils.leakContext(ctx);
    var mod = ensoCtx.findModule(moduleName);
    assertThat(mod.isPresent(), is(true));
    var modIr = mod.get().getIr();
    assertThat(modIr, is(notNullValue()));
    return modIr;
  }

  private static Location getLocationOf(String moduleSrc, String expr) {
    assertThat("moduleSrc must contain expr", moduleSrc, containsString(expr));
    var start = moduleSrc.indexOf(expr);
    assert start != -1;
    var end = start + expr.length();
    return new Location(start, end);
  }

  private static IR findIrByLocation(IR root, Location location) {
    var ret =
        root.preorder()
            .find(
                ir -> {
                  var irLoc = ir.identifiedLocation();
                  if (irLoc != null) {
                    if (irLoc.location().equals(location)) {
                      return true;
                    }
                  }
                  return false;
                });
    assertThat(ret.isDefined(), is(true));
    return ret.get();
  }
}
