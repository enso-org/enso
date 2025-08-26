package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.util.Set;
import org.enso.common.RuntimeOptions;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/** Test that source digest computation is avoided as much as possible. */
public final class SourceDigestTest {
  @Rule public final TemporaryFolder temporaryFolder = new TemporaryFolder();

  /**
   * Create empty project, compile it, and check that ImportExportBinding is OK. Ensure that no
   * source digest was computed
   */
  @Test
  public void noSourceDigestComputed_ForAlreadyCompiledLibrary() throws IOException {
    var libDir = temporaryFolder.newFolder("Lib").toPath();
    ProjectUtils.createProject(
        "Lib",
        Set.of(
            new SourceModule(
                QualifiedName.fromString("Utils"),
                """
                util_method x = x
                """),
            new SourceModule(
                QualifiedName.fromString("Main"),
                """
                from project.Utils import all
                lib_method y = util_method y
                """)),
        libDir);

    var projDir = temporaryFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
            from local.Lib import all
            main = lib_method 42
            """,
        projDir);
    // First, compile Lib
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.ENABLE_CACHE_COUNTERS, "true"))
            .withProjectRoot(libDir)
            .build()) {
      var polyCtx = new PolyglotContext(ctx.context());
      polyCtx.getTopScope().compile(true);
      var cacheCounters = ctx.ensoContext().getCacheCounters();
      var filesDigestComputed = cacheCounters.getFilesWithDigestComputation();
      assertThat(
          "During first compilation, some digests should be computed",
          filesDigestComputed.isEmpty(),
          is(false));
    }

    // Then, compile Proj with dependencies - Lib is already be compiled
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.ENABLE_CACHE_COUNTERS, "true"))
            .withProjectRoot(projDir)
            .build()) {
      var polyCtx = new PolyglotContext(ctx.context());
      polyCtx.getTopScope().compile(true);
      var cacheCounters = ctx.ensoContext().getCacheCounters();
      var filesDigestComputed = cacheCounters.getFilesWithDigestComputation();
      assertThat(filesDigestComputed.size(), is(0));
    }
  }
}
