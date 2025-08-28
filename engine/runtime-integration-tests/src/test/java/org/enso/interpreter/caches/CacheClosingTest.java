package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Predicate;
import org.enso.common.RuntimeOptions;
import org.enso.editions.LibraryName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.Value;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class CacheClosingTest {

  @Rule public final TemporaryFolder tmpFolder = new TemporaryFolder();

  @Test
  public void compilationSavesSuggestionsAndImportExportCache() throws Exception {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject("Proj", """
        method =
            42
        """, projDir);
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION, "false")
                        .option(RuntimeOptions.ENABLE_CACHE_STATS, "true"))
            .withProjectRoot(projDir)
            .build()) {
      var libName = LibraryName.apply("local", "Proj");
      compileAndAssertCreatedCaches(ctx, libName);
    }
  }

  @Test
  public void cachesAreLoaded_AfterProjectIsCompiled() throws IOException {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject("Proj", """
        main =
            42
        """, projDir);
    var libName = LibraryName.apply("local", "Proj");

    // First, compile the project
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION, "false")
                        .option(RuntimeOptions.ENABLE_CACHE_STATS, "true"))
            .withProjectRoot(projDir)
            .build()) {
      compileAndAssertCreatedCaches(ctx, libName);
    }

    // Second, run the project. Caches should be loaded.
    try (var ctx =
        ContextUtils.newBuilder()
            .withModifiedContext(
                bldr ->
                    bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                        .option(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION, "false")
                        .option(RuntimeOptions.ENABLE_CACHE_STATS, "true"))
            .withProjectRoot(projDir)
            .build()) {
      var res = runMain(ctx, projDir);
      assertThat("execution is OK", res.asInt(), is(42));
      var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
      assertContainsEvent(
          "load bindings cache",
          cacheEvents,
          e -> isImportExportCacheEvent(e, libName) && e instanceof CacheEvent.Load);
    }
  }

  /**
   * Compiles the project and asserts that suggestions and import/export (binding) caches were
   * created (saved).
   */
  private static void compileAndAssertCreatedCaches(ContextUtils ctx, LibraryName libName) {
    var polyCtx = new PolyglotContext(ctx.context());
    polyCtx.getTopScope().compile(true);
    var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
    assertContainsEvent(
        "save suggestions cache",
        cacheEvents,
        e -> isSuggestionCacheEvent(e, libName) && e instanceof CacheEvent.Save);
    assertContainsEvent(
        "save import/export cache",
        cacheEvents,
        e -> isImportExportCacheEvent(e, libName) && e instanceof CacheEvent.Save);
  }

  private static Value runMain(ContextUtils ctx, Path projDir) {
    var polyCtx = new PolyglotContext(ctx.context());
    var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
    if (!mainSrcPath.toFile().exists()) {
      throw new IllegalArgumentException("Main module not found in " + projDir);
    }
    var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
    var assocMainModType = mainMod.getAssociatedType();
    var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
    var res = mainMethod.execute();
    return res;
  }

  private static boolean isSuggestionCacheEvent(CacheEvent event, LibraryName libName) {
    return event.cacheName().contains("Suggestions")
        && event.cacheName().contains(libName.toString());
  }

  private static boolean isImportExportCacheEvent(CacheEvent event, LibraryName libName) {
    return libName.toString().equals(event.cacheName());
  }

  private static void assertContainsEvent(
      String descr, List<CacheEvent> events, Predicate<CacheEvent> predicate) {
    var hasItem = events.stream().anyMatch(predicate);
    if (!hasItem) {
      throw new AssertionError("Expected to find event: " + descr + " in " + events);
    }
  }
}
