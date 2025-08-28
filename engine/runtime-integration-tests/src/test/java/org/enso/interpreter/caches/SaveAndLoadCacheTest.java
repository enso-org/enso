package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

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

public class SaveAndLoadCacheTest {

  @Rule public final TemporaryFolder tmpFolder = new TemporaryFolder();

  @Test
  public void compilationSavesSuggestionsAndImportExportCache() throws Exception {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject("Proj", """
        method =
            42
        """, projDir);
    try (var ctx = projCtx(projDir)) {
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
    try (var ctx = projCtx(projDir)) {
      compileAndAssertCreatedCaches(ctx, libName);
    }

    // Second, run the project. Caches should be loaded.
    try (var ctx = projCtx(projDir)) {
      var res = runMain(ctx, projDir);
      assertThat("execution is OK", res.asInt(), is(42));
      var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
      assertContainsEvent(
          "load bindings cache",
          cacheEvents,
          e -> isImportExportCacheEvent(e, libName) && e instanceof CacheEvent.Load);
    }
  }

  @Test
  public void bindingCachesOfBigProject_AreMmapped() throws IOException {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    var mainSrc = createBigSource(6_000);
    ProjectUtils.createProject("Proj", mainSrc, projDir);
    var libName = LibraryName.apply("local", "Proj");

    int bindingsCacheSize;
    try (var ctx = projCtx(projDir)) {
      compileAndAssertCreatedCaches(ctx, libName);
      var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
      assertThat(cacheEvents, is(notNullValue()));
      var bindingCacheSave =
          cacheEvents.stream()
              .filter(e -> isImportExportCacheEvent(e, libName))
              .map(e -> (CacheEvent.Save) e)
              .findFirst()
              .orElseThrow(() -> new AssertionError("No binding cache events found"));
      bindingsCacheSize = bindingCacheSave.size();
      var savedMb = bindingCacheSave.size() / 1024 / 1024;
      assertThat("binding cache is at least 10MB", savedMb > 10, is(true));
    }

    // Run after compilation. Bindings cache should be mmapped.
    try (var ctx = projCtx(projDir)) {
      var res = runMain(ctx, projDir);
      assertThat("execution is OK", res.asInt(), is(42));
      var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
      var mmapLoad =
          cacheEvents.stream()
              .filter(e -> e instanceof CacheEvent.MmapLoad)
              .map(e -> (CacheEvent.MmapLoad) e)
              .findFirst()
              .orElseThrow(() -> new AssertionError("No mmap load events found"));
      assertThat("Loaded same cached as previously saved", mmapLoad.size(), is(bindingsCacheSize));
    }
  }

  private static ContextUtils projCtx(Path projDir) {
    return ContextUtils.newBuilder()
        .withModifiedContext(
            bldr ->
                bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                    .option(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION, "false")
                    .option(RuntimeOptions.ENABLE_CACHE_STATS, "true"))
        .withProjectRoot(projDir)
        .build();
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

  /** Creates executable big source file. */
  private static String createBigSource(int methodCount) {
    var sb = new StringBuilder();
    sb.append("""
        method_0 =
            42
        """);
    for (var i = 1; i < methodCount; i++) {
      sb.append("\n");
      sb.append("method_").append(i).append(" = \n");
      sb.append("    method_0");
      sb.append("\n");
    }
    sb.append("main = \n");
    sb.append("    ").append("method_").append(methodCount - 1).append("\n");
    return sb.toString();
  }
}
