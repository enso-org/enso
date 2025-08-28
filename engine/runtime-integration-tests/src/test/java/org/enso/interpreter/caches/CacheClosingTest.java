package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.enso.common.RuntimeOptions;
import org.enso.editions.LibraryName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class CacheClosingTest {

  @Rule public final TemporaryFolder tmpFolder = new TemporaryFolder();

  @Test
  public void compilationSavesSuggestionsAndImportExportCache() throws Exception {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject("Proj", """
        main =
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
      var polyCtx = new PolyglotContext(ctx.context());
      polyCtx.getTopScope().compile(true);
      var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
      var libName = LibraryName.apply("local", "Proj");
      var hasSaveSuggestionCacheEvent =
          cacheEvents.stream()
              .anyMatch(e -> isSuggestionCacheEvent(e, libName) && e instanceof CacheEvent.Save);
      var hasSaveImportExportCacheEvent =
          cacheEvents.stream()
              .anyMatch(e -> isImportExportCacheEvent(e, libName) && e instanceof CacheEvent.Save);
      assertThat(
          "There should be a save event for SuggestionsCache",
          hasSaveSuggestionCacheEvent,
          is(true));
      assertThat(
          "There should be a save event for ImportExportCache",
          hasSaveImportExportCacheEvent,
          is(true));
    }
  }

  private static boolean isSuggestionCacheEvent(CacheEvent event, LibraryName libName) {
    return event.cacheName().contains("Suggestions")
        && event.cacheName().contains(libName.toString());
  }

  private static boolean isImportExportCacheEvent(CacheEvent event, LibraryName libName) {
    return libName.toString().equals(event.cacheName());
  }
}
