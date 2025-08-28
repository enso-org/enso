package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Path;
import java.util.function.Predicate;
import org.enso.common.RuntimeOptions;
import org.enso.editions.LibraryName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.Value;
import org.hamcrest.CustomMatcher;
import org.hamcrest.Matcher;
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
      var polyCtx = new PolyglotContext(ctx.context());
      polyCtx.getTopScope().compile(true);
      var cacheEvents = ctx.ensoContext().getCacheStatistics().getCacheEvents();
      var libName = LibraryName.apply("local", "Proj");
      var saveSuggestionEventMatcher =
          eventMatcher(
              "save suggestions cache",
              e -> isSuggestionCacheEvent(e, libName) && e instanceof CacheEvent.Save);
      var saveBindingsCacheMatcher =
          eventMatcher(
              "save import/export cache",
              e -> isImportExportCacheEvent(e, libName) && e instanceof CacheEvent.Save);
      assertThat(cacheEvents, hasItem(saveSuggestionEventMatcher));
      assertThat(cacheEvents, hasItem(saveBindingsCacheMatcher));
    }
  }
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

  private static Matcher<CacheEvent> eventMatcher(String descr, Predicate<CacheEvent> predicate) {
    return new CustomMatcher<>(descr) {
      @Override
      public boolean matches(Object item) {
        if (item instanceof CacheEvent event) {
          return predicate.test(event);
        } else {
          return false;
        }
      }
    };
  }
}
