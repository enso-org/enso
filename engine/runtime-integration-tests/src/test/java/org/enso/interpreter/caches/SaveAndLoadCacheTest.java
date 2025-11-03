package org.enso.interpreter.caches;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import org.enso.common.RuntimeOptions;
import org.enso.editions.LibraryName;
import org.enso.polyglot.PolyglotContext;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class SaveAndLoadCacheTest {
  private static final String CACHE_LOGGER_NAME = "enso.org.enso.interpreter.caches.Cache";

  @Rule public final TemporaryFolder tmpFolder = new TemporaryFolder();
  private final List<LogRecord> collectedLogs = new ArrayList<>();
  private final LogHandler logHandler = new LogHandler();

  private final class LogHandler extends Handler {
    @Override
    public void publish(LogRecord record) {
      if (record.getLoggerName().equals(CACHE_LOGGER_NAME)) {
        collectedLogs.add(record);
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() {}
  }

  @After
  public void teardown() {
    collectedLogs.clear();
  }

  @Test
  public void compilationSavesSuggestionsAndImportExportCache() throws Exception {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        method =
            42
        """,
        projDir);
    var libName = LibraryName.apply("local", "Proj");
    try (var ctx = projCtx(projDir)) {
      compileAndAssertCreatedCaches(ctx, libName);
    }
  }

  @Test
  public void cachesAreLoaded_AfterProjectIsCompiled() throws IOException {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    ProjectUtils.createProject(
        "Proj",
        """
        main =
            42
        """,
        projDir);
    var libName = LibraryName.apply("local", "Proj");

    // First, compile the project
    try (var ctx = projCtx(projDir)) {
      compileAndAssertCreatedCaches(ctx, libName);
    }

    // Second, run the project. Caches should be loaded.
    try (var ctx = projCtx(projDir)) {
      var res = runMain(ctx, projDir);
      assertThat("execution is OK", res.asInt(), is(42));
      assertContainsLog(
          "load bindings cache", collectedLogs, log -> isLoadBindingsLog(log, libName));
    }
  }

  @Test
  public void bindingCachesOfBigProject_AreMmapped() throws IOException {
    var projDir = tmpFolder.newFolder("Proj").toPath();
    var mainSrc = createBigSource(6_000);
    ProjectUtils.createProject("Proj", mainSrc, projDir);
    var libName = LibraryName.apply("local", "Proj");

    int savedBindingsCacheSize;
    try (var ctx = projCtx(projDir)) {
      compileAndAssertCreatedCaches(ctx, libName);
      var saveBindingsLog =
          collectedLogs.stream()
              .filter(log -> isSaveBindingsLog(log, libName))
              .findFirst()
              .orElseThrow(() -> new AssertionError("No save bindings log found"));
      savedBindingsCacheSize = (int) saveBindingsLog.getParameters()[2];
      var savedMb = savedBindingsCacheSize / 1024 / 1024;
      assertThat("binding cache is at least 10MB", savedMb > 10, is(true));
    }

    // Run after compilation. Bindings cache should be mmapped.
    try (var ctx = projCtx(projDir)) {
      var res = runMain(ctx, projDir);
      assertThat("execution is OK", res.asInt(), is(42));
      var bindingsMmappedLog =
          collectedLogs.stream()
              .filter(
                  log -> {
                    var msg = log.getMessage();
                    return msg.contains("Cache") && msg.contains("mmapped");
                  })
              .findFirst()
              .orElseThrow(() -> new AssertionError("No load bindings log found"));
      var loadedBytes = (long) bindingsMmappedLog.getParameters()[1];
      assertThat(
          "Loaded same data as previously saved", (int) loadedBytes, is(savedBindingsCacheSize));
    }
  }

  private ContextUtils projCtx(Path projDir) {
    return ContextUtils.newBuilder()
        .withModifiedContext(
            bldr ->
                bldr.option(RuntimeOptions.DISABLE_IR_CACHES, "false")
                    .option(RuntimeOptions.LOG_LEVEL, "FINEST")
                    .logHandler(logHandler))
        .withProjectRoot(projDir)
        .build();
  }

  /**
   * Compiles the project and asserts that suggestions and import/export (binding) caches were
   * created (saved).
   */
  private void compileAndAssertCreatedCaches(ContextUtils ctx, LibraryName libName) {
    var polyCtx = new PolyglotContext(ctx.context());
    polyCtx.getTopScope().compile(true);
    assertThat("Some logs collected", collectedLogs.isEmpty(), is(false));
    assertContainsLog(
        "save suggestions cache", collectedLogs, log -> isSaveSuggestionsLog(log, libName));
    assertContainsLog(
        "save import/export cache", collectedLogs, log -> isSaveBindingsLog(log, libName));
  }

  private static void assertContainsLog(
      String descr, List<LogRecord> messages, Predicate<LogRecord> predicate) {
    var hasItem = messages.stream().anyMatch(predicate);
    if (!hasItem) {
      throw new AssertionError("Expected to find message: " + descr + " in " + messages);
    }
  }

  private static boolean hasParams(LogRecord log) {
    return log.getParameters() != null && log.getParameters().length > 0;
  }

  private static boolean isSaveSuggestionsLog(LogRecord log, LibraryName libName) {
    if (log.getMessage().contains("Written cache") && hasParams(log)) {
      if (log.getParameters()[0] instanceof String param) {
        return param.equals("Suggestions(" + libName + ")");
      }
    }
    return false;
  }

  private static boolean isSaveBindingsLog(LogRecord log, LibraryName libName) {
    if (log.getMessage().contains("Written cache") && hasParams(log)) {
      if (log.getParameters()[0] instanceof String param) {
        return param.equals(libName.toString());
      }
    }
    return false;
  }

  private static boolean isLoadBindingsLog(LogRecord log, LibraryName libName) {
    if (log.getMessage().contains("Loaded cache") && hasParams(log)) {
      if (log.getParameters()[0] instanceof String param) {
        return param.equals(libName.toString());
      }
    }
    return false;
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

  /** Creates big source file with main method. */
  private static String createBigSource(int methodCount) {
    var sb = new StringBuilder();
    sb.append(
        """
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
