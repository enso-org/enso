package org.enso.common;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchKey;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

/**
 * Development support for running <a
 * href="https://www.graalvm.org/latest/tools/graalvm-insight/">GraalVM Insight</a> scripts in the
 * Enso execution enviroments. Works both - in the CLI as well as in IDE. To use specify JVM
 * property {@link #INSIGHT_PROP} when executing the CLI:
 *
 * <pre>
 * enso --vm.D=enso.dev.insight=insightScript.js
 * </pre>
 *
 * or when launching the {@code project-manager}:
 *
 * <pre>
 * ENSO_JVM_OPTS=-Denso.dev.insight=`pwd`/insightScript.js project-manager
 * </pre>
 *
 * The sample {@code insightScript.js} can look for example like:
 *
 * <pre>
 * print("Initializing Insight: " + insight);
 * insight.on("enter", function(ctx) {
 *   print("Calling " + ctx.name);
 * }, {
 *   roots: true
 * });
 * </pre>
 *
 * More information about Insight scripts can be found in the <a
 * href="https://www.graalvm.org/latest/tools/graalvm-insight/manual/">Insight manual</a> and <a
 * href="https://www.graalvm.org/tools/javadoc/org/graalvm/tools/insight/Insight.html">programatic
 * documentation</a>.
 */
final class ContextInsightSetup {
  private static final String INSIGHT_PROP = "enso.dev.insight";
  private static ContextInsightSetup ACTIVE;

  private final Context ctx;
  private final Path insightFile;
  private AutoCloseable insightHandle;

  private ContextInsightSetup(Context ctx, Path file) {
    this.ctx = ctx;
    this.insightFile = file;
  }

  private static final class InsightFileWatcher implements Runnable {

    private final Path insightFilePath;
    private final Runnable onChange;

    public InsightFileWatcher(Path insightFilePath, Runnable onChange) {
      this.insightFilePath = insightFilePath;
      this.onChange = onChange;
    }

    @Override
    public void run() {
      try {
        watch();
      } catch (IOException e) {
        new IOException("Error watching the insight file", e).printStackTrace();
      }
    }

    private void watch() throws IOException {
      final Path insightDirectory = insightFilePath.getParent();
      final Path insightFileName = insightFilePath.getFileName();
      FileTime lastRecordedModifiedTime = FileTime.fromMillis(0);

      try (final var watchService = FileSystems.getDefault().newWatchService()) {
        insightDirectory.register(
            watchService,
            StandardWatchEventKinds.ENTRY_CREATE,
            StandardWatchEventKinds.ENTRY_MODIFY);
        while (true) {
          WatchKey watchKey;
          try {
            watchKey = watchService.take();
          } catch (InterruptedException ignored) {
            return;
          }

          boolean isModified = false;
          for (var watchEvent : watchKey.pollEvents()) {
            if (watchEvent.kind() == StandardWatchEventKinds.OVERFLOW) {
              continue;
            }

            final Path eventPath = (Path) watchEvent.context();
            if (eventPath.endsWith(insightFileName)) {
              final Path insightFilePath = insightDirectory.resolve(eventPath);
              try {
                final BasicFileAttributes attributes =
                    Files.readAttributes(insightFilePath, BasicFileAttributes.class);
                if (attributes.lastModifiedTime().compareTo(lastRecordedModifiedTime) > 0) {
                  lastRecordedModifiedTime = attributes.lastModifiedTime();
                  isModified = true;
                }
              } catch (IOException ignored) {
                continue;
              }
            }
          }

          if (isModified) {
            onChange.run();
          }

          final boolean isValid = watchKey.reset();
          if (!isValid) {
            break;
          }
        }
      }
    }
  }

  /**
   * Configures the context if {@link #INSIGHT_PROP} property is specified. This support is
   * <em>development only</em>. It can be (and will be) removed in the future.
   *
   * @param ctx context to configure
   * @throws AssertionError throws assertion error if the property is specified, but something goes
   *     wrong
   */
  @SuppressWarnings("CallToPrintStackTrace")
  static void configureContext(Context ctx) throws AssertionError {
    var insightProp = System.getProperty(INSIGHT_PROP);
    if (insightProp != null) {
      var insightFile = new File(insightProp);
      try {
        insightFile = insightFile.getCanonicalFile();
        assert insightFile.isFile()
            : "Cannot find " + insightFile + " specified via " + INSIGHT_PROP + " property";
        ACTIVE = new ContextInsightSetup(ctx, insightFile.toPath());
        ACTIVE.initialize();
        ACTIVE.startFileWatcherThread();
      } catch (Error | Exception ex) {
        var ae =
            new AssertionError(
                "Cannot initialize " + insightFile + " specified via " + INSIGHT_PROP + " property",
                ex);
        ae.printStackTrace();
        throw ae;
      }
    }
  }

  private void initialize() throws IOException {
    var insightCode = Files.readString(insightFile);
    var language = "js";
    if (insightFile.getFileName().toString().endsWith(".py")) {
      language = "python";
    }
    var insightSrc =
        Source.newBuilder("epb", insightFile.toFile())
            .content(language + ":0#" + insightCode)
            .build();

    var instrument = ctx.getEngine().getInstruments().get("insight");
    @SuppressWarnings("unchecked")
    var insight = (Function<Source, AutoCloseable>) instrument.lookup(Function.class);
    insightHandle = insight.apply(insightSrc);
  }

  private void reload() {
    try {
      if (insightHandle != null) {
        insightHandle.close();
      }
      initialize();
    } catch (Exception e) {
      new Exception("Error reloading the insight script", e).printStackTrace();
    }
  }

  private void startFileWatcherThread() {
    final InsightFileWatcher insightFileWatcher = new InsightFileWatcher(insightFile, this::reload);
    final Thread thread = new Thread(insightFileWatcher, "InsightFileWatcherThread");
    thread.setDaemon(true);
    thread.start();
  }
}
