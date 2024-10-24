package org.enso.common;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
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
}
