package org.enso.compiler.benchmarks;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.EnsoContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;

public final class Utils {
  private Utils() {}

  public static Context.Builder createDefaultContextBuilder() {
    return Context.newBuilder()
        .allowExperimentalOptions(true)
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath())
        .option(RuntimeOptions.LOG_LEVEL, Level.INFO.getName())
        .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
        .option(RuntimeOptions.STRICT_ERRORS, "true")
        .option("engine.CompilationFailureAction", "Print")
        // MultiTier = false does not affect the peak performance, and the graphs generated from
        // that are easier to inspect in IGV.
        .option("engine.MultiTier", "false")
        .logHandler(System.err)
        .allowIO(IOAccess.ALL)
        .allowAllAccess(true);
  }

  public static EnsoContext leakEnsoContext(Context ctx) {
    return ctx.getBindings(LanguageInfo.ID)
        .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
        .as(EnsoContext.class);
  }

  public static Object unwrapReceiver(Context ctx, Value value) {
    var unwrapper = new Unwrapper();
    var unwrapperValue = ctx.asValue(unwrapper);
    unwrapperValue.execute(value);
    return unwrapper.args[0];
  }

  public static File createSrcFile(String code, String name) {
    var benchDataDir = Path.of(".", "target", "bench-data");
    benchDataDir.toFile().mkdirs();
    var srcFile = benchDataDir.resolve(name).toFile();
    try {
      Files.writeString(srcFile.toPath(), code);
    } catch (IOException e) {
      throw new AssertionError(e);
    }
    return srcFile;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class Unwrapper implements TruffleObject {
    Object[] args;

    @ExportMessage
    Object execute(Object[] args) {
      this.args = args;
      return this;
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }
}
