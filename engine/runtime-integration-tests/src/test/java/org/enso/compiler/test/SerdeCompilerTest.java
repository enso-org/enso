package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.Module;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.PackageManager;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.Test;

public class SerdeCompilerTest {
  private final MockHandler mockHandler = new MockHandler();

  @Test
  public void testFibTest() throws Exception {
    var testName = "Fib_Test";
    final String forbiddenMessage = null; // "Parsing module [local.Fib_Test.Arith].";
    parseSerializedModule(testName, forbiddenMessage);
  }

  private void parseSerializedModule(String projectName, String forbiddenMessage)
      throws InterruptedException, ExecutionException, IOException, TimeoutException {
    Module old;
    var pkgPath = new File(getClass().getClassLoader().getResource(projectName).getPath());
    var pkg = PackageManager.Default().fromDirectory(pkgPath).get();
    try (org.graalvm.polyglot.Context ctx = ensoContextForPackage(projectName, pkgPath, true)) {
      var ensoContext =
          (EnsoContext)
              ctx.getBindings(LanguageInfo.ID)
                  .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                  .asHostObject();
      var module = ensoContext.getModuleForFile(pkg.mainFile()).get().asCompilerModule();
      var compiler = ensoContext.getCompiler();

      ctx.enter();
      var result = compiler.run(module);
      var nonStandard =
          result
              .compiledModules()
              .filter((m) -> !m.getPackage().libraryName().namespace().equals("Standard"));
      assertEquals("Two non-standard library modules are compiled", nonStandard.size(), 2);
      assertEquals(result.compiledModules().exists(m -> m == module), true);
      var futures = new ArrayList<Future<?>>();
      result
          .compiledModules()
          .filter((m) -> !m.isSynthetic())
          .foreach(
              (m) -> {
                var future = compiler.context().serializeModule(compiler, m, true, true);
                futures.add(future);
                return null;
              });
      futures.add(compiler.compile(false, true, true));
      for (var f : futures) {
        var persisted = f.get(10, TimeUnit.SECONDS);
        assertEquals("Fib_Test library has been fully persisted", true, persisted);
      }
      old = module.getIr().duplicate(true, true, true, false);
      ctx.leave();
    }

    Module now;
    mockHandler.failOnMessage(forbiddenMessage);

    try (org.graalvm.polyglot.Context ctx = ensoContextForPackage(projectName, pkgPath, false)) {
      var ensoContext =
          (EnsoContext)
              ctx.getBindings(LanguageInfo.ID)
                  .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                  .asHostObject();
      var module = ensoContext.getModuleForFile(pkg.mainFile()).get().asCompilerModule();
      var compiler = ensoContext.getCompiler();

      ctx.enter();
      var result = compiler.run(module);
      mockHandler.assertNoFailureMessage();
      assertEquals(result.compiledModules().exists(m -> m == module), true);

      var methods =
          org.enso.interpreter.runtime.Module.fromCompilerModule(module).getScope().getAllMethods();
      var main = methods.get(0);

      assertEquals("Main.main", main.getName());
      var mainValue = ctx.asValue(main);
      assertEquals(42, mainValue.execute().asInt());

      now = module.getIr().duplicate(true, true, true, false);

      ctx.leave();
    }
    CompilerTests.assertIR("Serialized and deserialized IR for " + projectName, old, now);
  }

  private Context ensoContextForPackage(String name, File pkgFile, boolean disableIrCaching)
      throws IOException {
    Context ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .option(RuntimeOptions.PROJECT_ROOT, pkgFile.getAbsolutePath())
            .option(RuntimeOptions.DISABLE_IR_CACHES, "" + disableIrCaching)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(mockHandler)
            .option("log.enso.org.enso.compiler.Compiler.level", "FINE")
            .allowAllAccess(true)
            .build();
    assertNotNull("Enso language is supported", ctx.getEngine().getLanguages().get("enso"));
    return ctx;
  }

  private static class MockHandler extends Handler {
    private final Formatter fmt = new SimpleFormatter();
    private final List<LogRecord> records = new ArrayList<>();
    private String failMsg;
    private Error failure;

    public MockHandler() {}

    public void failOnMessage(String msg) {
      this.failMsg = msg;
    }

    @Override
    public void publish(LogRecord lr) {
      records.add(lr);
      var msg = fmt.formatMessage(lr);
      if (failMsg != null && failMsg.equals(msg)) {
        failure = new AssertionError(this.toString() + "\nGot forbidden message: " + msg);
      }
    }

    @Override
    public void flush() {}

    @Override
    public void close() throws SecurityException {}

    @Override
    public String toString() {
      var sb = new StringBuilder();
      for (var r : records) {
        sb.append("\n").append(fmt.formatMessage(r));
      }
      return sb.toString();
    }

    private void assertNoFailureMessage() {
      if (failure != null) {
        failure.printStackTrace();
        throw failure;
      }
    }
  }
}
