package org.enso.compiler;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.SimpleFormatter;

import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.PackageManager;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;

public class ExecSimpleTest {
  private final MockHandler mockHandler = new MockHandler();

  public Context ensoContextForPackage(String name, File pkgFile, boolean disableIrCaching)
      throws IOException {
    Context ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(true)
            .option(RuntimeOptions.PROJECT_ROOT, pkgFile.getAbsolutePath())
            .option(RuntimeOptions.DISABLE_IR_CACHES, "" + disableIrCaching)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .logHandler(mockHandler)
            .option("log.enso.org.enso.compiler.Compiler.level", "FINE")
            .allowAllAccess(true)
            .build();
    assertNotNull("Enso language is supported", ctx.getEngine().getLanguages().get("enso"));
    return ctx;
  }

  @Test
  public void testSerializationOfFQNs() throws Exception {
    var testName = "Fib_Test";
    var pkgPath = new File(getClass().getClassLoader().getResource(testName).getPath());
    var pkg = PackageManager.Default().fromDirectory(pkgPath).get();
    try (org.graalvm.polyglot.Context ctx = ensoContextForPackage(testName, pkgPath, true)) {
      var ensoContext =
          (EnsoContext)
              ctx.getBindings(LanguageInfo.ID)
                  .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                  .asHostObject();
      var module = ensoContext.getModuleForFile(pkg.mainFile()).get();
      var compiler = ensoContext.getCompiler();

      ctx.enter();
      var result = compiler.run(module);
      assertEquals("Two library modules are compiled", result.compiledModules().size(), 2);
      assertEquals(result.compiledModules().exists(m -> m == module), true);
      var serializationManager = ensoContext.getCompiler().getSerializationManager();
      var future = serializationManager.serializeModule(module, true, true);
      var serialized = future.get(5, TimeUnit.SECONDS);
      assertEquals(serialized, true);
      var future2 = compiler.compile(false, true);
      var persisted = future2.get(5, TimeUnit.SECONDS);
      assertEquals("Fib_Test library has been persisted", true, persisted);
      ctx.leave();
    }

    mockHandler.failOnMessage("Parsing module [local.Fib_Test.Arith].");

    try (org.graalvm.polyglot.Context ctx = ensoContextForPackage(testName, pkgPath, false)) {
      var ensoContext =
          (EnsoContext)
              ctx.getBindings(LanguageInfo.ID)
                  .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                  .asHostObject();
      var module = ensoContext.getModuleForFile(pkg.mainFile()).get();
      var compiler = ensoContext.getCompiler();

      ctx.enter();
      var result = compiler.run(module);

      mockHandler.assertNoFailureMessage();
      assertEquals(
         "Only main library module is compiled: " + result.compiledModules(),
         result.compiledModules().size(),
         1);
      assertEquals(result.compiledModules().exists(m -> m == module), true);

      ctx.leave();
    }
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
