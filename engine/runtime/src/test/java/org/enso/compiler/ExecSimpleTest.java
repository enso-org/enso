package org.enso.compiler;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.concurrent.TimeUnit;

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
  public Context ensoContextForPackage(String name, File pkgFile) throws IOException {
    Context ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .allowIO(true)
            .option(RuntimeOptions.PROJECT_ROOT, pkgFile.getAbsolutePath())
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .logHandler(OutputStream.nullOutputStream())
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
    try (org.graalvm.polyglot.Context ctx = ensoContextForPackage(testName, pkgPath)) {
        var ensoContext =
            (EnsoContext)
                ctx.getBindings(LanguageInfo.ID)
                    .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                    .asHostObject();
        var module = ensoContext.getModuleForFile(pkg.mainFile()).get();
        var compiler = ensoContext.getCompiler();

        ctx.enter();
        var result = compiler.run(module);
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
  }
}
