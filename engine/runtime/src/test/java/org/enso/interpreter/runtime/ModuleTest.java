package org.enso.interpreter.runtime;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.logging.Level;

import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.interpreter.runtime.Module;
import static org.enso.interpreter.util.ScalaConversions.nil;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

import scala.Option;

public class ModuleTest {

  private File f;
  private Context ctx;

  @Before
  public void prepareTest() throws IOException {

    f = File.createTempFile("module-sources", ".enso");
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    this.ctx = Context.newBuilder().engine(eng).allowIO(IOAccess.ALL).allowAllAccess(true).build();
  }

  @After
  public void cleanup() {
    f.delete();
    this.ctx.close();
  }

  @Test
  public void moduleKeepsFileRefAfterSourceUnset() {
    var name = QualifiedName.simpleName("local.Unnamed_1");
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);
    var module = new Module(name, null, tFile);
    assertTrue(
        "getPath is non-null", tFile.getPath() != null && module.getPath() == tFile.getPath());
    module.unsetLiteralSource();
    assertTrue(
        "getPath is non-null", tFile.getPath() != null && module.getPath() == tFile.getPath());
  }

  @Test
  public void updaterCanNullTheBindings() throws Exception {
    var name = QualifiedName.simpleName("SimpleExample");
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);

    var code = Source.newBuilder("enso", """
    main = 42
    """, name.toString()).build();

    ctx.eval(code);
    var module = ensoContext.getTopScope().getModule(name.toString()).get().asCompilerModule();
    ctx.enter();
    var compilerContext = ensoContext.getCompiler().context();

    assertNull("No bindings map by default", module.getBindingsMap());

    var bindings = new BindingsMap(nil(), new BindingsMap$ModuleReference$Concrete(module));
    compilerContext.updateModule(module, (u) -> {
      u.bindingsMap(bindings);
    });
    assertEquals("Bindings map has changed", bindings, module.getBindingsMap());

    compilerContext.updateModule(module, (u) -> {
      u.bindingsMap(null);
    });
    assertNull("No bindings map again", module.getBindingsMap());
  }

  @Test
  public void updaterCanNullTheIR() throws Exception {
    var name = QualifiedName.simpleName("AnotherSimpleExample");
    var ensoContext =
        (EnsoContext)
            ctx.getBindings(LanguageInfo.ID)
                .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
                .asHostObject();
    var tFile = ensoContext.getTruffleFile(f);

    var code = Source.newBuilder("enso", """
    main = 42
    """, name.toString()).build();

    ctx.eval(code);
    var module = ensoContext.getTopScope().getModule(name.toString()).get().asCompilerModule();
    ctx.enter();
    var compilerContext = ensoContext.getCompiler().context();

    assertNull("No IR by default", module.getIr());

    var ir = new org.enso.compiler.core.ir.Module(nil(), nil(), nil(), false, Option.empty(), null, null);
    compilerContext.updateModule(module, (u) -> {
      u.ir(ir);
    });
    assertEquals("IR has changed", ir, module.getIr());

    compilerContext.updateModule(module, (u) -> {
      u.ir(null);
    });
    assertNull("No IR again", module.getIr());
  }
}
