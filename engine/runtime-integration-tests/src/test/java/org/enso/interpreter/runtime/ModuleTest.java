package org.enso.interpreter.runtime;

import static org.enso.scala.wrapper.ScalaConversions.nil;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextRule;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class ModuleTest {

  private File f;

  @ClassRule
  public static final ContextRule ctxRule = ContextRule.createCustom(ModuleTest::createCtx);

  @ClassRule
  public static final ContextRule myCtxRule = ContextRule.newBuilder()
      .withModifiedContext(ctxBldr -> {
        Engine eng =
            Engine.newBuilder()
                .allowExperimentalOptions(true)
                .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
                .option(RuntimeOptions.STRICT_ERRORS, "false")
                .logHandler(System.err)
                .option(
                    RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                    Paths.get("../../distribution/component").toFile().getAbsolutePath())
                .build();
        return ctxBldr.engine(eng).allowIO(IOAccess.ALL).allowAllAccess(true);
      })
      .build();

  private static Context createCtx() {
    Engine eng =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .option(RuntimeOptions.STRICT_ERRORS, "false")
            .logHandler(System.err)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .build();
    var ctx = Context.newBuilder().engine(eng).allowIO(IOAccess.ALL).allowAllAccess(true).build();
    return ctx;
  }

  @Before
  public void prepareTest() throws IOException {
    f = File.createTempFile("module-sources", ".enso");
  }

  @After
  public void cleanup() {
    f.delete();
  }

  @Test
  public void noSuchModuleError() {
    var b = ctxRule.context().getBindings(LanguageInfo.ID);
    try {
      var r = b.invokeMember(MethodNames.TopScope.GET_MODULE, "Does.Not.Exist.Module");
      fail("Expecting failure, but got: " + r);
    } catch (PolyglotException ex) {
      assertThat(ex.getMessage(), containsString("Module_Does_Not_Exist"));
    }
  }

  @Test
  public void moduleKeepsFileRefAfterSourceUnset() {
    var name = QualifiedName.simpleName("local.Unnamed_1");
    var ensoContext = ctxRule.leakContext();
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
    var ensoContext = ctxRule.leakContext();
    var tFile = ensoContext.getTruffleFile(f);

    var code = Source.newBuilder("enso", """
    main = 42
    """, name.toString()).build();

    ctxRule.eval(code);
    var module = ensoContext.getTopScope().getModule(name.toString()).get().asCompilerModule();
    ctxRule.context().enter();
    var compilerContext = ensoContext.getCompiler().context();

    assertNull("No bindings map by default", module.getBindingsMap());

    var bindings = new BindingsMap(nil(), new BindingsMap$ModuleReference$Concrete(module));
    compilerContext.updateModule(
        module,
        (u) -> {
          u.bindingsMap(bindings);
        });
    assertEquals("Bindings map has changed", bindings, module.getBindingsMap());

    compilerContext.updateModule(
        module,
        (u) -> {
          u.bindingsMap(null);
        });
    assertNull("No bindings map again", module.getBindingsMap());
  }

  @Test
  public void updaterCanNullTheIR() throws Exception {
    var name = QualifiedName.simpleName("AnotherSimpleExample");
    var ensoContext = ctxRule.leakContext();
    var tFile = ensoContext.getTruffleFile(f);

    var code = Source.newBuilder("enso", """
    main = 42
    """, name.toString()).build();

    ctxRule.eval(code);
    var module = ensoContext.getTopScope().getModule(name.toString()).get().asCompilerModule();
    ctxRule.context().enter();
    var compilerContext = ensoContext.getCompiler().context();

    assertNull("No IR by default", module.getIr());

    var ir = new org.enso.compiler.core.ir.Module(nil(), nil(), nil(), false, null, null);
    compilerContext.updateModule(
        module,
        (u) -> {
          u.ir(ir);
        });
    assertEquals("IR has changed", ir, module.getIr());

    compilerContext.updateModule(
        module,
        (u) -> {
          u.ir(null);
        });
    assertNull("No IR again", module.getIr());
  }
}
