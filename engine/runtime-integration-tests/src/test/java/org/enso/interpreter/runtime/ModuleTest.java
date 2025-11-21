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
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap$ModuleReference$Concrete;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.junit.After;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Test;

public class ModuleTest {

  private File f;

  @ClassRule
  public static final ContextUtils ctxRule =
      ContextUtils.newBuilder()
          .withModifiedContext(b -> b.option(RuntimeOptions.STRICT_ERRORS, "false"))
          .build();

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
    var ensoContext = ctxRule.ensoContext();
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
    var ensoContext = ctxRule.ensoContext();
    var tFile = ensoContext.getTruffleFile(f);

    var code =
        Source.newBuilder(
                "enso",
                """
                main = 42
                """,
                name.toString())
            .build();

    ctxRule.eval(code);
    var module = ensoContext.getTopScope().getModule(name.toString()).get().asCompilerModule();
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
    var ensoContext = ctxRule.ensoContext();
    var tFile = ensoContext.getTruffleFile(f);

    var code =
        Source.newBuilder(
                "enso",
                """
                main = 42
                """,
                name.toString())
            .build();

    ctxRule.eval(code);
    var module = ensoContext.getTopScope().getModule(name.toString()).get().asCompilerModule();
    var compilerContext = ensoContext.getCompiler().context();

    assertNull("No IR by default", module.getIr());

    var ir =
        new org.enso.compiler.core.ir.Module(
            nil(), nil(), nil(), false, null, new MetadataStorage(), null);
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
