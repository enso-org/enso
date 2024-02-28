package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Paths;
import java.util.Map;
import java.util.logging.Level;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.service.ExecutionService.FunctionPointer;
import org.enso.interpreter.test.TestBase;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class FunctionPointerTest extends TestBase {

  private Context context;

  @Before
  public void initContext() {
    context =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .build();

    var engine = context.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @After
  public void disposeContext() {
    context.close();
  }

  @Test
  public void moduleFunctionPointer() throws Exception {
    var rawCode = """
        from Standard.Base import all

        run a b = a + b
        """;
    var src = Source.newBuilder("enso", rawCode, "TestFunctionPointer.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "run");

    assertTrue("fn: " + res, res.canExecute());
    var rawRes = TestBase.unwrapValue(context, res);
    assertTrue("function: " + rawRes, rawRes instanceof Function);
    var c = FunctionPointer.fromFunction((Function) rawRes);
    assertNotNull(c);
    assertEquals("TestFunctionPointer", c.moduleName().toString());
    assertEquals("TestFunctionPointer", c.typeName().toString());
    assertEquals("run", c.functionName().toString());
  }

  @Test
  public void typeStaticMethodPointer() throws Exception {
    var rawCode =
        """
        from Standard.Base import all

        type X
            run a b = a + b
        """;
    var src = Source.newBuilder("enso", rawCode, "StaticMethodPointer.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "X.run");

    assertTrue("fn: " + res, res.canExecute());
    var rawRes = TestBase.unwrapValue(context, res);
    assertTrue("function: " + rawRes, rawRes instanceof Function);
    var c = FunctionPointer.fromFunction((Function) rawRes);
    assertNotNull(c);
    assertEquals("StaticMethodPointer", c.moduleName().toString());
    assertEquals("StaticMethodPointer.X", c.typeName().toString());
    assertEquals("run", c.functionName().toString());

    var apply = res.execute(1);
    assertTrue("fn: " + apply, apply.canExecute());
    var rawApply = TestBase.unwrapValue(context, res);
    assertTrue("function: " + rawApply, rawApply instanceof Function);
    var a = FunctionPointer.fromFunction((Function) rawApply);
    assertNotNull(a);
    assertEquals("StaticMethodPointer", a.moduleName().toString());
    assertEquals("StaticMethodPointer.X", a.typeName().toString());
    assertEquals("run", a.functionName().toString());
  }

  @Test
  public void typeInstanceMethodPointer() throws Exception {
    var rawCode =
        """
        from Standard.Base import all

        type X
            run self b c = [self, b, c]
        """;
    var src = Source.newBuilder("enso", rawCode, "InstanceMethodPointer.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "X.run");

    assertTrue("fn: " + res, res.canExecute());
    var rawRes = TestBase.unwrapValue(context, res);
    assertTrue("function: " + rawRes, rawRes instanceof Function);
    var c = FunctionPointer.fromFunction((Function) rawRes);
    assertNotNull(c);
    assertEquals("InstanceMethodPointer", c.moduleName().toString());
    assertEquals("InstanceMethodPointer.X", c.typeName().toString());
    assertEquals("run", c.functionName().toString());

    var apply = res.execute(1);
    assertTrue("fn: " + apply, apply.canExecute());
    var rawApply = TestBase.unwrapValue(context, res);
    assertTrue("function: " + rawApply, rawApply instanceof Function);
    var a = FunctionPointer.fromFunction((Function) rawApply);
    assertNotNull(a);
    assertEquals("InstanceMethodPointer", a.moduleName().toString());
    assertEquals("InstanceMethodPointer.X", a.typeName().toString());
    assertEquals("run", a.functionName().toString());
  }

  @Test
  public void typeConstructorPointer() throws Exception {
    var rawCode =
        """
        from Standard.Base import all

        type X
            Run a b
        """;
    var src = Source.newBuilder("enso", rawCode, "ConstructorPointer.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "X.Run");

    assertTrue("fn: " + res, res.canInstantiate());
    var rawRes = TestBase.unwrapValue(context, res);
    assertTrue("function: " + rawRes.getClass(), rawRes instanceof AtomConstructor);
    var rawFn = ((AtomConstructor) rawRes).getConstructorFunction();
    var c = FunctionPointer.fromFunction(rawFn);
    assertNotNull("We should get a pointer for " + rawFn, c);

    assertEquals("ConstructorPointer", c.moduleName().toString());
    assertEquals("ConstructorPointer.X", c.typeName().toString());
    assertEquals("Run", c.functionName());

    var d = FunctionPointer.fromAtomConstructor((AtomConstructor) rawRes);
    assertNotNull("We should get a pointer from " + rawRes, d);

    assertEquals("ConstructorPointer", d.moduleName().toString());
    assertEquals("ConstructorPointer.X", d.typeName().toString());
    assertEquals("Run", d.functionName());
  }
}
