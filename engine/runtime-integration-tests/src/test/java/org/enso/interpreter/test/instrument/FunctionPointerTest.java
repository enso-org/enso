package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.service.ExecutionService.FunctionPointer;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Test;

public class FunctionPointerTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Test
  public void moduleFunctionPointer() throws Exception {
    var rawCode =
        """
        from Standard.Base import all

        run a b = a + b
        """;
    var src = Source.newBuilder("enso", rawCode, "TestFunctionPointer.enso").build();
    var module = ctxRule.eval(src);
    var res = module.invokeMember("eval_expression", "run");

    assertTrue("fn: " + res, res.canExecute());
    var rawRes = ctxRule.unwrapValue(res);
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
    var module = ctxRule.eval(src);
    var res = module.invokeMember("eval_expression", "X.run");

    assertTrue("fn: " + res, res.canExecute());
    var rawRes = ctxRule.unwrapValue(res);
    assertTrue("function: " + rawRes, rawRes instanceof Function);
    var c = FunctionPointer.fromFunction((Function) rawRes);
    assertNotNull(c);
    assertEquals("StaticMethodPointer", c.moduleName().toString());
    assertEquals("StaticMethodPointer.X", c.typeName().toString());
    assertEquals("run", c.functionName().toString());

    var apply = res.execute(1);
    assertTrue("fn: " + apply, apply.canExecute());
    var rawApply = ctxRule.unwrapValue(res);
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
    var module = ctxRule.eval(src);
    var res = module.invokeMember("eval_expression", "X.run");

    assertTrue("fn: " + res, res.canExecute());
    var rawRes = ctxRule.unwrapValue(res);
    assertTrue("function: " + rawRes, rawRes instanceof Function);
    var c = FunctionPointer.fromFunction((Function) rawRes);
    assertNotNull(c);
    assertEquals("InstanceMethodPointer", c.moduleName().toString());
    assertEquals("InstanceMethodPointer.X", c.typeName().toString());
    assertEquals("run", c.functionName().toString());

    var apply = res.execute(1);
    assertTrue("fn: " + apply, apply.canExecute());
    var rawApply = ctxRule.unwrapValue(res);
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
    var module = ctxRule.eval(src);
    var res = module.invokeMember("eval_expression", "X.Run");

    assertTrue("fn: " + res, res.canInstantiate());
    var rawRes = ctxRule.unwrapValue(res);
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
