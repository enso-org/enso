package org.enso.interpreter.node.expression.builtin.error;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.enso.interpreter.node.expression.foreign.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.TestRootNode;
import org.hamcrest.Matchers;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class CatchPanicNodeTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();
  private static CatchPanicNode catchPanicNode;
  private static HostValueToEnsoNode hostValueToEnsoNode;
  private static TestRootNode testRootNode;

  @BeforeClass
  public static void initContextAndData() {
    catchPanicNode = CatchPanicNode.build();
    hostValueToEnsoNode = HostValueToEnsoNode.build();
    testRootNode = new TestRootNode();
    testRootNode.insertChildren(catchPanicNode, hostValueToEnsoNode);
  }

  @AfterClass
  public static void disposeContext() {
    catchPanicNode = null;
    hostValueToEnsoNode = null;
    testRootNode = null;
  }

  @Test
  public void passNothingThru() {
    var ctx = EnsoContext.get(catchPanicNode);
    var any = ctx.getBuiltins().any();
    var nothing = ctx.getBuiltins().nothing();
    var result = catchPanicNode.execute(null, any, nothing, null);
    assertEquals("Nothing gets returned", nothing, result);
  }

  @Test
  public void passTextThru() {
    var ctx = EnsoContext.get(catchPanicNode);
    var any = ctx.getBuiltins().any();
    var text = Text.create("Hello");
    var result = catchPanicNode.execute(null, any, text, null);
    assertEquals("Hello gets returned", "Hello", result.toString());
  }

  @Test
  public void passEvaluatedThunkThru() {
    var ctx = EnsoContext.get(catchPanicNode);
    var any = ctx.getBuiltins().any();
    var text = Text.create("Running");
    var fn = new TestRootNode((frame) -> text);
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var result = catchPanicNode.execute(null, any, thunk, null);
    assertEquals("Thunk gets evaluated", "Running", result.toString());
  }

  @Test
  public void catchAnyPanic() {
    var ctx = EnsoContext.get(catchPanicNode);
    var any = ctx.getBuiltins().any();
    var thrown = Text.create("Thrown");
    var text = Text.create("Catched");
    var handlerFn =
        new TestRootNode(
            (frame) -> {
              var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
              assertEquals("One argument expected", 1, args.length);
              var argType = TypeOfNode.getUncached().findTypeOrError(args[0]);
              if (argType == ctx.getBuiltins().caughtPanic().getType()) {
                assertThat(args[0].toString(), Matchers.containsString("Thrown"));
                return text;
              } else {
                fail("Expecting Catched_Panic: " + args[0] + " type: " + argType);
                return null;
              }
            });
    var fn =
        new TestRootNode(
            (frame) -> {
              throw new PanicException(thrown, null);
            });
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var handler = new Function(handlerFn.getCallTarget(), null, schema("err"));
    var result = catchPanicNode.execute(null, any, thunk, handler);
    assertEquals("Thunk gets evaluated", "Catched", result.toString());
  }

  @Test
  public void catchAnyPanicSentinel() {
    var ctx = EnsoContext.get(catchPanicNode);
    var any = ctx.getBuiltins().any();
    var thrown = Text.create("Thrown");
    var text = Text.create("Catched");
    var handlerFn =
        new TestRootNode(
            (frame) -> {
              var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
              assertEquals("One argument expected", 1, args.length);
              var argType = TypeOfNode.getUncached().findTypeOrError(args[0]);
              if (argType == ctx.getBuiltins().caughtPanic().getType()) {
                assertThat(args[0].toString(), Matchers.containsString("Thrown"));
                return text;
              } else {
                fail("Expecting Catched_Panic: " + args[0] + " type: " + argType);
                return null;
              }
            });
    var fn = new TestRootNode((frame) -> new PanicSentinel(new PanicException(thrown, null), null));
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var handler = new Function(handlerFn.getCallTarget(), null, schema("err"));
    var result = catchPanicNode.execute(null, any, thunk, handler);
    assertEquals("Thunk gets evaluated", "Catched", result.toString());
  }

  @Test
  public void catchSpecificPanic() {
    var ctx = EnsoContext.get(catchPanicNode);
    var textType = ctx.getBuiltins().text();
    var thrown = Text.create("Thrown");
    var text = Text.create("Catched");
    var handlerFn =
        new TestRootNode(
            (frame) -> {
              var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
              assertEquals("One argument expected", 1, args.length);
              var argType = TypeOfNode.getUncached().findTypeOrError(args[0]);
              if (argType == ctx.getBuiltins().caughtPanic().getType()) {
                assertThat(args[0].toString(), Matchers.containsString("Thrown"));
                return text;
              } else {
                fail("Expecting Catched_Panic: " + args[0] + " type: " + argType);
                return null;
              }
            });
    var fn =
        new TestRootNode(
            (frame) -> {
              throw new PanicException(thrown, null);
            });
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var handler = new Function(handlerFn.getCallTarget(), null, schema("err"));
    var result = catchPanicNode.execute(null, textType, thunk, handler);
    assertEquals("Thunk gets evaluated", "Catched", result.toString());
  }

  @Test
  public void catchSpecificPanicSentinel() {
    var ctx = EnsoContext.get(catchPanicNode);
    var textType = ctx.getBuiltins().text();
    var thrown = Text.create("Thrown");
    var text = Text.create("Catched");
    var handlerFn =
        new TestRootNode(
            (frame) -> {
              var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
              assertEquals("One argument expected", 1, args.length);
              var argType = TypeOfNode.getUncached().findTypeOrError(args[0]);
              if (argType == ctx.getBuiltins().caughtPanic().getType()) {
                assertThat(args[0].toString(), Matchers.containsString("Thrown"));
                return text;
              } else {
                fail("Expecting Catched_Panic: " + args[0] + " type: " + argType);
                return null;
              }
            });
    var fn =
        new TestRootNode(
            (frame) -> {
              return new PanicSentinel(new PanicException(thrown, null), null);
            });
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var handler = new Function(handlerFn.getCallTarget(), null, schema("err"));
    var result = catchPanicNode.execute(null, textType, thunk, handler);
    assertEquals("Thunk gets evaluated", "Catched", result.toString());
  }

  @Test
  public void dontCatchSpecificPanic() {
    var ctx = EnsoContext.get(catchPanicNode);
    var numberType = ctx.getBuiltins().number().getNumber();
    var thrown = Text.create("Thrown");
    var text = Text.create("Catched");
    var handlerFn =
        new TestRootNode(
            (frame) -> {
              var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
              assertEquals("One argument expected", 1, args.length);
              var argType = TypeOfNode.getUncached().findTypeOrError(args[0]);
              if (argType == ctx.getBuiltins().caughtPanic().getType()) {
                assertThat(args[0].toString(), Matchers.containsString("Thrown"));
                return text;
              } else {
                fail("Expecting Catched_Panic: " + args[0] + " type: " + argType);
                return null;
              }
            });
    var fn =
        new TestRootNode(
            (frame) -> {
              throw new PanicException(thrown, null);
            });
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var handler = new Function(handlerFn.getCallTarget(), null, schema("err"));
    try {
      var result = catchPanicNode.execute(null, numberType, thunk, handler);
      fail("Not expecting any result back: " + result);
    } catch (PanicException ex) {
      // OK
      assertEquals("Thrown", ex.getMessage());
    }
  }

  @Test
  public void dontCatchSpecificPanicSentinel() {
    var ctx = EnsoContext.get(catchPanicNode);
    var numberType = ctx.getBuiltins().number().getNumber();
    var thrown = Text.create("Thrown");
    var text = Text.create("Catched");
    var handlerFn =
        new TestRootNode(
            (frame) -> {
              var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
              assertEquals("One argument expected", 1, args.length);
              var argType = TypeOfNode.getUncached().findTypeOrError(args[0]);
              if (argType == ctx.getBuiltins().caughtPanic().getType()) {
                assertThat(args[0].toString(), Matchers.containsString("Thrown"));
                return text;
              } else {
                fail("Expecting Catched_Panic: " + args[0] + " type: " + argType);
                return null;
              }
            });
    var fn =
        new TestRootNode(
            (frame) -> {
              return new PanicSentinel(new PanicException(thrown, null), null);
            });
    var thunk = Function.thunk(fn.getCallTarget(), null);
    var handler = new Function(handlerFn.getCallTarget(), null, schema("err"));
    try {
      var result = catchPanicNode.execute(null, numberType, thunk, handler);
      fail("Not expecting any result back: " + result);
    } catch (PanicException ex) {
      // OK
      assertEquals("Thrown", ex.getMessage());
    }
  }

  private static FunctionSchema schema(String argName) {
    var def =
        new ArgumentDefinition(0, argName, null, null, ArgumentDefinition.ExecutionMode.EXECUTE);
    return FunctionSchema.newBuilder().argumentDefinitions(def).build();
  }
}
