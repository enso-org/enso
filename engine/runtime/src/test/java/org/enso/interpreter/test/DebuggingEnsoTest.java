package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.debug.DebugException;
import com.oracle.truffle.api.debug.DebugScope;
import com.oracle.truffle.api.debug.DebugStackFrame;
import com.oracle.truffle.api.debug.DebugValue;
import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.DebuggerSession;
import com.oracle.truffle.api.debug.SuspendedEvent;
import java.net.URI;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.enso.interpreter.runtime.error.PanicException;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class DebuggingEnsoTest {
  private Context context;
  private Engine engine;
  private Debugger debugger;

  @Before
  public void initContext() {
    engine = Engine.newBuilder()
        .allowExperimentalOptions(true)
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../test/micro-distribution/component").toFile().getAbsolutePath()
        ).build();

    context = Context.newBuilder()
        .engine(engine)
        .allowExperimentalOptions(true)
        .allowIO(true)
        .allowAllAccess(true)
        .build();

    debugger = Debugger.find(engine);

    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @After
  public void disposeContext() {
    context.close();
    engine.close();
  }

  private static void expectStackFrame(DebugStackFrame actualFrame, Map<String, String> expectedValues) {
    Map<String, String> actualValues = new HashMap<>();
    for (DebugValue declaredValue : actualFrame.getScope().getDeclaredValues()) {
      actualValues.put(
          declaredValue.getName(),
          declaredValue.toDisplayString()
      );
    }
    String errMessage = String.format("Expected values in stack: %s, instead got: %s",
        expectedValues, actualValues);
    Assert.assertEquals(errMessage, expectedValues, actualValues);
  }

  private static List<DebugStackFrame> getStackFramesFromEvent(SuspendedEvent event) {
    List<DebugStackFrame> stackFrames = new ArrayList<>();
    event.getStackFrames().forEach(stackFrames::add);
    return stackFrames;
  }

  private static Source createEnsoSource(String srcCode) {
    return Source.newBuilder("enso", srcCode, "tmp.enso")
        .uri(URI.create("memory://tmp.enso"))
        .buildLiteral();
  }


  /**
   * Steps through recursive evaluation of factorial with an accumulator, and for each step,
   * checks the value of the `accumulator` variable.
   */
  @Test
  @Ignore
  public void recursiveFactorialCall() throws Exception {
    final Source facSrc = createEnsoSource("""
    fac : Number -> Number
    fac n =
        facacc : Number -> Number -> Number
        facacc n accumulator =
                stop = n <= 1
                if stop then accumulator else @Tail_Call facacc n-1 n*accumulator

        facacc n 1
    """);

    var module = context.eval(facSrc);
    var facFn = module.invokeMember("eval_expression", "fac");
    final var values = new TreeSet<Integer>();
    try (var session = debugger.startSession((event) -> {
      final DebugValue accumulatorValue = findDebugValue(event, "accumulator");
      if (accumulatorValue != null) {
        final int accumulator = accumulatorValue.asInt();
        values.add(accumulator);
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      var fac5 = facFn.execute(5);
      Assert.assertEquals("5!", 120, fac5.asInt());
    }
    assertEquals("Accumulator gets following values one by one", Set.of(1, 5, 20, 60, 120), values);
  }

  /**
   * Checks whether the debugger correctly displays the values of variables in
   * stack frames, including the stack frame of the caller method.
   */
  @Test
  @Ignore
  public void callerVariablesAreVisibleOnPreviousStackFrame() {
    Source fooSource = createEnsoSource("""
        bar arg_bar =
            loc_bar = arg_bar + 1
            loc_bar
            
        foo x =
            loc_foo = 1
            bar loc_foo
        """);

    Value module = context.eval(fooSource);
    Value fooFunc = module.invokeMember("eval_expression", "foo");

    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      // TODO[PM]: This is a workaround for proper breakpoints, which do not work atm.
      switch (event.getSourceSection().getCharacters().toString().strip()) {
        // In method "foo"
        case "bar loc_foo" -> {
          List<DebugStackFrame> stackFrames = getStackFramesFromEvent(event);
          Assert.assertEquals(1, stackFrames.size());
          expectStackFrame(stackFrames.get(0), Map.of("x", "42", "loc_foo", "1"));
        }
        // In method "bar" called from "foo"
        case "loc_bar" -> {
          List<DebugStackFrame> stackFrames = getStackFramesFromEvent(event);

          Assert.assertEquals(2, stackFrames.size());
          assertTrue(stackFrames.get(1).getName().contains("foo"));
          assertTrue(stackFrames.get(0).getName().contains("bar"));

          expectStackFrame(stackFrames.get(1), Map.of("x", "42", "loc_foo", "1"));
          expectStackFrame(stackFrames.get(0), Map.of("arg_bar", "1", "loc_bar", "2"));
        }
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      fooFunc.execute(42);
    }
  }

  /**
   * Host values in the stack frame are handled specially, because of https://github.com/oracle/graal/issues/5513
   */
  @Test
  public void testHostValues() {
    Source src = createEnsoSource("""
        polyglot java import java.nio.file.Path
        polyglot java import java.util.ArrayList
        
        foo x =
            path = Path.of 'blaaaaa'
            list = ArrayList.new
            list.add 10
            list.add 20
            tmp = 42
        """);
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, "foo");

    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      switch (event.getSourceSection().getCharacters().toString().strip()) {
        case "tmp = 42" -> {
          DebugScope scope = event.getTopStackFrame().getScope();
          DebugValue pathValue = scope.getDeclaredValue("path");
          assertTrue(pathValue.isReadable());
          assertFalse(pathValue.isInternal());
          assertFalse(pathValue.hasReadSideEffects());
          assertTrue(pathValue.toDisplayString().startsWith("HostObject"));

          DebugValue listValue = scope.getDeclaredValue("list");
          // ArrayList is internally represented as Enso list, but as an object
          // initialized in host context, it suffers from the issue mentioned in
          // https://github.com/oracle/graal/issues/5513. Therefore, we display
          // it just as 'HostObject' in the debugger.
          assertNotNull(listValue);
          assertTrue(listValue.toDisplayString().startsWith("HostObject"));
        }
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void testEvaluateExpression() {
    Source src = createEnsoSource("""
        polyglot java import java.nio.file.Path
        
        foo x =
            a = 10
            b = 20
            tmp = 42
        """);
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, "foo");

    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      switch (event.getSourceSection().getCharacters().toString().strip()) {
        case "tmp = 42" -> {
          DebugStackFrame stackFrame = event.getTopStackFrame();
          DebugValue evaluatedValue = stackFrame.eval("a + b");
          assertTrue(evaluatedValue.isNumber());
          assertEquals(30, evaluatedValue.asInt());
        }
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void testRewriteLocalVariable() {
    Source src = createEnsoSource("""
        foo x =
            a = 10
            b = 20
            tmp = a + b
            end = 42
        """);
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, "foo");

    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      switch (event.getSourceSection().getCharacters().toString().strip()) {
        case "tmp = a + b" -> {
          DebugStackFrame stackFrame = event.getTopStackFrame();
          assertTrue(stackFrame.getScope().getDeclaredValue("a").isWritable());
          assertTrue(stackFrame.getScope().getDeclaredValue("b").isWritable());

          stackFrame.getScope().getDeclaredValue("a").set(
              stackFrame.eval("1")
          );
          stackFrame.getScope().getDeclaredValue("b").set(
              stackFrame.eval("2")
          );
          assertEquals(3, stackFrame.eval("a + b").asInt());
        }
        case "end = 42" -> {
          DebugStackFrame stackFrame = event.getTopStackFrame();
          assertEquals(1, stackFrame.getScope().getDeclaredValue("a").asInt());
          assertEquals(2, stackFrame.getScope().getDeclaredValue("b").asInt());
          assertEquals(3, stackFrame.getScope().getDeclaredValue("tmp").asInt());
        }
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void testRewriteVariableInCallerStackFrame() {
    Source src = createEnsoSource("""
        bar =
            loc_bar = 42
        
        foo x =
            a = 10  # Will get modified to 1
            b = 20  # Will get modified to 2
            bar
            a + b
        """);
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, "foo");

    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      switch (event.getSourceSection().getCharacters().toString().strip()) {
        case "loc_bar = 42" -> {
          // Modify variables in the caller's frame
          List<DebugStackFrame> frames = new ArrayList<>();
          event.getStackFrames().iterator().forEachRemaining(frames::add);
          assertEquals(2, frames.size());
          DebugStackFrame callerStackFrame = frames.get(1);
          callerStackFrame.getScope().getDeclaredValue("a").set(
              callerStackFrame.eval("1")
          );
          callerStackFrame.getScope().getDeclaredValue("b").set(
              callerStackFrame.eval("2")
          );

        }
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      Value valFromFoo = fooFunc.execute(0);
      assertEquals(3, valFromFoo.asInt());
    }
  }

  @Test
  public void testFailingEvaluations() {
    Source src = createEnsoSource("foo x = x");
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, "foo");
    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      // This snippet is actually called from chromeinspector as the very first command
      // after typing anything in the console.
      assertThrows("Evaluating syntactically incorrect snippet should throw exception",
          DebugException.class,
          () -> event.getTopStackFrame().eval("(async function(){ await 1;})()")
      );
      // Also test that the thrown exception contains some reasonable error message, because
      // that error message will be printed in the chromeinspector console
      DebugException exception = assertThrows("Evaluating non existing identifiers should throw PanicException, wrapped in DebugException",
          DebugException.class,
          () -> event.getTopStackFrame().eval("non_existing_identifier")
      );
      assertTrue(exception.getMessage().contains("The name `non_existing_identifier` could not be found"));

      assertThrows(
              DebugException.class,
              () -> event.getTopStackFrame().eval("13 + non_existing_identifier")
      );
      assertThrows("Imports should not be evaluated",
              DebugException.class,
              () -> event.getTopStackFrame().eval("from Standard.Base import all")
      );
      assertThrows("Assignments should not be evaluated",
              DebugException.class,
              () -> event.getTopStackFrame().eval("tmp = 45")
      );
    })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  // TODO[PM]: Re-enable (https://www.pivotaltracker.com/story/show/183854585)
  @Test
  @Ignore
  public void unsafeRecursiveAtom() throws Exception {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../test/micro-distribution/component").toFile().getAbsolutePath()
      ).build();
    Context ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .allowHostClassLoading(true)
      .allowHostClassLookup((c) -> true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    org.junit.Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));

    final URI onceUri = new URI("memory://once.enso");
    final Source onesSrc = Source.newBuilder("enso", """
        import Standard.Base.Runtime.Unsafe

        type Gen
            Empty
            Generator a:Int tail:Gen

        ones : Gen
        ones =
            g = Gen.Generator 1 Gen.Empty
            Unsafe.set_atom_field g 1 g
            g

        next g = case g of
            Gen.Generator a tail -> a
            Gen.Empty -> -1
        """, "ones.enso")
            .uri(onceUri)
            .buildLiteral();

    var module = ctx.eval(onesSrc);
    var ones = module.invokeMember("eval_expression", "ones");
    var next = module.invokeMember("eval_expression", "next");


    final var dbg = Debugger.find(eng);
    final var values = new HashSet<String>();
    try (var session = dbg.startSession((event) -> {
      final DebugValue gVariable = findDebugValue(event, "g");
      if (gVariable != null) {
        final String str = gVariable.toDisplayString(false);
        assertNotNull("The string shall always be computed for " + gVariable, str);
        values.add(str);
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      var one = next.execute(ones);
      Assert.assertEquals("First element from list of ones", 1, one.asInt());
    }
    Assert.assertEquals("Some values of g variable found: " + values, 1, values.size());
  }

  private static DebugValue findDebugValue(SuspendedEvent event, final String n) throws DebugException {
    for (var v : event.getTopStackFrame().getScope().getDeclaredValues()) {
      if (v.getName().contains(n)) {
        return v;
      }
    }
    return null;
  }
}
