package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.debug.DebugException;
import com.oracle.truffle.api.debug.DebugScope;
import com.oracle.truffle.api.debug.DebugStackFrame;
import com.oracle.truffle.api.debug.DebugValue;
import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.DebuggerSession;
import com.oracle.truffle.api.debug.SuspendedCallback;
import com.oracle.truffle.api.debug.SuspendedEvent;
import com.oracle.truffle.api.nodes.LanguageInfo;
import java.io.OutputStream;
import java.net.URI;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
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
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        )
        .logHandler(OutputStream.nullOutputStream())
        .build();

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

  private Value createEnsoMethod(String source, String methodName) {
    Value module = context.eval(createEnsoSource(source));
    return module.invokeMember(Module.EVAL_EXPRESSION, methodName);
  }

  /**
   * Steps through recursive evaluation of factorial with an accumulator, and for each step,
   * checks the value of the `accumulator` variable.
   */
  @Test
  public void recursiveFactorialCall() {
    final Value facFn = createEnsoMethod("""
    fac : Number -> Number
    fac n =
        facacc : Number -> Number -> Number
        facacc n accumulator =
                stop = n <= 1
                if stop then accumulator else @Tail_Call facacc n-1 n*accumulator

        facacc n 1
    """, "fac");

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
  public void callerVariablesAreVisibleOnPreviousStackFrame() {
    Value fooFunc = createEnsoMethod("""
        bar arg_bar =
            loc_bar = arg_bar + 1
            loc_bar
            
        foo x =
            loc_foo = 1
            bar loc_foo
        """, "foo");

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
     Value fooFunc = createEnsoMethod("""
        polyglot java import java.nio.file.Path
        polyglot java import java.util.ArrayList
        
        foo x =
            path = Path.of 'blaaaaa'
            list = ArrayList.new
            list.add 10
            list.add 20
            tmp = 42
        """, "foo");

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
  public void testHostValueAsAtomField() {
    Value fooFunc = createEnsoMethod("""
        from Standard.Base import Vector
        
        foo x =
            vec_builder = Vector.new_builder
            end = 42
        """, "foo");

    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      if (event.getSourceSection().getCharacters().toString().strip().equals("end = 42")) {
        DebugValue vecBuilder = event.getTopStackFrame().eval("vec_builder");
        // `java_builder` is a field of `vec_builder` atom and it is a HostObject.
        // As such it should be wrapped, and considered only as an interop string.
        DebugValue javaBuilder = vecBuilder.getProperty("java_builder");
        assertTrue(javaBuilder.toDisplayString().contains("Array_Builder"));
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void testEvaluateExpression() {
    Value fooFunc = createEnsoMethod("""
        polyglot java import java.nio.file.Path
        
        foo x =
            a = 10
            b = 20
            tmp = 42
        """, "foo");

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
    Value fooFunc = createEnsoMethod("""
        foo x =
            a = 10
            b = 20
            tmp = a + b
            end = 42
        """, "foo");

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
    Value fooFunc = createEnsoMethod("""
        bar =
            loc_bar = 42
        
        foo x =
            a = 10  # Will get modified to 1
            b = 20  # Will get modified to 2
            bar
            a + b
        """, "foo");

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
    Value fooFunc = createEnsoMethod("foo x = x", "foo");
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

  /**
   * Tests stepping through the given source.
   * @param src Source that is stepped through.
   * @param methodName Name of the method to invoke
   * @param methodArgs Arguments to the method.
   * @param steps A queue of {@link SuspendedCallback} events to be done
   * @param expectedLineNumbers A list of line numbers on which the debugger is expected to stop.
   */
  private void testStepping(Source src, String methodName, Object[] methodArgs, Queue<SuspendedCallback> steps, List<Integer> expectedLineNumbers) {
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, methodName);
    List<Integer> lineNumbers = new ArrayList<>();
    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      if (!steps.isEmpty()) {
        steps.remove().onSuspend(event);
      }
      lineNumbers.add(
          event.getSourceSection().getStartLine()
      );
    })) {
      session.suspendNextExecution();
      fooFunc.execute(methodArgs);
    }
    assertListEquals(expectedLineNumbers, lineNumbers);
  }

  /**
   * Create a queue of stepping over events. The very first event must always be step into,
   * otherwise the execution would end after first step.
   * @param numSteps Total number of steps, usually the number of expected lines.
   */
  private static Queue<SuspendedCallback> createStepOverEvents(int numSteps) {
    Queue<SuspendedCallback> steps = new ArrayDeque<>();
    steps.add((event) -> event.prepareStepInto(1));
    for (int i = 0; i < numSteps - 1; i++) {
      steps.add(
          (event) -> event.prepareStepOver(1)
      );
    }
    return steps;
  }

  private static List<Integer> mapLinesToLineNumbers(Source src, List<String> lines) {
    // We use \n even on Windows
    String[] linesInSrc = src.getCharacters().toString().split("\n");
    return lines.stream()
        .map(line -> {
          for (int i = 0; i < linesInSrc.length; i++) {
            if (linesInSrc[i].stripLeading().startsWith(line)) {
              // From the Truffle debugger point of view, lines are indexed from 1 instead of 0.
              return i + 1;
            }
          }
          throw new IllegalStateException();
        })
        .collect(Collectors.toList());
  }

  private void assertListEquals(List<?> expected, List<?> actual) {
    String failureMsg = "Expected list: " + expected + " is not equal to actual list: " + actual;
    assertEquals(failureMsg, expected.size(), actual.size());
    for (int i = 0; i < expected.size(); i++) {
      assertEquals(failureMsg, expected.get(i), actual.get(i));
    }
  }

  @Test
  public void testSteppingOver() {
    Source src = createEnsoSource("""
        baz x = x      # 1
        bar x = baz x  # 2
        foo x =        # 3
            bar 42     # 4
            end = 0    # 5
        """);
    List<Integer> expectedLineNumbers = List.of(3, 4, 5);
    Queue<SuspendedCallback> steps = createStepOverEvents(expectedLineNumbers.size());
    testStepping(src, "foo", new Object[]{0}, steps, expectedLineNumbers);
  }

  /**
   * Use some methods from Vector in stdlib. Stepping over methods from different
   * modules might be problematic.
   */
  @Test
  public void testSteppingOverUseStdLib() {
    Source src = createEnsoSource("""
        from Standard.Base import Vector
        
        bar vec num_elems =
            vec.slice 0 num_elems
        
        foo x =
            vec_builder = Vector.new_builder
            vec_builder.append 1
            vec_builder.append 2
            vec = bar (vec_builder.to_vector) (vec_builder.to_vector.length - 1)
            end = 0
        """);

    List<String> expectedLines = List.of(
        "foo x =",
        "vec_builder = Vector.new_builder",
        "vec_builder.append 1",
        "vec_builder.append 2",
        "vec = bar (vec_builder.to_vector) (vec_builder.to_vector.length - 1)",
        "end = 0"
    );
    List<Integer> expectedLineNumbers = mapLinesToLineNumbers(src, expectedLines);
    Queue<SuspendedCallback> steps = createStepOverEvents(expectedLineNumbers.size());
    testStepping(src, "foo", new Object[]{0}, steps, expectedLineNumbers);
  }

  @Test
  public void testSteppingInto() {
    Source src = createEnsoSource("""
        baz x = x       # 1
        bar x = baz x   # 2
        foo x =         # 3
            bar 42      # 4
            end = 0     # 5
        """);
    List<Integer> expectedLineNumbers = List.of(3, 4, 2, 1, 2, 4, 5);
    Queue<SuspendedCallback> steps = new ArrayDeque<>(
        Collections.nCopies(expectedLineNumbers.size(), (event) -> event.prepareStepInto(1))
    );

    testStepping(src, "foo", new Object[]{0}, steps, expectedLineNumbers);
  }

  @Test
  public void testSteppingIntoMoreExpressionsOneLine() {
    Source src = createEnsoSource("""
        baz x = x        # 1
        bar x = x        # 2
        foo x =          # 3
            bar (baz x)  # 4
            end = 0      # 5
        """);
    List<Integer> expectedLineNumbers = List.of(3, 4, 1, 4, 2, 4, 5);
    Queue<SuspendedCallback> steps = new ArrayDeque<>(
        Collections.nCopies(expectedLineNumbers.size(), (event) -> event.prepareStepInto(1))
    );
    testStepping(src, "foo", new Object[]{0}, steps, expectedLineNumbers);
  }

  /**
   * Steps through some stdlib methods, enumerates all the values in frames and checks if all
   * the host values are wrapped.
   *
   * Note that this is essentially a check whether the workaround for https://github.com/oracle/graal/issues/5513 works.
   */
  @Test
  public void testAllHostObjectsAreWrapped() {
    Value fooFunc = createEnsoMethod("""
        from Standard.Base import Vector
        foo x =
            vec = [5, 5, 1, 2, 1]
            vec.distinct
        """, "foo");
    List<FrameEntry> frames = new ArrayList<>();
    try (DebuggerSession session = debugger.startSession((SuspendedEvent event) -> {
      DebugScope topScope = event.getTopStackFrame().getScope();
      var frameEntry = new FrameEntry(topScope.getName(), event.getReturnValue());
      for (DebugValue declaredValue : topScope.getDeclaredValues()) {
        frameEntry.addValue(declaredValue);
      }
      frames.add(frameEntry);
      event.prepareStepInto(1);
    })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
    // Throughout Vector.distinct call, there will definitely be at least one host object
    // in one of the stack frames.
    long hostObjectValues = frames.stream()
        .filter(frameEntry ->
            frameEntry
                .values
                .values()
                .stream()
                .anyMatch(displayString -> displayString.contains("HostObject"))
        )
        .count();
    assertTrue(frames.size() > 1);
    assertTrue(hostObjectValues > 1);
  }

  private static final class FrameEntry {
    private final String scopeName;
    private final Map<String, String> values = new HashMap<>();
    private final String returnValue;

    FrameEntry(String scopeName, DebugValue returnValue) {
      Objects.requireNonNull(scopeName);
      this.scopeName = scopeName;
      this.returnValue = returnValue != null ? toDisplayString(returnValue) : null;
    }

    void addValue(DebugValue value) {
      LanguageInfo originLang = value.getOriginalLanguage();
      String valueDisplay = value.asInLanguage(originLang).toDisplayString();
      String name = value.getName();
      values.put(name, valueDisplay);
    }

    /**
     * Emulates the behavior of the chromeinspector, by getting the origin language of the
     * value, and interpreting the value in the origin language. This is problematic
     * for host object.
     */
    private static String toDisplayString(DebugValue value) {
      LanguageInfo originLang = value.getOriginalLanguage();
      return value.asInLanguage(originLang).toDisplayString();
    }

    @Override
    public String toString() {
      return String.format("%s: Values=%s, RetValue=%s", scopeName, values, returnValue);
    }
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
