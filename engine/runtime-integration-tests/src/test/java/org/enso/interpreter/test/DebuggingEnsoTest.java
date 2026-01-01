package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.nio.file.Paths;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.enso.common.MethodNames.Module;
import org.enso.common.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

public class DebuggingEnsoTest {
  private static Context context;
  private static Engine engine;
  private static Debugger debugger;
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void initContext() {
    engine =
        Engine.newBuilder()
            .allowExperimentalOptions(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .option(RuntimeOptions.CHECK_CWD, "false")
            .logHandler(out)
            .err(out)
            .out(out)
            .build();

    context =
        Context.newBuilder()
            .engine(engine)
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .build();

    debugger = Debugger.find(engine);

    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @AfterClass
  public static void disposeContext() throws IOException {
    context.close();
    context = null;
    engine.close();
    engine = null;
    debugger = null;
    out.close();
  }

  @Before
  public void resetOut() {
    out.reset();
  }

  /** Only print warnings from the compiler if a test fails. */
  @Rule
  public TestWatcher testWatcher =
      new TestWatcher() {
        @Override
        protected void failed(Throwable e, Description description) {
          System.err.println("Test failed: " + description.getMethodName());
          System.err.println("Error: " + e.getMessage());
          System.err.println("Logs from the compiler and the engine: ");
          System.err.println(out);
        }
      };

  private static void expectStackFrame(
      DebugStackFrame actualFrame, Map<String, String> expectedValues) {
    Map<String, String> actualValues = new HashMap<>();
    for (DebugValue declaredValue : actualFrame.getScope().getDeclaredValues()) {
      actualValues.put(declaredValue.getName(), declaredValue.toDisplayString());
    }
    String errMessage =
        String.format(
            "Expected values in stack: %s, instead got: %s", expectedValues, actualValues);
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
    Value m =
        context.eval(createEnsoSource("from Standard.Base import all\n\n" + methodName + " = 10"));
    m.invokeMember(Module.EVAL_EXPRESSION, methodName);
    Value module = context.eval(createEnsoSource(source));
    return module.invokeMember(Module.EVAL_EXPRESSION, methodName);
  }

  /**
   * Steps through recursive evaluation of factorial with an accumulator, and for each step, checks
   * the value of the `accumulator` variable.
   */
  @Test
  public void recursiveFactorialCall() {
    final Value facFn =
        createEnsoMethod(
            """
            from Standard.Base import all

            fac : Number -> Number
            fac n =
                facacc : Number -> Number -> Number
                facacc n accumulator =
                        stop = n <= 1
                        if stop then accumulator else @Tail_Call facacc n-1 n*accumulator

                facacc n 1
            """,
            "fac");

    final var values = new TreeSet<Integer>();
    try (var session =
        debugger.startSession(
            (event) -> {
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

  @Test
  public void unwindFrameTest() {
    var log = new StringWriter();
    log.append("\n");

    final Value compute =
        createEnsoMethod(
            """
            from Standard.Base import all

            combine a b =
                p =
                    x = a + b
                    x
                m =
                    a - b
                p * m

            compute u v =
                combine u v
            """,
            "compute");

    var check = compute.execute(5, 3);
    assertTrue("An integer produced", check.fitsInInt());
    assertEquals("Result is good", 16, check.asInt());

    final var values = new LinkedList<Integer>();
    final var rewinded = new boolean[] {false};
    try (var session =
        debugger.startSession(
            (event) -> {
              var aValue = findDebugValue(event, "a");
              var bValue = findDebugValue(event, "b");
              log.append("a " + aValue + " b " + bValue + "\n");
              var xValue = findDebugValue(event, "x");
              if (xValue != null && xValue.fitsInInt()) {
                log.append("x " + xValue + "\n");
                values.add(xValue.asInt());
                if (!rewinded[0]) {
                  rewinded[0] = true;
                  var it = event.getStackFrames().iterator();
                  var frame = it.next();
                  event.prepareUnwindFrame(frame);
                  return;
                }
              }
              if (aValue != null && aValue.fitsInInt() && bValue != null && bValue.fitsInInt()) {
                values.add(aValue.asInt());
                values.add(bValue.asInt());
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      var ab = compute.execute(5, 3);
      assertTrue("An integer produced again: " + ab + log, ab.fitsInInt());
      assertEquals("Same result as before" + log, 16, ab.asInt());
    }
    assertEquals(
        "Up to x=8, then again and then final result",
        List.of(
            5,
            3,
            5,
            3,
            5,
            3,
            5,
            3,
            8, // unwinded here
            5,
            3,
            5,
            3,
            5,
            3,
            5,
            3,
            8, // not unwinded again
            5,
            3,
            5,
            3 // computed to the end
            ),
        values);
  }

  /**
   * Checks whether the debugger correctly displays the values of variables in stack frames,
   * including the stack frame of the caller method.
   */
  @Test
  public void callerVariablesAreVisibleOnPreviousStackFrame() {
    Value fooFunc =
        createEnsoMethod(
            """
            bar arg_bar =
                loc_bar = arg_bar + 1
                loc_bar

            foo x =
                loc_foo = 1
                bar loc_foo
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
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

  @Test
  public void testHostValues() {
    Value fooFunc =
        createEnsoMethod(
            """
            polyglot java import java.nio.file.Path
            polyglot java import java.util.ArrayList

            foo x =
                path = Path.of 'blaaaaa'
                list = ArrayList.new
                list.add 10
                list.add 20
                tmp = 42
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "tmp = 42" -> {
                  DebugScope scope = event.getTopStackFrame().getScope();
                  DebugValue pathValue = scope.getDeclaredValue("path");
                  assertTrue(pathValue.isReadable());
                  assertFalse(pathValue.isInternal());
                  assertFalse(pathValue.hasReadSideEffects());

                  DebugValue listValue = scope.getDeclaredValue("list");
                  assertNotNull(listValue);
                  assertTrue(listValue.isArray());
                  assertEquals(10, listValue.getArray().get(0).asInt());
                  assertEquals(20, listValue.getArray().get(1).asInt());
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  /**
   * Both {@code Date.new 2024 12 15} and {@code Date.parse "2024-12-15"} should be seen by the
   * debugger as the exact same objects. Internally, the value from {@code Date.parse} is a host
   * value.
   */
  @Test
  public void hostValueIsTreatedAsItsEnsoCounterpart() {
    Value fooFunc =
        createEnsoMethod(
            """
            from Standard.Base import Date, Date_Time, Dictionary
            polyglot java import java.lang.String
            polyglot java import java.util.List as JList
            polyglot java import java.util.Map as JMap

            foreign js js_date = '''
                return new Date();

            foreign js js_str = '''
                return "Hello_World";

            foreign js js_list = '''
                return [1, 2, 3];

            foreign js js_map = '''
                let m = new Map();
                m.set('A', 1);
                m.set('B', 2);
                return m;

            foreign python py_list = '''
                return [1, 2, 3]

            foreign python py_dict = '''
                return {'A': 1, 'B': 2}

            foo _ =
                d_enso = Date.new 2024 12 15
                d_java = Date.parse "2024-12-15"
                dt_enso = Date_Time.now
                dt_java = Date_Time.parse "2020-05-06 04:30:20" "yyyy-MM-dd HH:mm:ss"
                dt_js = js_date
                str_enso = "Hello_World"
                str_js = js_str
                str_java = String.new "Hello_World"
                list_enso = [1, 2, 3]
                list_js = js_list
                list_py = py_list
                list_java = JList.of 1 2 3
                dict_enso = Dictionary.from_vector [["A", 1], ["B", 2]]
                dict_js = js_map
                dict_py = py_dict
                dict_java = JMap.of "A" 1 "B" 2
                end = 42
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "end = 42" -> {
                  DebugScope scope = event.getTopStackFrame().getScope();

                  DebugValue ensoDate = scope.getDeclaredValue("d_enso");
                  DebugValue javaDate = scope.getDeclaredValue("d_java");
                  assertSameProperties(ensoDate.getProperties(), javaDate.getProperties());

                  DebugValue ensoDateTime = scope.getDeclaredValue("dt_enso");
                  DebugValue javaDateTime = scope.getDeclaredValue("dt_java");
                  DebugValue jsDateTime = scope.getDeclaredValue("dt_js");
                  assertSameProperties(ensoDateTime.getProperties(), javaDateTime.getProperties());
                  assertSameProperties(ensoDateTime.getProperties(), jsDateTime.getProperties());

                  DebugValue ensoString = scope.getDeclaredValue("str_enso");
                  DebugValue javaString = scope.getDeclaredValue("str_java");
                  DebugValue jsString = scope.getDeclaredValue("str_js");
                  assertSameProperties(ensoString.getProperties(), javaString.getProperties());
                  assertSameProperties(ensoString.getProperties(), jsString.getProperties());

                  DebugValue ensoList = scope.getDeclaredValue("list_enso");
                  DebugValue javaList = scope.getDeclaredValue("list_java");
                  DebugValue jsList = scope.getDeclaredValue("list_js");
                  DebugValue pyList = scope.getDeclaredValue("list_py");
                  assertSameProperties(ensoList.getProperties(), javaList.getProperties());
                  assertSameProperties(ensoList.getProperties(), jsList.getProperties());
                  assertSameProperties(ensoList.getProperties(), pyList.getProperties());

                  DebugValue ensoDict = scope.getDeclaredValue("dict_enso");
                  DebugValue javaDict = scope.getDeclaredValue("dict_java");
                  DebugValue jsDict = scope.getDeclaredValue("dict_js");
                  DebugValue pyDict = scope.getDeclaredValue("dict_py");
                  assertSameProperties(ensoDict.getProperties(), javaDict.getProperties());
                  assertSameProperties(ensoDict.getProperties(), jsDict.getProperties());
                  assertSameProperties(ensoDict.getProperties(), pyDict.getProperties());
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  /** Asserts that the given values have same property names. */
  private void assertSameProperties(
      Collection<DebugValue> expectedProps, Collection<DebugValue> actualProps) {
    if (expectedProps == null) {
      assertThat(actualProps, anyOf(empty(), nullValue()));
      return;
    }
    assertThat(actualProps.size(), is(expectedProps.size()));
    var expectedPropNames =
        expectedProps.stream().map(DebugValue::getName).collect(Collectors.toUnmodifiableSet());
    var actualPropNames =
        actualProps.stream().map(DebugValue::getName).collect(Collectors.toUnmodifiableSet());
    assertThat(actualPropNames, is(expectedPropNames));
  }

  @Test
  public void testHostValueAsAtomField() {
    Value fooFunc =
        createEnsoMethod(
            """
            import Standard.Base.Internal.Array_Like_Helpers
            from Standard.Base import Any
            from Standard.Base import Integer

            new_vector_builder : Integer -> Any
            new_vector_builder capacity = @Builtin_Method "Array_Like_Helpers.new_vector_builder"

            type Builder
                Value java_builder

            foo x =
                java_builder = new_vector_builder 1
                builder = Builder.Value java_builder
                end = 42
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              if (event.getSourceSection().getCharacters().toString().strip().equals("end = 42")) {
                DebugValue vecBuilder = event.getTopStackFrame().eval("builder");
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
    Value fooFunc =
        createEnsoMethod(
            """
            polyglot java import java.nio.file.Path

            foo x =
                a = 10
                b = 20
                tmp = 42
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
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
  public void testEvaluateExpressionInDebugBreakpoint() {
    Value fooFunc =
        createEnsoMethod(
            """

            import Standard.Base.Runtime.Debug

            foo x =
                a = 6
                b = 7
                Debug.breakpoint
            """,
            "foo");

    int[] res = {0};
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "Debug.breakpoint" -> {
                  DebugStackFrame stackFrame = event.getTopStackFrame();
                  DebugValue evaluatedValue = stackFrame.eval("a * b");
                  assertTrue(evaluatedValue.isNumber());
                  assertEquals(42, res[0] = evaluatedValue.asInt());
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
    assertEquals("Really suspended at 42", 42, res[0]);
  }

  @Test
  public void testRewriteLocalVariable() {
    Value fooFunc =
        createEnsoMethod(
            """
            foo x =
                a = 10
                b = 20
                tmp = a + b
                end = 42
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "tmp = a + b" -> {
                  DebugStackFrame stackFrame = event.getTopStackFrame();
                  assertTrue(stackFrame.getScope().getDeclaredValue("a").isWritable());
                  assertTrue(stackFrame.getScope().getDeclaredValue("b").isWritable());

                  stackFrame.getScope().getDeclaredValue("a").set(stackFrame.eval("1"));
                  stackFrame.getScope().getDeclaredValue("b").set(stackFrame.eval("2"));
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
    Value fooFunc =
        createEnsoMethod(
            """
            bar =
                loc_bar = 42

            foo x =
                a = 10  # Will get modified to 1
                b = 20  # Will get modified to 2
                bar
                a + b
            """,
            "foo");

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "loc_bar = 42" -> {
                  // Modify variables in the caller's frame
                  List<DebugStackFrame> frames = new ArrayList<>();
                  event.getStackFrames().iterator().forEachRemaining(frames::add);
                  assertEquals(2, frames.size());
                  DebugStackFrame callerStackFrame = frames.get(1);
                  callerStackFrame.getScope().getDeclaredValue("a").set(callerStackFrame.eval("1"));
                  callerStackFrame.getScope().getDeclaredValue("b").set(callerStackFrame.eval("2"));
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
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              // This snippet is actually called from chromeinspector as the very first command
              // after typing anything in the console.
              assertThrows(
                  "Evaluating syntactically incorrect snippet should throw exception",
                  DebugException.class,
                  () -> event.getTopStackFrame().eval("(async function(){ await 1;})()"));
              // Also test that the thrown exception contains some reasonable error message, because
              // that error message will be printed in the chromeinspector console
              DebugException exception =
                  assertThrows(
                      "Evaluating non existing identifiers should throw PanicException, wrapped in"
                          + " DebugException",
                      DebugException.class,
                      () -> event.getTopStackFrame().eval("non_existing_identifier"));
              assertThat(
                  exception.getMessage(),
                  containsString("The name `non_existing_identifier` could not be found"));

              assertThrows(
                  DebugException.class,
                  () -> event.getTopStackFrame().eval("13 + non_existing_identifier"));
              assertThrows(
                  "Imports should not be evaluated",
                  DebugException.class,
                  () -> event.getTopStackFrame().eval("from Standard.Base import all"));
              assertThrows(
                  "Assignments should not be evaluated",
                  DebugException.class,
                  () -> event.getTopStackFrame().eval("tmp = 45"));
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void testAtomFieldsAreReadable() {
    var fooFunc =
        createEnsoMethod(
            """
            type My_Type
                Cons field_1 field_2

            foo x =
                obj = My_Type.Cons 1 2
                obj
            """,
            "foo");
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "obj" -> {
                  DebugScope scope = event.getTopStackFrame().getScope();
                  DebugValue objValue = scope.getDeclaredValue("obj");
                  assertThat(objValue.isReadable(), is(true));
                  assertThat(objValue.isInternal(), is(false));
                  assertThat(objValue.hasReadSideEffects(), is(false));

                  for (var propName : List.of("field_1", "field_2")) {
                    var fieldProp = objValue.getProperty(propName);
                    assertThat(fieldProp.isReadable(), is(true));
                    assertThat(fieldProp.isNumber(), is(true));
                  }
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void debuggerDoesNotEvaluateMethods() {
    var fooFunc =
        createEnsoMethod(
            """
            from Standard.Base import IO

            type My_Type
                Cons

                method self =
                    IO.println "Method evaluated"
                    42

            foo x =
                obj = My_Type.Cons
                obj
            """,
            "foo");
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "obj" -> {
                  DebugScope scope = event.getTopStackFrame().getScope();
                  DebugValue objValue = scope.getDeclaredValue("obj");
                  assertThat(objValue.isReadable(), is(true));
                  assertThat(objValue.isInternal(), is(false));
                  assertThat(objValue.hasReadSideEffects(), is(false));

                  var methodProp = objValue.getProperty("method");
                  assertThat(methodProp.canExecute(), is(true));
                  assertThat("It is a method, not a number", methodProp.isNumber(), is(false));
                  assertThat(
                      "Method should not be evaluated when accessed as a debug property",
                      out.toString(),
                      not(containsString("Method evaluated")));
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void testAtomFieldAreReadable_MultipleConstructors() {
    var fooFunc =
        createEnsoMethod(
            """
            type My_Type
                Cons_1 f1 f2
                Cons_2 g1 g2 g3

            foo x =
                obj = My_Type.Cons_1 1 2
                obj
            """,
            "foo");
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "obj" -> {
                  DebugScope scope = event.getTopStackFrame().getScope();
                  DebugValue objValue = scope.getDeclaredValue("obj");
                  assertThat(objValue.isReadable(), is(true));
                  assertThat(objValue.isInternal(), is(false));
                  assertThat(objValue.hasReadSideEffects(), is(false));

                  for (var propName : List.of("f1", "f2")) {
                    var fieldProp = objValue.getProperty(propName);
                    assertThat(fieldProp.isReadable(), is(true));
                    assertThat(fieldProp.isNumber(), is(true));
                  }
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  /**
   * Tests stepping through the given source.
   *
   * @param src Source that is stepped through.
   * @param methodName Name of the method to invoke
   * @param methodArgs Arguments to the method.
   * @param steps A queue of {@link SuspendedCallback} events to be done
   * @param expectedLineNumbers A list of line numbers on which the debugger is expected to stop.
   */
  private void testStepping(
      Source src,
      String methodName,
      Object[] methodArgs,
      Queue<SuspendedCallback> steps,
      List<Integer> expectedLineNumbers) {
    Value module = context.eval(src);
    Value fooFunc = module.invokeMember(Module.EVAL_EXPRESSION, methodName);
    List<Integer> lineNumbers = new ArrayList<>();
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              if (!steps.isEmpty()) {
                steps.remove().onSuspend(event);
              }
              lineNumbers.add(event.getSourceSection().getStartLine());
            })) {
      session.suspendNextExecution();
      fooFunc.execute(methodArgs);
    }
    assertThat(lineNumbers, equalTo(expectedLineNumbers));
  }

  /**
   * Create a queue of stepping over events. The very first event must always be step into,
   * otherwise the execution would end after first step.
   *
   * @param numSteps Total number of steps, usually the number of expected lines.
   */
  private static Queue<SuspendedCallback> createStepOverEvents(int numSteps) {
    Queue<SuspendedCallback> steps = new ArrayDeque<>();
    for (int i = 0; i < numSteps - 1; i++) {
      steps.add((event) -> event.prepareStepOver(1));
    }
    return steps;
  }

  private static List<Integer> mapLinesToLineNumbers(Source src, List<String> lines) {
    // We use \n even on Windows
    String[] linesInSrc = src.getCharacters().toString().split("\n");
    return lines.stream()
        .map(
            line -> {
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

  @Test
  public void testSteppingOver() {
    Source src =
        createEnsoSource(
            """
            baz x = x        # 1
            bar x =          # 2
                ret = baz x  # 3
                ret          # 4
            foo x =          # 5
                bar 42       # 6
                end = 0      # 7
            """);
    List<Integer> expectedLineNumbers = List.of(6, 7);
    Queue<SuspendedCallback> steps = createStepOverEvents(expectedLineNumbers.size());
    testStepping(src, "foo", new Object[] {0}, steps, expectedLineNumbers);
  }

  /**
   * Use some methods from Vector in stdlib. Stepping over methods from different modules might be
   * problematic.
   */
  @Test
  public void testSteppingOverUseStdLib() {
    Source src =
        createEnsoSource(
            """
            from Standard.Base import Vector
            import Standard.Base.Data.Vector.Builder

            bar vec num_elems =
                vec.slice 0 num_elems

            foo x =
                vec_builder = Builder.new
                vec_builder.append 1
                vec_builder.append 2
                vec = bar (vec_builder.to_vector) (vec_builder.to_vector.length - 1)
                end = 0
            """);

    List<String> expectedLines =
        List.of(
            "vec_builder = Builder.new",
            "vec_builder.append 1",
            "vec_builder.append 2",
            "vec = bar (vec_builder.to_vector) (vec_builder.to_vector.length - 1)",
            "end = 0");
    List<Integer> expectedLineNumbers = mapLinesToLineNumbers(src, expectedLines);
    Queue<SuspendedCallback> steps = createStepOverEvents(expectedLineNumbers.size());
    testStepping(src, "foo", new Object[] {0}, steps, expectedLineNumbers);
  }

  @Test
  public void testSteppingInto() {
    Source src =
        createEnsoSource(
            """
            baz x = x       # 1
            bar x = baz x   # 2
            foo x =         # 3
                bar 42      # 4
                end = 0     # 5
            """);
    List<Integer> expectedLineNumbers = List.of(4, 2, 1, 2, 4, 5);
    Queue<SuspendedCallback> steps =
        new ArrayDeque<>(
            Collections.nCopies(expectedLineNumbers.size(), (event) -> event.prepareStepInto(1)));

    testStepping(src, "foo", new Object[] {0}, steps, expectedLineNumbers);
  }

  @Test
  public void testSteppingIntoMoreExpressionsOneLine() {
    Source src =
        createEnsoSource(
            """
            baz x = x        # 1
            bar x = x        # 2
            foo x =          # 3
                bar (baz x)  # 4
                end = 0      # 5
            """);
    List<Integer> expectedLineNumbers = List.of(4, 1, 4, 2, 4, 5);
    Queue<SuspendedCallback> steps =
        new ArrayDeque<>(
            Collections.nCopies(expectedLineNumbers.size(), (event) -> event.prepareStepInto(1)));
    testStepping(src, "foo", new Object[] {0}, steps, expectedLineNumbers);
  }

  @Test
  public void testEvaluateLazyField() {
    var fooFunc =
        createEnsoMethod(
            """
            from Standard.Base.Any import all

            type Generator
                Value n ~next

            natural =
                gen n = Generator.Value n (gen n+1)
                gen 2

            foo x =
                two = natural
                three = two.next
                end = 0
            """,
            "foo");
    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              switch (event.getSourceSection().getCharacters().toString().strip()) {
                case "end = 0" -> {
                  DebugScope scope = event.getTopStackFrame().getScope();
                  var twoValue = scope.getDeclaredValue("two");
                  assertThat(twoValue.isReadable(), is(true));
                  var nProp = twoValue.getProperty("n");
                  assertThat("n property should be readable", nProp.asInt(), is(2));
                  var nextProp = twoValue.getProperty("next");
                  assertThat("next property is readable", nextProp.isReadable(), is(true));
                }
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      fooFunc.execute(0);
    }
  }

  @Test
  public void breakInMeta() {
    Value fooFunc =
        createEnsoMethod(
            """
            from Standard.Base import Meta
            foo x =
                Meta.meta x
            """,
            "foo");

    var interceptedKind = new int[] {-1};

    try (DebuggerSession session =
        debugger.startSession(
            (SuspendedEvent event) -> {
              var code = event.getSourceSection().getCharacters().toString();
              if (code.contains("case kind:Integer")) {
                // at Meta.enso:381 currently
                var kind = event.getTopStackFrame().eval("kind");
                interceptedKind[0] = kind.asInt();
              }
              event.getSession().suspendNextExecution();
            })) {
      session.suspendNextExecution();
      var res = fooFunc.execute(42);
      assertEquals("(Primitive.Value 42)", res.toString());
      assertEquals("Primitive.Value kind", 0, interceptedKind[0]);
    }
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
     * Emulates the behavior of the chromeinspector, by getting the origin language of the value,
     * and interpreting the value in the origin language. This is problematic for host object.
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

  private static DebugValue findDebugValue(SuspendedEvent event, final String n)
      throws DebugException {
    for (var frame : event.getStackFrames()) {
      for (var v : frame.getScope().getDeclaredValues()) {
        if (v.getName().contains(n)) {
          return v;
        }
      }
    }
    return null;
  }
}
