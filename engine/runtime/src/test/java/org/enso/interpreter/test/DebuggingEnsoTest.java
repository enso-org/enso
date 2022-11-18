package org.enso.interpreter.test;

import com.oracle.truffle.api.debug.Breakpoint;
import com.oracle.truffle.api.debug.DebugException;
import com.oracle.truffle.api.debug.DebugStackFrame;
import com.oracle.truffle.api.debug.DebugValue;
import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.DebuggerSession;
import com.oracle.truffle.api.debug.SuspendedEvent;
import com.oracle.truffle.tck.DebuggerTester;
import java.net.URI;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Assert;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class DebuggingEnsoTest {
  private Context context;
  private Engine engine;
  private Debugger debugger;
  private DebuggerTester debuggerTester;

  @Before
  public void initTester() {
    engine = Engine.newBuilder()
        .allowExperimentalOptions(true)
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../test/micro-distribution/component").toFile().getAbsolutePath()
        ).build();

    debuggerTester = new DebuggerTester(
        Context.newBuilder()
            .engine(engine)
            .allowIO(true)
    );
  }

  @After
  public void disposeContext() {
    context.close();
    engine.close();
  }

  @Test
  public void callerVariablesAreVisibleOnPreviousStackFrame() {
    Source fooSource = Source.create("enso", """
        bar arg_bar =
            loc_bar = arg_bar + 1  # BreakPoint
            loc_bar
            
        foo =
            loc_foo = 1
            bar loc_foo
        """);

    Value module = context.eval(fooSource);
    Value fooFunc = module.invokeMember("eval_expression", "foo");

    try (DebuggerSession session = debuggerTester.startSession()) {
      debuggerTester.startEval(fooSource);
      var breakpoint = Breakpoint
          .newBuilder(DebuggerTester.getSourceImpl(fooSource))
          .lineIs(2)
          .build();
      session.install(breakpoint);

      debuggerTester.expectSuspended((SuspendedEvent event) -> {
        List<DebugStackFrame> stackFrames = new ArrayList<>();
        event.getStackFrames().forEach(stackFrames::add);
      });
    }
  }

  @Test
  public void evaluation() throws Exception {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../test/micro-distribution/component").toFile().getAbsolutePath()
      ).build();
    Context ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    org.junit.Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));

    final URI facUri = new URI("memory://fac.enso");
    final Source facSrc = Source.newBuilder("enso", """
    fac : Number -> Number
    fac n =
        facacc : Number -> Number -> Number
        facacc n accumulator =
                stop = n <= 1
                if stop then accumulator else @Tail_Call facacc n-1 n*accumulator

        facacc n 1
    """, "fac.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var facFn = module.invokeMember("eval_expression", "fac");
    final var dbg = Debugger.find(eng);
    final var values = new TreeSet<Integer>();
    try (var session = dbg.startSession((event) -> {
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

  // TODO: Re-enable
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
    var stackFrames = event.getStackFrames();
    var debugScope = event.getTopStackFrame().getScope();
    var declaredValues = debugScope.getDeclaredValues();
    for (var v : event.getTopStackFrame().getScope().getDeclaredValues()) {
      if (v.getName().contains(n)) {
        return v;
      }
    }
    return null;
  }
}
