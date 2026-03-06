package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.junit.Rule;
import org.junit.Test;

public class InsightInEnsoTest {
  @Rule public final ContextUtils ctxRule = ContextUtils.newBuilder().assertGC(false).build();

  @Test
  public void letInsightObserveLoadingOfEnsoFilesWithoutImport() throws Exception {
    var insightCode =
        """
        insight.on "source" \\ev->
            Standard.Base.IO.println "Loading "+ev.name.to_text
        """;
    assertLoading(insightCode);
  }

  @Test
  public void letInsightObserveLoadingOfEnsoFilesWithImport() throws Exception {
    var insightCode =
        """
        import Standard.Base.IO

        insight.on "source" \\ev->
            IO.println "Loading "+ev.name.to_text
        """;
    assertLoading(insightCode);
  }

  @Test
  public void letInsightObserveLoadingOfEnsoFilesWithFromImport() throws Exception {
    var insightCode =
        """
        from Standard.Base import IO

        insight.on "source" \\ev->
            IO.println "Loading "+ev.name.to_text
        """;
    assertLoading(insightCode);
  }

  private void assertLoading(String insightCode) throws Exception {
    try (var _ = registerInsight(insightCode)) {
      var code =
          """
          from Standard.Base import all

          value = 6 * 7
          """;
      var value = ctxRule.evalModule(code, "Some_File.enso", "value");
      assertEquals(42, value.asInt());

      var loading =
          ctxRule
              .getStdOut()
              .lines()
              .filter(l -> l.startsWith("Loading "))
              .collect(Collectors.joining("\n"));

      assertEquals(
          "Loading Some_File.enso",
          """
          Loading Some_File.enso
          Loading IO.enso
          Loading Text.enso
          Loading Some_File
          Loading Numbers.enso\
          """,
          loading);
    }
  }

  @Test
  public void computeFactorial() throws Exception {
    try (var _ = registerTraceOfLocalVariables("fac")) {
      var code =
          Source.newBuilder(
                  "enso",
                  """
                  import Standard.Base.Data.Numbers
                  fac n =
                      acc n v = if n <= 1 then v else
                          @Tail_Call acc n-1 n*v

                      acc n 1
                  """,
                  "factorial.enso")
              .build();

      var m = ctxRule.eval(code);
      var fac = m.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "fac");
      var res = fac.execute(5);
      assertEquals(120, res.asInt());

      var msgs = ctxRule.getOut();
      assertContainsAll("Step one: " + msgs, msgs, "n=5", "v=1", "acc=factorial.fac.acc");
      assertContainsAll("Step two: " + msgs, msgs, "n=4", "v=5", "acc=factorial.fac.acc");
      assertContainsAll("3rd step: " + msgs, msgs, "n=3", "v=20", "acc=factorial.fac.acc");
      assertContainsAll("4th step: " + msgs, msgs, "n=2", "v=60", "acc=factorial.fac.acc");

      assertNotEquals(
          "Uninitialized variables (seen as Enso Nothing) are there: " + msgs,
          -1,
          msgs.indexOf("Nothing"));
    }
  }

  private AutoCloseable registerTraceOfLocalVariables(String method) throws AssertionError {
    var insightCode =
        """
        from Standard.Base import True, Dictionary, IO, Polyglot, Meta

        when = Dictionary.empty
            . insert "roots" True
            . insert "rootNameFilter" ".*${method}.*"

        log ctx frame =
            IO.println ctx.name+" at "+ctx.source.name+":"+ctx.line.to_text+":"
            members = Polyglot.get_members frame
            line = members.fold "" \\sb -> \\p->
                sb + "  " + p + "=" + (Polyglot.get_member frame p).to_text
            IO.println line

        insight.on "enter" log when
        """
            .replace("${method}", method);
    return registerInsight(insightCode);
  }

  @Test
  public void instantiateConstructor() throws Exception {
    try (var _ = registerTraceOfLocalVariables("omplex")) {
      doInstantiateConstructor(false, false);
    }
  }

  @Test
  public void instantiateAutoscopedConstructor() throws Exception {
    try (var _ = registerTraceOfLocalVariables("omplex")) {
      doInstantiateConstructor(true, false);
    }
  }

  @Test
  public void lazyInstantiateConstructor() throws Exception {
    try (var _ = registerTraceOfLocalVariables("omplex")) {
      doInstantiateConstructor(false, true);
    }
  }

  @Test
  public void lazyInstantiateAutoscopedConstructor() throws Exception {
    try (var _ = registerTraceOfLocalVariables("omplex")) {
      doInstantiateConstructor(true, true);
    }
  }

  private void doInstantiateConstructor(boolean useAutoscoping, boolean lazy) throws Exception {
    var code =
        Source.newBuilder(
                "enso",
                """
                id x = x
                init_first_switch_arg x = x

                type Complex
                    Number re im

                    switch f=(init_first_switch_arg id) n:Complex = Complex.Number (f n.im) (f n.re)
                    switch_lazy f=(init_first_switch_arg id) (~n:Complex) = Complex.Number (f n.im) (f n.re)

                alloc1 a b = Complex.switch n=(Complex.Number a b)
                alloc2 a b = Complex.switch n=(..Number a b)
                alloc3 a b = Complex.switch_lazy n=(Complex.Number a b)
                alloc4 a b = Complex.switch_lazy n=(..Number a b)
                """,
                "complex.enso")
            .build();

    var m = ctxRule.eval(code);
    var alloc1 = m.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "alloc1");
    var alloc2 = m.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "alloc2");
    var alloc3 = m.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "alloc3");
    var alloc4 = m.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "alloc4");

    var useAlloc = useAutoscoping ? (lazy ? alloc4 : alloc2) : (lazy ? alloc3 : alloc1);
    var res = useAlloc.execute(3, 4);
    assertEquals("Complex", res.getMetaObject().getMetaSimpleName());
    assertEquals(3, res.getMember("im").asInt());
    assertEquals(4, res.getMember("re").asInt());

    var msgs = ctxRule.getOut();

    var firstCons = msgs.indexOf("complex::complex.Complex::Number");
    var secondCons = msgs.lastIndexOf("complex::complex.Complex::Number");
    var switchInitCall = msgs.indexOf("complex::complex::init_first_switch_arg");

    assertNotEquals(msgs, -1, switchInitCall);
    assertNotEquals(msgs, -1, firstCons);
    assertNotEquals(msgs, -1, secondCons);
    assertTrue(
        "First constructor call must be sooner than second:\n" + msgs, firstCons < secondCons);

    if (useAutoscoping || lazy) {
      assertTrue(
          "Switch call ("
              + switchInitCall
              + ") first and then both constructors ("
              + firstCons
              + "):\n"
              + msgs,
          switchInitCall < firstCons);
    } else {
      assertTrue("First constructor sooner than switch call:\n" + msgs, firstCons < switchInitCall);
      assertTrue(
          "Switch call sooner than second constructor:\n" + msgs, switchInitCall < secondCons);
    }
  }

  private void assertContainsAll(String msg, String text, String... expected) {
    NEXT_LINE:
    for (var line : text.split("\n")) {
      for (var w : expected) {
        if (line.indexOf(w) == -1) {
          continue NEXT_LINE;
        }
      }
      // found all expected
      return;
    }
    fail(msg + " expecting " + Arrays.asList(expected));
  }

  private AutoCloseable registerInsight(String insightCode) throws AssertionError {
    var ctx = ctxRule.context();
    var engine = ctx.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    @SuppressWarnings("unchecked")
    var fn =
        (Function<Source, AutoCloseable>)
            engine.getInstruments().get("insight").lookup(Function.class);
    assertNotNull(fn);
    Source insightScript;
    try {
      insightScript = Source.newBuilder("enso", insightCode, "trace_sources.enso").build();
    } catch (IOException e) {
      throw new AssertionError(e);
    }
    return fn.apply(insightScript);
  }
}
