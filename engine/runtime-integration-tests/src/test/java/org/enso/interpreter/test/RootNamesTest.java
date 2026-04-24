package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class RootNamesTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.newBuilder().build();

  @Before
  public void cleanOut() {
    ctxRule.resetOut();
  }

  @BeforeClass
  public static void initCtx() {
    var ctx = ctxRule.context();
    var engine = ctx.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));

    @SuppressWarnings("unchecked")
    var fn =
        (Function<Source, AutoCloseable>)
            engine.getInstruments().get("insight").lookup(Function.class);
    assertNotNull(fn);

    Source insightScript = null;
    try {
      insightScript =
          Source.newBuilder(
                  "js",
                  """
                  insight.on('enter', (ctx, frame) => {
                      print(`ENTER: ${ctx.name}`);
                  }, {
                      roots : true
                  });
                  """,
                  "trace.js")
              .build();
    } catch (IOException e) {
      throw new AssertionError(e);
    }
    fn.apply(insightScript);
  }

  @Test
  public void computeFactorial() throws Exception {
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
    var res = fac.execute(3);
    assertEquals(6, res.asInt());

    var msgs = ctxRule.getOut();
    var closures =
        msgs.lines()
            .filter(l -> l.startsWith("ENTER: "))
            .map(l -> l.substring(7))
            .collect(Collectors.toSet());

    assertEquals("Few closures: " + closures, 2, closures.size());
    assertTrue(
        "Fully qualified name for method: " + closures,
        closures.contains("factorial::factorial::fac"));
    assertTrue(
        "Name with dots for local method: " + closures, closures.contains("factorial.fac.acc"));
  }

  @Test
  public void useNameOfPreviousBinding() throws Exception {
    var code =
        Source.newBuilder(
                "enso",
                """
                import Standard.Base.Data.Numbers

                compute a b =
                  plus = x-> y-> x+y
                  mul = x-> y-> x*y

                  mul (plus a b) (plus b a)
                """,
                "bindings_test.enso")
            .build();

    var m = ctxRule.eval(code);
    var compute = m.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "compute");
    var powerOfEight = compute.execute(3, 5);
    assertEquals(64, powerOfEight.asInt());

    var msgs = ctxRule.getOut();
    var closures =
        msgs.lines()
            .filter(l -> l.startsWith("ENTER: "))
            .map(l -> l.substring(7))
            .collect(Collectors.toSet());

    assertEquals("Few closures: " + closures, 3, closures.size());
    assertTrue(
        "Fully qualified name for method: " + closures,
        closures.contains("bindings_test::bindings_test::compute"));
    assertTrue(
        "Name taken from previous binding plus: " + closures,
        closures.contains("bindings_test.compute.plus"));
    assertTrue(
        "Name taken from previous binding mul: " + closures,
        closures.contains("bindings_test.compute.mul"));
  }
}
