package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.util.Map;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class InsightForEnsoTest extends TestBase {
  private Context ctx;
  private AutoCloseable insightHandle;
  private final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @Before
  public void initContext() throws Exception {
    this.ctx = defaultContextBuilder().out(out).build();

    var engine = ctx.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));

    @SuppressWarnings("unchecked")
    var fn =
        (Function<Source, AutoCloseable>)
            engine.getInstruments().get("insight").lookup(Function.class);
    assertNotNull(fn);

    var insightScript =
        Source.newBuilder(
                "js",
                """
        insight.on('enter', (ctx, frame) => {
            print(`${ctx.name} at ${ctx.source.name}:${ctx.line}:`);
            let dump = "";
            for (let p in frame) {
                frame.unknown // used to yield NullPointerException
                dump += ` ${p}=${frame[p]}`;
            }
            print(dump);
        }, {
            roots : true
        });
        """,
                "trace.js")
            .build();
    this.insightHandle = fn.apply(insightScript);
  }

  @After
  public void disposeContext() throws Exception {
    this.insightHandle.close();
    this.ctx.close();
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

    var m = ctx.eval(code);
    var fac = m.invokeMember("eval_expression", "fac");
    var res = fac.execute(5);
    assertEquals(120, res.asInt());

    var msgs = out.toString();
    assertNotEquals("Step one: " + msgs, -1, msgs.indexOf("n=5 v=1 acc=function"));
    assertNotEquals("Step two: " + msgs, -1, msgs.indexOf("n=4 v=5 acc=function"));
    assertNotEquals("3rd step: " + msgs, -1, msgs.indexOf("n=3 v=20 acc=function"));
    assertNotEquals("4th step: " + msgs, -1, msgs.indexOf("n=2 v=60 acc=function"));

    assertNotEquals(
        "Uninitialized variables are seen as JavaScript null: " + msgs,
        -1,
        msgs.indexOf("n=null v=null acc=function"));
  }

  @Test
  public void instantiateConstructor() throws Exception {
    doInstantiateConstructor(false);
  }

  @Test
  public void instantiateAutoscopedConstructor() throws Exception {
    doInstantiateConstructor(true);
  }

  private void doInstantiateConstructor(boolean useAutoscoping) throws Exception {
    var code =
        Source.newBuilder(
                "enso",
                """
                type Complex
                    Number re im

                    switch n:Complex = Complex.Number n.im n.re

                alloc1 a b = Complex.switch (Complex.Number a b)
                alloc2 a b = Complex.switch (..Number a b)
                """,
                "complex.enso")
            .build();

    var m = ctx.eval(code);
    var alloc1 = m.invokeMember("eval_expression", "alloc1");
    var alloc2 = m.invokeMember("eval_expression", "alloc2");

    var res = useAutoscoping ? alloc2.execute(3, 4) : alloc1.execute(3, 4);
    assertEquals("Complex", res.getMetaObject().getMetaSimpleName());
    assertEquals(3, res.getMember("im").asInt());
    assertEquals(4, res.getMember("re").asInt());

    var msgs = out.toString();

    var firstCons = msgs.indexOf("complex::complex.Complex::Number");
    var secondCons = msgs.lastIndexOf("complex::complex.Complex::Number");
    var switchCall = msgs.indexOf("complex::complex.Complex.type::switch");

    assertNotEquals(msgs, -1, switchCall);
    assertNotEquals(msgs, -1, firstCons);
    assertNotEquals(msgs, -1, secondCons);
    assertTrue(
        "First constructor call must be sooner than second:\n" + msgs, firstCons < secondCons);

    if (useAutoscoping) {
      assertTrue("Switch call first and then both constructors:\n" + msgs, switchCall < firstCons);
    } else {
      assertTrue("First constructor sooner than switch call:\n" + msgs, firstCons < switchCall);
      assertTrue("Switch call sooner than second constructor:\n" + msgs, switchCall < secondCons);
    }
  }
}
