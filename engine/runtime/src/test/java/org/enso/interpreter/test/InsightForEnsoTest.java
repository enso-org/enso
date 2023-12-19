package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.Map;
import java.util.function.Function;
import java.util.logging.Level;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class InsightForEnsoTest {
  private Context ctx;
  private AutoCloseable insightHandle;
  private final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @Before
  public void initContext() throws Exception {
    this.ctx =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .out(out)
            .allowAllAccess(true)
            .build();

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
}
