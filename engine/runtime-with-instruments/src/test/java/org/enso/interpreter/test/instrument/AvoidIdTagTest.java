package org.enso.interpreter.test.instrument;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.Map;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.test.NodeCountingTestInstrument;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class AvoidIdTagTest {

  private Engine engine;
  private Context context;

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

    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));
  }

  @After
  public void disposeContext() {
    context.close();
    engine.close();
  }

  @Test
  public void instrumentedNodes() {
    var nodes = engine.getInstruments().get(NodeCountingTestInstrument.INSTRUMENT_ID).lookup(NodeCountingTestInstrument.class);
    nodes.enable();

    var code = """
    from Standard.Base import all
    import Standard.Visualization

    run n = 0.up_to n . map i-> 1.noise * i
    """;

    var module = context.eval("enso", code);
    var run = module.invokeMember("eval_expression", "run");
    var res = run.execute(10000);
    var found = nodes.assertNewNodes("Give me nodes", 0, 10000);

    for (var nn : found.values()) {
      for (var n : nn) {
        var ss = n.getSourceSection();
        if (ss == null) {
          continue;
        }
        var st = ss.getCharacters().toString();
        if (st.contains("noise") && !st.contains("map")) {
          System.err.println("code: " + st + " for node " + n.getClass().getName());
          if (n instanceof InstrumentableNode in) {
            System.err.println("  AvoidIdInstrumentationTag: " + in.hasTag(AvoidIdInstrumentationTag.class));
            System.err.println("  IdentifiedTag: " + in.hasTag(IdentifiedTag.class));
            System.err.println("  ExpressionTag: " + in.hasTag(StandardTags.ExpressionTag.class));
            System.err.println("  RootNode: " + n.getRootNode());
            if (n.getRootNode() instanceof ClosureRootNode crn) {
              System.err.println("  crn.subject to instr: " + crn.isSubjectToInstrumentation());
              System.err.println("  crn.used in bindings: " + crn.isUsedInBinding());
            }
          }
        }
      }
    }
  }
}
