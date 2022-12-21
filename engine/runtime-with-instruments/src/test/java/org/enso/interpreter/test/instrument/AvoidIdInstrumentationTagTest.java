package org.enso.interpreter.test.instrument;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.io.OutputStream;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;

public class AvoidIdInstrumentationTagTest {

  private Engine engine;
  private Context context;
  private NodeCountingTestInstrument nodes;

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

    nodes = engine.getInstruments().get(NodeCountingTestInstrument.INSTRUMENT_ID).lookup(NodeCountingTestInstrument.class);
    nodes.enable();
  }

  @After
  public void disposeContext() {
    context.close();
    engine.close();
  }

  @Test
  public void avoidIdInstrumentationInLambdaMapFunctionWithNoise() {
    var code = """
    from Standard.Base import all
    import Standard.Visualization

    run n = 0.up_to n . map i-> 1.noise * i
    """;

    var module = context.eval("enso", code);
    var run = module.invokeMember("eval_expression", "run");
    var res = run.execute(10000);
    assertEquals("Array of the requested size computed", 10000, res.getArraySize());

    Predicate<SourceSection> isLambda = (ss) -> {
      var st = ss.getCharacters().toString();
      return st.contains("noise") && !st.contains("map");
    };

    assertAvoidIdInstrumentationTag(isLambda);
  }

  private void assertAvoidIdInstrumentationTag(Predicate<SourceSection> isLambda) {
    var found = nodes.assertNewNodes("Give me nodes", 0, 10000);
    var err = new StringBuilder();
    var missingTagInLambda = false;
    for (var nn : found.values()) {
      for (var n : nn) {
        var ss = n.getSourceSection();
        if (ss == null) {
          continue;
        }
        if (isLambda.test(ss)) {
          err.append("\n").append("code: ").append(ss.getCharacters()).append(" for node ").append(n.getClass().getName());
          if (n instanceof InstrumentableNode in) {
            final boolean hasAvoidIdInstrumentationTag = in.hasTag(AvoidIdInstrumentationTag.class);
            if (!hasAvoidIdInstrumentationTag) {
              missingTagInLambda = true;
            }

            err.append("\n").append("  AvoidIdInstrumentationTag: ").append(hasAvoidIdInstrumentationTag);
            err.append("\n").append("  IdentifiedTag: ").append(in.hasTag(IdentifiedTag.class));
            err.append("\n").append("  ExpressionTag: ").append(in.hasTag(StandardTags.ExpressionTag.class));
            err.append("\n").append("  RootNode: ").append(n.getRootNode());
            if (n.getRootNode() instanceof ClosureRootNode crn) {
              err.append("\n").append("  ClosureRootNode.subject to instr: ").append(crn.isSubjectToInstrumentation());
              err.append("\n").append("  ClosureRootNode.used in bindings: ").append(crn.isUsedInBinding());
            }
          }
        }
      }
    }
    if (missingTagInLambda) {
      fail(err.toString());
    }
  }
}
