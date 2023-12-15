package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import java.nio.file.Paths;
import java.util.Map;
import java.util.function.Predicate;
import java.util.logging.Level;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.test.instruments.NodeCountingTestInstrument;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class AvoidIdInstrumentationTagTest {

  private Context context;
  private NodeCountingTestInstrument nodes;

  @Before
  public void initContext() {
    context =
        Context.newBuilder()
            .allowExperimentalOptions(true)
            .option(
                RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
                Paths.get("../../distribution/component").toFile().getAbsolutePath())
            .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
            .logHandler(System.err)
            .allowExperimentalOptions(true)
            .allowIO(IOAccess.ALL)
            .allowAllAccess(true)
            .build();

    var engine = context.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));

    nodes =
        engine
            .getInstruments()
            .get(NodeCountingTestInstrument.INSTRUMENT_ID)
            .lookup(NodeCountingTestInstrument.class);
    nodes.enable();
  }

  @After
  public void disposeContext() {
    context.close();
  }

  @Test
  public void avoidIdInstrumentationInLambdaMapFunctionWithFloor() throws Exception {
    var code =
        """
    from Standard.Base import all
    import Standard.Visualization

    run n = 0.up_to n . map i-> 1.floor * i
    """;
    var src = Source.newBuilder("enso", code, "TestLambda.enso").build();
    var module = context.eval(src);
    var run = module.invokeMember("eval_expression", "run");
    var res = run.execute(10000);
    assertEquals("Array of the requested size computed", 10000, res.getArraySize());

    Predicate<SourceSection> isLambda =
        (ss) -> {
          var sameSrc =
              ss.getSource().getCharacters().toString().equals(src.getCharacters().toString());
          var st = ss.getCharacters().toString();
          return sameSrc && st.contains("floor") && !st.contains("map");
        };

    assertAvoidIdInstrumentationTag(isLambda);
  }

  @Test
  public void avoidIdInstrumentationInLambdaMapFunctionYear2010() throws Exception {
    var code =
        """
    from Standard.Base import all

    operator13 = [ 1973, 1975, 2005, 2006 ]
    operator15 = operator13.map year-> if year < 2000 then [255, 100] else if year < 2010 then [0, 255] else [0, 100]
    """;
    var src = Source.newBuilder("enso", code, "YearLambda.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "operator15");
    assertEquals("Array of the requested size computed", 4, res.getArraySize());
    for (var i = 0; i < res.getArraySize(); i++) {
      var element = res.getArrayElement(i);
      assertTrue("Also array", element.hasArrayElements());
      assertEquals("Size is 2", 2, element.getArraySize());
    }

    Predicate<SourceSection> isLambda =
        (ss) -> {
          var sameSrc =
              ss.getSource().getCharacters().toString().equals(src.getCharacters().toString());
          var st = ss.getCharacters().toString();
          return sameSrc && st.contains("2010") && !st.contains("map");
        };

    assertAvoidIdInstrumentationTag(isLambda);
  }

  @Test
  public void avoidIdInstrumentationInMap() throws Exception {
    var code =
        """
    from Standard.Base import all

    run =
        operator1 = [ "FooBar", "whateveR" ]
        fun1 = _.to_case Case.Lower
        operator2 = operator1.map fun1
        operator2
    """;
    var src = Source.newBuilder("enso", code, "CaseLambda.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "run");
    assertEquals("Array of the requested size computed", 2, res.getArraySize());

    Predicate<SourceSection> isLambda =
        (ss) -> {
          var sameSrc =
              ss.getSource().getCharacters().toString().equals(src.getCharacters().toString());
          var st = ss.getCharacters().toString();
          return sameSrc && st.contains("Case.Lower") && !st.contains("to_case");
        };

    assertAvoidIdInstrumentationTag(isLambda);
  }

  private void assertAvoidIdInstrumentationTag(Predicate<SourceSection> isLambda) {
    var found = nodes.assertNewNodes("Give me nodes", 0, 10000);
    var err = new StringBuilder();
    var missingTagInLambda = false;
    var count = 0;
    for (var nn : found.values()) {
      for (var n : nn) {
        var ss = n.getSourceSection();
        if (ss == null) {
          continue;
        }
        if (isLambda.test(ss)) {
          err.append("\n")
              .append("code: ")
              .append(ss.getCharacters())
              .append(" for node ")
              .append(n.getClass().getName());
          if (n instanceof InstrumentableNode in) {
            if (!hasAvoidIdInstrumentationTag(err, in, n.getRootNode())) {
              missingTagInLambda = true;
            } else {
              count++;
            }
          }
        }
      }
    }
    if (missingTagInLambda) {
      fail(err.toString());
    }
    assertNotEquals("Found some nodes", 0, count);
  }

  private boolean hasAvoidIdInstrumentationTag(
      StringBuilder err, InstrumentableNode in, RootNode rn) {
    var hasAvoidIdInstrumentationTag = in.hasTag(AvoidIdInstrumentationTag.class);
    if (!hasAvoidIdInstrumentationTag) {
      err.append("\nERROR!");
    }

    err.append("\n").append("  AvoidIdInstrumentationTag: ").append(hasAvoidIdInstrumentationTag);
    err.append("\n").append("  IdentifiedTag: ").append(in.hasTag(IdentifiedTag.class));
    err.append("\n")
        .append("  ExpressionTag: ")
        .append(in.hasTag(StandardTags.ExpressionTag.class));
    err.append("\n").append("  RootNode: ").append(rn);
    if (rn instanceof ClosureRootNode crn) {
      err.append("\n")
          .append("  ClosureRootNode.subject to instr: ")
          .append(crn.isSubjectToInstrumentation());
      err.append("\n").append("  ClosureRootNode.used in bindings: ").append(crn.isUsedInBinding());
    }
    return hasAvoidIdInstrumentationTag;
  }
}
