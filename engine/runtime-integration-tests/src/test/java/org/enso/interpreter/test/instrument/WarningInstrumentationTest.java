package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import java.util.Map;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.test.Metadata;
import org.enso.interpreter.test.instruments.NodeCountingTestInstrument;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class WarningInstrumentationTest {

  @ClassRule public static final ContextUtils ctxRule = ContextUtils.newBuilder().build();

  private static NodeCountingTestInstrument instrument;

  @BeforeClass
  public static void initContext() {
    var context = ctxRule.context();
    var engine = context.getEngine();
    Map<String, Language> langs = engine.getLanguages();
    Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));

    instrument =
        engine
            .getInstruments()
            .get(NodeCountingTestInstrument.INSTRUMENT_ID)
            .lookup(NodeCountingTestInstrument.class);
    SourceSectionFilter builder =
        SourceSectionFilter.newBuilder()
            .tagIs(StandardTags.ExpressionTag.class, StandardTags.CallTag.class)
            .tagIs(IdentifiedTag.class)
            .tagIsNot(AvoidIdInstrumentationTag.class)
            .build();
    instrument.enable(builder);
  }

  @AfterClass
  public static void disposeContext() {
    instrument = null;
  }

  @Test
  public void instrumentValueWithWarnings() throws Exception {
    var metadata = new Metadata("");

    var idOp1 = metadata.addItem(140, 34, null);
    var idOp2 = metadata.addItem(191, 31, null);
    var idOp3 = metadata.addItem(239, 13, null);
    var rawCode =
        """
        from Standard.Base import all
        from Standard.Base.Warning import Warning
        from Standard.Table import Table

        run column_name =
            operator1 = Table.new [[column_name, [1,2,3]]]
            operator2 = Warning.attach "Text" operator1
            operator3 = operator2.get
            operator3
        """;
    var code = metadata.appendToCode(rawCode);
    var src = Source.newBuilder("enso", code, "TestWarning.enso").build();
    var module = ctxRule.eval(src);
    var res = module.invokeMember("eval_expression", "run");
    res.execute("A");

    var calls = instrument.registeredCalls();

    assertEquals(3, calls.keySet().size());
    assertEquals("new", calls.get(idOp1).functionName());
    assertEquals("attach", calls.get(idOp2).functionName());
    assertTrue(calls.get(idOp3).typeName().contains("Table"));
    assertEquals("get", calls.get(idOp3).functionName());
  }
}
