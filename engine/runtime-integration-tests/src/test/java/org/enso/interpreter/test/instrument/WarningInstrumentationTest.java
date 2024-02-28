package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import java.nio.file.Paths;
import java.util.Map;
import java.util.logging.Level;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.interpreter.test.Metadata;
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

public class WarningInstrumentationTest {

  private Context context;
  private NodeCountingTestInstrument instrument;

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

  @After
  public void disposeContext() {
    context.close();
  }

  @Test
  public void instrumentValueWithWarnings() throws Exception {
    var metadata = new Metadata("");

    var idOp1 = metadata.addItem(151, 34, null);
    var idOp2 = metadata.addItem(202, 31, null);
    var idOp3 = metadata.addItem(250, 13, null);
    var rawCode =
        """
                from Standard.Base import all
                from Standard.Base.Warning import Warning
                from Standard.Table.Data.Table import Table

                run column_name =
                    operator1 = Table.new [[column_name, [1,2,3]]]
                    operator2 = Warning.attach "Text" operator1
                    operator3 = operator2.get
                    operator3
                """;
    var code = metadata.appendToCode(rawCode);
    var src = Source.newBuilder("enso", code, "TestWarning.enso").build();
    var module = context.eval(src);
    var res = module.invokeMember("eval_expression", "run");
    res.execute("A");

    var calls = instrument.registeredCalls();

    assertEquals(calls.keySet().size(), 3);
    assertEquals(calls.get(idOp1).functionName(), "new");
    assertEquals(calls.get(idOp2).functionName(), "attach");
    assertTrue(calls.get(idOp3).typeName().contains("Table"));
    assertEquals(calls.get(idOp3).functionName(), "get");
  }
}
