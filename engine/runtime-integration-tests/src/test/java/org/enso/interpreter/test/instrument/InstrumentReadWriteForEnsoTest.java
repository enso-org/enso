package org.enso.interpreter.test.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.function.Consumer;
import org.enso.interpreter.test.instruments.VariablesTestInstrument;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Test;

public class InstrumentReadWriteForEnsoTest {
  @ClassRule
  public static final ContextUtils ctxRule = ContextUtils.newBuilder().assertGC(false).build();

  @Test
  public void verifyInJavaScript() throws Exception {
    var context = ctxRule.context();
    var main =
        context.eval(
            "js",
            """
            (function main(n) {
              let a = n + 2;
              let b = n * 3;
              let r = b - a;
              return r;
            });
            """);
    verifyReadAndWriteTrace(main);
  }

  @Test
  public void verifyInEnso() throws Exception {
    var main =
        ctxRule.evalModule(
            """
            from Standard.Base import all

            main n =
              a = n + 2
              b = n * 3
              r = b - a
              r
            """);
    verifyReadAndWriteTrace(main);
  }

  private void verifyReadAndWriteTrace(Value main) {
    var w = new StringWriter();
    var pw = new PrintWriter(w);

    var instr = ctxRule.context().getEngine().getInstruments();
    var vars = instr.get(VariablesTestInstrument.ID);
    assertNotNull("VariablesInstrument found among " + instr, vars);
    @SuppressWarnings("unchecked")
    Consumer<PrintWriter> tracer = vars.lookup(Consumer.class);

    tracer.accept(pw);
    var res = main.execute(5);
    tracer.accept(null);

    assertEquals(8, res.asInt());
    var expectedTrace =
        """
        writeVariableNameEnter n
        writeVariableNameReturn = 5
        writeVariableNameEnter a
          readVariableNameEnter n
          readVariableNameReturn = 5
        writeVariableNameReturn = 7
        writeVariableNameEnter b
          readVariableNameEnter n
          readVariableNameReturn = 5
        writeVariableNameReturn = 15
        writeVariableNameEnter r
          readVariableNameEnter b
          readVariableNameReturn = 15
          readVariableNameEnter a
          readVariableNameReturn = 7
        writeVariableNameReturn = 8
        readVariableNameEnter r
        readVariableNameReturn = 8
        """;
    var trace = w.toString().replace("\r\n", "\n");
    assertEquals(
        "Expected traces generated and show that:" //
            + "- a depends on n" //
            + "- b depends on n" //
            + "- r depends on a and b", //
        expectedTrace,
        trace);
  }
}
