package org.enso.interpreter.test;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.assertEquals;

import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.StepConfig;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import org.enso.test.utils.ContextUtils;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class DebuggingControlFlowTest {
  @ClassRule
  public static final ContextUtils ctx = ContextUtils.newBuilder("enso").assertGC(false).build();

  private static Debugger debugger;
  private static final ByteArrayOutputStream out = new ByteArrayOutputStream();

  @BeforeClass
  public static void initContext() {
    debugger = Debugger.find(ctx.getEngine());
  }

  @AfterClass
  public static void disposeContext() throws IOException {
    debugger = null;
  }

  @Test
  public void stepsOverBlock() {
    var prelude =
        """
        from Standard.Base import Integer

        main a:Integer=7 b:Integer=5 c:Integer=3 =
        """;

    var body =
        """
            bc = b*c
            ac = a*c
            ab = b*a
            plus = ab + ac + bc
            plus
        """;

    var code = prelude + body;

    var events = new ArrayList<String>();
    try (com.oracle.truffle.api.debug.DebuggerSession session =
        debugger.startSession(
            (event) -> {
              events.add(event.getSourceSection().getCharacters().toString());
              event.prepareStepOver(StepConfig.newBuilder().build());
            })) {
      session.suspendNextExecution();
      var res = ctx.evalModule(code);
      assertEquals(71, res.asInt());
    }
    assertSuspendedEvents(events, 5, body.split("\n"));
  }

  @Test
  public void stepsOverIf() {
    var prelude =
        """
        from Standard.Base import Integer

        main a:Integer=7 b:Integer=5 c:Integer=3 =
        """;

    var bc =
        """
            bc = b*c
        """;
    var cond =
        """
            if bc < 10 then 0 else
                ac = a*c
                ab = b*a
                plus = ab + ac + bc
                plus
        """;

    var body = bc + cond;
    var code = prelude + body;

    var events = new ArrayList<String>();
    try (com.oracle.truffle.api.debug.DebuggerSession session =
        debugger.startSession(
            (event) -> {
              events.add(event.getSourceSection().getCharacters().toString());
              event.prepareStepOver(StepConfig.newBuilder().build());
            })) {
      session.suspendNextExecution();
      var res = ctx.evalModule(code);
      assertEquals(71, res.asInt());
    }
    var bodyLines = body.split("\n");
    bodyLines[1] = cond; // if statement section is full `cond`
    assertSuspendedEvents(events, 6, bodyLines);
  }

  @Test
  public void stepsOverCase() {
    var prelude =
        """
        from Standard.Base import Integer

        main a:Integer=7 b:Integer=5 c:Integer=3 =
        """;

    var bc =
        """
            bc = b*c
        """;
    var caseBegin =
        """
            case bc of
                5 ->
                    bc
                15 ->
        """;

    var caseBody =
        """
                    ac = a*c
                    ab = b*a
                    plus = ab + ac + bc
                    plus
        """;
    var caseTail =
        """
                _ ->
                    -bc
        """;

    var body = bc + "case of line placeholder\n" + caseBody;
    var code = prelude + bc + caseBegin + caseBody + caseTail;

    var events = new ArrayList<String>();
    try (com.oracle.truffle.api.debug.DebuggerSession session =
        debugger.startSession(
            (event) -> {
              events.add(event.getSourceSection().getCharacters().toString());
              event.prepareStepOver(StepConfig.newBuilder().build());
            })) {
      session.suspendNextExecution();
      var res = ctx.evalModule(code);
      assertEquals(71, res.asInt());
    }
    var bodyLines = body.split("\n");
    bodyLines[1] = caseBegin + caseBody + caseTail; // whole case statement section
    assertSuspendedEvents(events, 6, bodyLines);
  }

  private static void assertSuspendedEvents(
      ArrayList<String> events, final int expectedStops, String... bodyLines) {
    if (events.size() != expectedStops) {
      var sb = new StringBuilder("Events:");
      var cnt = 0;
      for (var e : events) {
        sb.append("\n#").append(cnt++).append(": ").append(e);
      }
      assertEquals(sb.toString(), expectedStops, events.size());
    }
    for (var i = 0; i < events.size(); i++) {
      if (bodyLines[i] == null) {
        continue;
      }
      assertEquals("At " + i, bodyLines[i].trim(), events.get(i));
    }
  }
}
