package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;

import org.enso.test.utils.ContextUtils;
import org.enso.testkit.ReportLogsOnFailureRule;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;

public class JsInteropTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  @Rule public ReportLogsOnFailureRule appenderRule = new ReportLogsOnFailureRule();

  @Test
  public void testDefaultJSPrint() {
    var src =
        """
      from Standard.Base import Json

      main =
        json = Json.parse <| '''
          {
            "inner": {
              "a": 1
            }
          }
        json.get "inner"
      """;
    Value res = ctxRule.evalModule(src);
    assertEquals("{\"a\":1}", res.toString());
  }
}
