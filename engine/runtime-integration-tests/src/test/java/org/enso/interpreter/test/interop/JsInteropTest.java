package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;

import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.ClassRule;
import org.junit.Test;

public class JsInteropTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

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
