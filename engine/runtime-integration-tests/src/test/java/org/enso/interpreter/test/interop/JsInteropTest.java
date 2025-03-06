package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class JsInteropTest {

  private static ByteArrayOutputStream out = new ByteArrayOutputStream();
  private Context ctx;

  @Before
  public void initContext() {
    ctx = ContextUtils.createDefaultContext(out);
  }

  @After
  public void disposeCtx() throws IOException {
    ctx.close();
    ctx = null;
    out.close();
    out = null;
  }

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
    Value res = ContextUtils.evalModule(ctx, src);
    assertEquals("{\"a\":1}", res.toString());
  }
}
