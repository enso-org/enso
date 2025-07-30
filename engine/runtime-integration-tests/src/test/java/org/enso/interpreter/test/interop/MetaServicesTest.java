package org.enso.interpreter.test.interop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import org.enso.common.RuntimeOptions;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Source;
import org.junit.ClassRule;
import org.junit.Test;

public class MetaServicesTest {
  @ClassRule
  public static ContextUtils ctx =
      ContextUtils.newBuilder()
          .withModifiedContext(
              (b) -> {
                return b.option(RuntimeOptions.DISABLE_PRIVATE_CHECK, "true");
              })
          .build();

  @Test
  public void loadFileSystemServices() throws Exception {
    final URI uri = new URI("memory://services.enso");
    final Source src =
        Source.newBuilder(
                "enso",
                """
    import Standard.Base.System.File.File_System_SPI
    import Standard.Base.Internal.Meta_Helpers
    spis =
        Meta_Helpers.lookup_services File_System_SPI
    """,
                "services.enso")
            .uri(uri)
            .buildLiteral();

    var arr = ctx.evalModule(src, "spis");

    assertTrue("Got SPIs", arr.hasArrayElements());
    var len = arr.getArraySize();
    for (var i = 0L; i < len; i++) {
      var p = arr.getArrayElement(i);
      System.err.println("found " + p);
      if (p.getMember("protocol").asString().equals("enso")) {
        var type = p.getMember("typ");
        assertTrue("It is a type", type.isMetaObject());
        assertEquals("Enso_File", type.getMetaSimpleName());
        return;
      }
    }
    fail("Not found `enso` file protocol among: " + arr);
  }
}
