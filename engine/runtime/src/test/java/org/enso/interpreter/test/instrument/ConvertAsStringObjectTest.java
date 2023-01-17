package org.enso.interpreter.test.instrument;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.io.OutputStream;
import java.nio.file.Paths;
import org.enso.interpreter.instrument.job.VisualizationResult;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.BeforeClass;
import org.junit.Test;

public class ConvertAsStringObjectTest {
  @Test
  public void conversionOfStringObject() {
    var wrap = new AsString("Hello");
    var wrapRes = VisualizationResult.visualizationResultToBytes(wrap);
    assertNotNull("Special String object converted", wrapRes);
    var directRes = VisualizationResult.visualizationResultToBytes("Hello");
    assertArrayEquals("Both arrays are the same", wrapRes, directRes);
  }

  @ExportLibrary(InteropLibrary.class)
  static class AsString implements TruffleObject {
    private final String value;

    private AsString(String value) {
      this.value = value;
    }

    @ExportMessage
    boolean isString() {
      return true;
    }

    @ExportMessage
    String asString() {
      return value;
    }
  }
}
