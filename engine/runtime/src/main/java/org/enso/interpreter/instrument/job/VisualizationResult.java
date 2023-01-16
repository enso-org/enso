package org.enso.interpreter.instrument.job;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.nio.charset.StandardCharsets;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.WithWarnings;

public final class VisualizationResult {
  private VisualizationResult() {
  }

  public static byte[] visualizationResultToBytes(Object value) {
    if (value instanceof byte[] arr) {
      return arr;
    }
    if (value instanceof String text) {
      return text.getBytes(StandardCharsets.UTF_8);
    }
    if (value instanceof Text text) {
      return visualizationResultToBytes(text.toString());
    }
    if (value instanceof WithWarnings warn) {
      return visualizationResultToBytes(warn.getValue());
    }
    var iop = InteropLibrary.getUncached();
    if (iop.isString(value)) {
      try {
        return visualizationResultToBytes(iop.asString(value));
      } catch (UnsupportedMessageException ex) {
        // fallthru
      }
    }
    return null;
  }
}
