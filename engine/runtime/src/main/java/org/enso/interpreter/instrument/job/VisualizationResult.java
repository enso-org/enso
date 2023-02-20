package org.enso.interpreter.instrument.job;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.nio.charset.StandardCharsets;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.WithWarnings;

public final class VisualizationResult {
  private VisualizationResult() {
  }

  /**
   * Extracts a string representation for a polyglot exception.
   *
   * @param exexception the exception
   * @return message representing the exception
   */
  @CompilerDirectives.TruffleBoundary
  public static String findExceptionMessage(Throwable ex) {
    Object msg = ex;
    var iop = InteropLibrary.getUncached();
    if (iop.hasExceptionMessage(ex)) {
      try {
        msg = iop.getExceptionMessage(ex);
      } catch (UnsupportedMessageException ignore) {
        throw CompilerDirectives.shouldNotReachHere(ignore);
      }
    }
    try {
      if (iop.isString(msg)) {
        return iop.asString(msg);
      } else if (iop.hasMetaObject(msg)) {
        var meta = iop.getMetaObject(msg);
        var name = iop.getMetaQualifiedName(meta);
        return iop.asString(name);
      } else {
        return ex.getMessage() != null ? ex.getMessage() : ex.getClass().getName();
      }
    } catch (UnsupportedMessageException ignore) {
      throw CompilerDirectives.shouldNotReachHere(ignore);
    }
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
