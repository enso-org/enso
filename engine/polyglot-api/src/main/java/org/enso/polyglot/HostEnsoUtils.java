package org.enso.polyglot;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import org.graalvm.polyglot.PolyglotException;

public final class HostEnsoUtils {
  private HostEnsoUtils() {
  }

  /**
   * Extracts a string representation for a polyglot exception.
   *
   * @param exexception the exception
   * @return message representing the exception
   */
  public static String findExceptionMessage(Throwable ex) {
    return ex.getMessage() != null ? ex.getMessage() : switch (ex) {
      case PolyglotException exception when exception.isHostException() ->
        exception.asHostException().getClass().getName();
      default ->
        ex.getClass().getName();
    };
  }
}
