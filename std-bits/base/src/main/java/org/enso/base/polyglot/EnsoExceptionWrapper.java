package org.enso.base.polyglot;

import org.graalvm.polyglot.Value;

/** A utility class to convert Java exceptions to Enso errors. */
public class EnsoExceptionWrapper {
  private EnsoExceptionWrapper() {}

  private static Value makeEnsoFile(String path) {
    var fileType = EnsoMeta.getType("Standard.Base.System.File", "File");
    return fileType.invokeMember("new", path);
  }

  /**
   * Wraps common exceptions into their Enso counterparts.
   *
   * @param e the exception to wrap.
   * @return the wrapped exception or null if not supported by this method.
   */
  public static Value wrapCommonExceptions(Exception e) {
    return switch (e) {
      case IllegalArgumentException argException ->
          EnsoMeta.makeInstance(
              "Standard.Base.Errors.Illegal_Argument",
              "Illegal_Argument",
              "Error",
              argException.getMessage(),
              argException);
      case IllegalStateException stateException ->
          EnsoMeta.makeInstance(
              "Standard.Base.Errors.Illegal_State",
              "Illegal_State",
              "Error",
              stateException.getMessage(),
              stateException);
      default -> null;
    };
  }
}
