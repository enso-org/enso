package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.util.Set;

/**
 * This exception is thrown once a foreign method cannot be parsed because the associated foreign
 * language is not installed, or enabled, in the Truffle engine.
 */
@ExportLibrary(InteropLibrary.class)
public class ForeignParsingException extends AbstractTruffleException {
  private final String message;

  /**
   * @param truffleLangId ID of the language that caused the parsing exception, i.e., ID of the
   *     language that is not installed in the GraalVM distribution.
   * @param installedLangs Set of all the installed (supported) language IDs in the GraalVM
   *     distribution.
   * @param location Location node passed to {@link AbstractTruffleException}.
   */
  public ForeignParsingException(String truffleLangId, Set<String> installedLangs, Node location) {
    super(createMessage(truffleLangId, installedLangs), location);
    this.message = createMessage(truffleLangId, installedLangs);
  }

  private static String createMessage(String truffleLangId, Set<String> installedLangs) {
    return String.format(
        "Cannot parse foreign %s method. Only available languages are %s",
        truffleLangId, installedLangs);
  }

  @Override
  public String getMessage() {
    return message;
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  RuntimeException throwException() {
    return this;
  }

  @ExportMessage
  String toDisplayString(boolean hasSideEffects) {
    return "ForeignParsingException: '" + message + "'";
  }
}
