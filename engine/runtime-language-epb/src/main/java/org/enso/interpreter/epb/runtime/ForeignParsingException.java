package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;

/**
 * This exception is thrown once a foreign method cannot be parsed because the associated foreign
 * language is not installed, or enabled, in the Truffle engine.
 */
@ExportLibrary(InteropLibrary.class)
public class ForeignParsingException extends AbstractTruffleException {
  private final String message;

  public ForeignParsingException(String truffleLangId, Node location) {
    super(createMessage(truffleLangId), location);
    this.message = createMessage(truffleLangId);
  }

  private static String createMessage(String truffleLangId) {
    return "Failed to parse Truffle language with ID " + truffleLangId;
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
