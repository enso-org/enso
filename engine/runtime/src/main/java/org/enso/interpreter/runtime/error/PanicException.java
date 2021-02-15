package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;

/** An exception type for user thrown panic exceptions. */
@ExportLibrary(value = InteropLibrary.class, delegateTo = "payload")
public class PanicException extends AbstractTruffleException {
  final Object payload;

  /**
   * Creates an instance of this class.
   *
   * @param payload arbitrary, user-provided payload carried by this exception
   * @param location the node throwing this exception, for use in guest stack traces
   */
  public PanicException(Object payload, Node location) {
    super(payload.toString(), location);
    this.payload = payload;
  }

  /**
   * Returns the payload in the panic.
   *
   * @return the panic payload
   */
  public Object getPayload() {
    return payload;
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  RuntimeException throwException() {
    throw this;
  }

  @ExportMessage
  ExceptionType getExceptionType() {
    return ExceptionType.RUNTIME_ERROR;
  }

  @ExportMessage
  int getExceptionExitStatus() {
    return 1;
  }

  @ExportMessage
  boolean isExceptionIncompleteSource() {
    return false;
  }
}
