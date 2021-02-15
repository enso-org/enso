package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;

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
    super(location);
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

  @Override
  public String getMessage() {
    return payload.toString();
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

  @ExportMessage
  boolean hasSourceLocation() {
    return getLocation().getEncapsulatingSourceSection() != null;
  }

  @ExportMessage(name = "getSourceLocation")
  SourceSection getSourceSection() throws UnsupportedMessageException {
    SourceSection loc = getLocation().getEncapsulatingSourceSection();
    if (loc == null) {
      throw UnsupportedMessageException.create();
    }
    return getLocation().getEncapsulatingSourceSection();
  }
}
