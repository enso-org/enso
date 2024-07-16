package org.enso.interpreter.service.error;

import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;

/** Thrown when {@code System.exit} call was called during execution. */
@ExportLibrary(InteropLibrary.class)
public class ExitException extends AbstractTruffleException implements ServiceException {
  private final int exitCode;

  public ExitException(int code, Node location) {
    super("Exit was called with exit code " + code + ".", location);
    this.exitCode = code;
  }

  @ExportMessage
  public boolean isException() {
    return true;
  }

  @ExportMessage
  public int getExceptionExitStatus() {
    return exitCode;
  }

  @ExportMessage
  public ExceptionType getExceptionType() {
    return ExceptionType.EXIT;
  }

  @ExportMessage
  public boolean hasExceptionMessage() {
    return true;
  }

  @ExportMessage
  public String getExceptionMessage() {
    return getMessage();
  }

  @ExportMessage
  public RuntimeException throwException() {
    return this;
  }

  @ExportMessage
  public boolean hasExceptionStackTrace() {
    return true;
  }

  @ExportMessage
  public Object getExceptionStackTrace() {
    var node = this.getLocation();
    var frame = TruffleStackTraceElement.create(node, node.getRootNode().getCallTarget(), null);
    return ArrayLikeHelpers.asVectorWithCheckAt(frame.getGuestObject());
  }
}
