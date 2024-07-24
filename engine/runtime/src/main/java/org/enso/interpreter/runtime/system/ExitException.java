package org.enso.interpreter.runtime.system;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
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
final class ExitException extends AbstractTruffleException {
  private final int exitCode;

  ExitException(int code, Node location) {
    super(location);
    this.exitCode = code;
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  int getExceptionExitStatus() {
    return exitCode;
  }

  @ExportMessage
  ExceptionType getExceptionType() {
    return ExceptionType.EXIT;
  }

  @ExportMessage
  boolean hasExceptionMessage() {
    return true;
  }

  @ExportMessage
  String getExceptionMessage() {
    return getMessage();
  }

  @TruffleBoundary
  @Override
  public String getMessage() {
    return "Exit was called with exit code " + exitCode + ".";
  }

  @ExportMessage
  RuntimeException throwException() {
    return this;
  }

  @ExportMessage
  boolean hasExceptionStackTrace() {
    return true;
  }

  @ExportMessage
  Object getExceptionStackTrace() {
    var node = this.getLocation();
    var frame = TruffleStackTraceElement.create(node, node.getRootNode().getCallTarget(), null);
    return ArrayLikeHelpers.asVectorWithCheckAt(frame.getGuestObject());
  }
}
