package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.ExceptionType;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

/**
 * A sentinel value used to trace the propagation of panics through the program.
 *
 * <p>This tracing is enabled by the active intervention of the runtime instrumentation, and does
 * not function in textual mode.
 */
@ExportLibrary(value = InteropLibrary.class, delegateTo = "panic")
@ExportLibrary(MethodDispatchLibrary.class)
public class PanicSentinel extends AbstractTruffleException {
  final PanicException panic;

  /**
   * Create an instance of the panic sentinel, wrapping the provided panic.
   *
   * @param panic the panic to wrap
   * @param location the location from where the sentinel was thrown
   */
  public PanicSentinel(PanicException panic, Node location) {
    super(location);
    this.panic = panic;
  }

  /**
   * Get the underlying panic.
   *
   * @return the underlying panic object
   */
  public PanicException getPanic() {
    return panic;
  }

  @Override
  public String getMessage() {
    return panic.getMessage();
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
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
