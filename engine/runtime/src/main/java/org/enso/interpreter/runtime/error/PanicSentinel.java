package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * A sentinel value used to trace the propagation of panics through the program.
 *
 * <p>This tracing is enabled by the active intervention of the runtime instrumentation, and does
 * not function in textual mode.
 */
@ExportLibrary(TypesLibrary.class)
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
}
