package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

/**
 * A sentinel value used to trace the propagation of panics through the program.
 *
 * <p>This tracing is enabled by the active intervention of the runtime instrumentation, and does
 * not function in textual mode.
 */
@ExportLibrary(MethodDispatchLibrary.class)
public class PanicSentinel extends RuntimeException implements TruffleException {
  private final PanicException panic;
  private final Node location;

  /**
   * Create an instance of the panic sentinel, wrapping the provided panic.
   *
   * @param panic the panic to wrap
   * @param location the location from where the sentinel was thrown
   */
  public PanicSentinel(PanicException panic, Node location) {
    super(panic.getExceptionObject().toString());
    this.panic = panic;
    this.location = location;
  }

  /**
   * Returns the location where this exception was thrown.
   *
   * @return the original throw location
   */
  @Override
  public Node getLocation() {
    return location;
  }

  /**
   * Returns the payload carried by this exception.
   *
   * @return the payload object
   */
  @Override
  public Object getExceptionObject() {
    return panic;
  }

  /**
   * Override recommended by the Truffle documentation for better performance.
   *
   * @see <a
   *     href="https://www.graalvm.org/truffle/javadoc/com/oracle/truffle/api/TruffleException.html">Relevant
   *     documentation</a>
   * @return this exception
   */
  @Override
  public Throwable fillInStackTrace() {
    return this;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }
}
