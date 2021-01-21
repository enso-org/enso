package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.TruffleException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.Node;
import javax.xml.crypto.Data;

/**
 * A runtime object representing an arbitrary, user-created dataflow error.
 *
 * <p>Dataflow errors in Enso are those that are standard values, and flow through the graph until
 * they are handled. Another term used to describe these errors is "broken values".
 */
@ExportLibrary(InteropLibrary.class)
public class DataflowError extends RuntimeException implements TruffleObject, TruffleException {
  private final Object payload;
  private final Node location;

  /** Construct a new dataflow error with the default stack trace.
   *
   * The default stack trace has the throwing location as the top element of the stack trace.
   *
   * @param payload the user-provided value carried by the error
   * @param location the node in which the error was created
   * @return a new dataflow error
   */
  public static DataflowError withDefaultTrace(Object payload, Node location) {
    var error = new DataflowError(payload, location);
    error.fillInStackTrace();
    return error;
  }

  /** Construct a new dataflow error with the provided stack trace.
   *
   * This is useful for when the dataflow error is created from the recovery of a panic, and we want
   * to point to the original location of the panic.
   *
   * @param payload the user-provided value carried by the error
   * @param location the node in which the error was located
   * @param trace a specific stack trace
   * @return a new dataflow error
   */
  public static DataflowError withTrace(Object payload, Node location, StackTraceElement[] trace) {
    var error = new DataflowError(payload, location);
    error.setStackTrace(trace);
    return error;
  }

  DataflowError(Object payload, Node location) {
    this.payload = payload;
    this.location = location;
  }

  @Override
  public Node getLocation() {
    return location;
  }

  /**
   * Returns the user provided payload carried by this object.
   *
   * @return the payload object
   */
  public Object getPayload() {
    return payload;
  }

  /**
   * Represents this object as a string.
   *
   * @return a string representation of this object
   */
  @Override
  public String toString() {
    return "Error:" + getPayload().toString();
  }

  @ExportMessage
  public String toDisplayString(
      boolean allowSideEffects,
      @CachedLibrary(limit = "3") InteropLibrary displays,
      @CachedLibrary(limit = "3") InteropLibrary strings) {
    try {
      return "(Error: " + strings.asString(displays.toDisplayString(payload)) + ")";
    } catch (UnsupportedMessageException e) {
      return "Error";
    }
  }
}
