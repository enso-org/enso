package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.TruffleStackTraceElement;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.util.Objects;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * A runtime object representing an arbitrary, user-created dataflow error.
 *
 * <p>Dataflow errors in Enso are those that are standard values, and flow through the graph until
 * they are handled. Another term used to describe these errors is "broken values".
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class DataflowError extends AbstractTruffleException {
  /** Signals (local) values that haven't yet been initialized */
  public static final DataflowError UNINITIALIZED = new DataflowError(null, (Node) null);

  private static final boolean assertsOn;

  static {
    var b = false;
    assert b = true;
    assertsOn = b;
  }

  private final Object payload;
  private final boolean ownTrace;

  /**
   * Construct a new dataflow error with the default stack trace.
   *
   * <p>The default stack trace has the throwing location as the top element of the stack trace.
   *
   * @param payload the user-provided value carried by the error
   * @param location the node in which the error was created
   * @return a new dataflow error
   */
  public static DataflowError withoutTrace(Object payload, Node location) {
    assert payload != null;
    if (assertsOn) {
      var result = new DataflowError(payload, UNLIMITED_STACK_TRACE, location);
      TruffleStackTrace.fillIn(result);
      return result;
    } else {
      var result = new DataflowError(payload, location);
      return result;
    }
  }

  /**
   * Construct a new dataflow error with the provided stack trace.
   *
   * <p>This is useful for when the dataflow error is created from the recovery of a panic, and we
   * want to point to the original location of the panic.
   *
   * @param payload the user-provided value carried by the error
   * @param prototype the exception to derive the stacktrace from
   * @return a new dataflow error
   */
  public static DataflowError withTrace(Object payload, AbstractTruffleException prototype) {
    assert payload != null;
    var result = new DataflowError(payload, prototype);
    TruffleStackTrace.fillIn(result);
    return result;
  }

  private DataflowError(Object payload, Node location) {
    super(null, null, 1, location);
    this.payload = payload;
    this.ownTrace = location != null && location.getRootNode() != null;
  }

  private DataflowError(Object payload, AbstractTruffleException prototype) {
    super(prototype);
    this.payload = payload;
    this.ownTrace = false;
  }

  private DataflowError(Object payload, int stackTraceElementLimit, Node location) {
    super(null, null, stackTraceElementLimit, location);
    this.ownTrace = false;
    this.payload = payload;
  }

  /**
   * Returns the user provided payload carried by this object.
   *
   * @return the payload object
   */
  public Object getPayload() {
    return payload != null ? payload : "Uninitialized value";
  }

  /**
   * Represents this object as a string.
   *
   * @return a string representation of this object
   */
  @Override
  @TruffleBoundary
  public String toString() {
    return "Error:" + Objects.toString(getPayload());
  }

  @ExportMessage
  @TruffleBoundary
  public String toDisplayString(boolean allowSideEffects) {
    try {
      var iop = InteropLibrary.getUncached();
      return "(Error: " + iop.asString(iop.toDisplayString(getPayload())) + ")";
    } catch (UnsupportedMessageException e) {
      return "Error";
    }
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().dataflowError();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  boolean hasExceptionStackTrace() {
    return ownTrace;
  }

  @ExportMessage
  Object getExceptionStackTrace() throws UnsupportedMessageException {
    if (!ownTrace) {
      throw UnsupportedMessageException.create();
    }
    var node = this.getLocation();
    var frame = TruffleStackTraceElement.create(node, node.getRootNode().getCallTarget(), null);
    return ArrayLikeHelpers.asVectorWithCheckAt(frame.getGuestObject());
  }

  @ExportMessage
  boolean isNull() {
    return payload == null;
  }

  @ExportMessage
  RuntimeException throwException() throws UnsupportedMessageException {
    return this;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().dataflowError();
  }
}
