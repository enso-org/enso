package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.interop.InteropLibrary;

/** A runtime object representing an arbitrary, user-created error. */
@ExportLibrary(InteropLibrary.class)
public class RuntimeError implements TruffleObject {
  private final Object payload;

  /**
   * Creates an instance of this error.
   *
   * @param payload arbitrary, user-provided payload to be carried by this object.
   */
  public RuntimeError(Object payload) {
    this.payload = payload;
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
