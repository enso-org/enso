package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

/**
 * A runtime object representing an arbitrary, user-created dataflow error.
 *
 * <p>Dataflow errors in Enso are those that are standard values, and flow through the graph until
 * they are handled. Another term used to describe these errors is "broken values".
 */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
public class DataflowError extends AbstractTruffleException {
  private final Object payload;

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
    DataflowError result = new DataflowError(payload, location);
    TruffleStackTrace.fillIn(result);
    return result;
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
    return new DataflowError(payload, prototype);
  }

  DataflowError(Object payload, Node location) {
    super(location);
    this.payload = payload;
  }

  DataflowError(Object payload, AbstractTruffleException prototype) {
    super(prototype);
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

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  @ExportMessage
  boolean hasSpecialConversion() {
    return true;
  }

  @ExportMessage
  static class GetConversionFunction {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor target, UnresolvedConversion conversion) {
      Context context = getContext();
      return conversion.resolveFor(target, context.getBuiltins().dataflowError().constructor());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
            guards = {
                    "!getContext().isInlineCachingDisabled()",
                    "cachedTarget == target",
                    "cachedConversion == conversion",
                    "function != null"
            },
            limit = "CACHE_SIZE")
    static Function resolveCached(
            DataflowError _this,
            AtomConstructor target,
            UnresolvedConversion conversion,
            @Cached("conversion") UnresolvedConversion cachedConversion,
            @Cached("target") AtomConstructor cachedTarget,
            @Cached("doResolve(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
            DataflowError _this,
            AtomConstructor target,
            UnresolvedConversion conversion)
            throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
