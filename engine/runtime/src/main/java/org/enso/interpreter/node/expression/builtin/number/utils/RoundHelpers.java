package org.enso.interpreter.node.expression.builtin.number.utils;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.error.PanicException;

public class RoundHelpers {
  /** Minimum value for the `decimal_places` parameter to `roundDouble`. */
  public static final double ROUND_MIN_DECIMAL_PLACES = -15;

  /** Maximum value for the `decimal_places` parameter to `roundDouble`. */
  public static final double ROUND_MAX_DECIMAL_PLACES = 15;

  /** Minimum value for the `n` parameter to `roundDouble`. */
  public static final double ROUND_MIN_DOUBLE = -99999999999999.0;

  /** Minimum value for the `n` parameter to `roundDouble`. */
  public static final double ROUND_MAX_DOUBLE = 99999999999999.0;

  public static final double ROUND_MIN_LONG = -99999999999999L;

  /** Minimum value for the `n` parameter to `roundDouble`. */
  public static final double ROUND_MAX_LONG = 99999999999999L;

  @TruffleBoundary
  public static void decimalPlacesOutOfRangePanic(Node node, long decimalPlaces)
      throws PanicException {
    String msg =
        "round: decimalPlaces must be between "
            + ROUND_MIN_DECIMAL_PLACES
            + " and "
            + ROUND_MAX_DECIMAL_PLACES
            + " (inclusive), but was "
            + decimalPlaces;
    Builtins builtins = EnsoContext.get(node).getBuiltins();
    throw new PanicException(
        builtins.error().makeUnsupportedArgumentsError(new Object[] {decimalPlaces}, msg), node);
  }

  @TruffleBoundary
  public static void argumentOutOfRangePanic(Node node, double n) throws PanicException {
    String msg =
        "Error: `round` can only accept values between "
            + ROUND_MIN_DOUBLE
            + " and "
            + ROUND_MAX_DOUBLE
            + " (inclusive), but was "
            + n;
    Builtins builtins = EnsoContext.get(node).getBuiltins();
    throw new PanicException(
        builtins.error().makeUnsupportedArgumentsError(new Object[] {n}, msg), node);
  }

  @TruffleBoundary
  public static void specialValuePanic(Node node, double n) throws PanicException {
    String msg = "Error: `round` cannot accept " + (Double.isNaN(n) ? "NaN" : "Inf") + " values ";
    Builtins builtins = EnsoContext.get(node).getBuiltins();
    throw new PanicException(
        builtins.error().makeUnsupportedArgumentsError(new Object[] {n}, msg), node);
  }
}
