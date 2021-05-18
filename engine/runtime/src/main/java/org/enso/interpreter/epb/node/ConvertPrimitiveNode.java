package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;

@GenerateUncached
@ReportPolymorphism
public abstract class ConvertPrimitiveNode extends Node {

  /**
   * Build a new convert primitive node.
   *
   * @return a new convert primitive node.
   */
  public static ConvertPrimitiveNode build() {
    return ConvertPrimitiveNodeGen.create();
  }

  /**
   * Converts a primitive value from a polyglot language executing in the same context as Enso into
   * an Enso primitive.
   *
   * @param value the value to perform conversion on
   * @return the result of converting {@code value} to an Enso primitive where appropriate
   */
  public abstract Object execute(Object value);

  @Specialization(guards = "booleans.isBoolean(b)")
  boolean doForeignBoolean(Object b, @CachedLibrary(limit = "5") InteropLibrary booleans) {
    try {
      return booleans.asBoolean(b);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `b` has been checked to be a boolean");
    }
  }

  @Specialization(guards = {"numbers.isNumber(l)", "numbers.fitsInLong(l)"})
  long doForeignInteger(Object l, @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asLong(l);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `l` is checked to be a long");
    }
  }

  @Specialization(
      guards = {"numbers.isNumber(d)", "!numbers.fitsInLong(d)", "numbers.fitsInDouble(d)"})
  double doForeignDecimal(Object d, @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asDouble(d);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `l` is checked to be a long");
    }
  }

  @Fallback
  Object doOther(Object o) {
    return o;
  }
}
