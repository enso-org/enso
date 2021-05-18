package org.enso.interpreter.epb.node;

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
   * Converts a primitive value from a polyglot language executing in the same context as Enso into
   * an Enso primitive.
   *
   * @param value the value to perform conversion on
   * @return the result of converting {@code value} to an Enso primitive where appropriate
   */
  public abstract Object execute(Object value);

  @Specialization(guards = {"numbers.isNumber(l)", "numbers.fitsInLong(l)"})
  long doWrappedInteger(Object l, @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asLong(l);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `l` is checked to be a long");
    }
  }

  @Specialization(
      guards = {"numbers.isNumber(d)", "!numbers.fitsInLong(d)", "numbers.fitsInDouble(d)"})
  double doWrappedDouble(Object d, @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asDouble(d);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `l` is checked to be a long");
    }
  }
}
