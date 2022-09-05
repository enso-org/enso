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
public abstract class CoercePrimitiveNode extends Node {

  /**
   * Create a new node responsible for coercing primitive values to Enso primitives at the polyglot
   * boundary.
   *
   * @return a new primitive coercion node
   */
  public static CoercePrimitiveNode build() {
    return CoercePrimitiveNodeGen.create();
  }

  /**
   * Converts an object from a polyglot representation into an equivalent representation as an Enso
   * primitive.
   *
   * @param value the polyglot value to perform coercion on
   * @return {@code value} coerced to an Enso primitive where applicable
   */
  public abstract Object execute(Object value);

  @Specialization(guards = "bools.isBoolean(bool)")
  boolean doBoolean(Object bool, @CachedLibrary(limit = "5") InteropLibrary bools) {
    try {
      return bools.asBoolean(bool);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `bool` already checked to be a boolean");
    }
  }

  @Specialization(guards = {"numbers.isNumber(integer)", "numbers.fitsInLong(integer)"})
  long doInteger(Object integer, @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asLong(integer);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `integer` is checked to be a long");
    }
  }

  @Specialization(
      guards = {
        "numbers.isNumber(decimal)",
        "!numbers.fitsInLong(decimal)",
        "numbers.fitsInDouble(decimal)"
      })
  double doDecimal(Object decimal, @CachedLibrary(limit = "5") InteropLibrary numbers) {
    try {
      return numbers.asDouble(decimal);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `decimal` is checked to be a long");
    }
  }

  @Specialization(guards = {"characters.isString(character)", "isChar(character)"})
  long doChar(Object character, @CachedLibrary(limit = "5") InteropLibrary characters) {
    try {
      return characters.asString(character).charAt(0);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `character` is checked to be a long");
    }
  }

  static boolean isChar(Object s) {
    return s instanceof Character;
  }

  @Fallback
  Object doNonPrimitive(Object value) {
    return value;
  }
}
