package org.enso.interpreter.runtime.type;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;
import org.enso.interpreter.runtime.callable.Callable;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * This class defines the interpreter-level type system for Enso.
 *
 * <p>While the language has support for rich types, the interpreter only cares about a small set of
 * primitive-level types in order to make execution fast. All higher-level types can be desugared in
 * terms of the more limited set of types expressed here.
 *
 * By declaring the primitive types here, the interpreter obtains automatically generated utilities
 * for working with them.
 */
@TypeSystem({long.class, Function.class, Atom.class, AtomConstructor.class, Callable.class})
public class Types {

  /**
   * An implicit conversion between {@code int} and {@code long} for Enso programs.
   *
   * @param value the value to convert
   * @return {@code value} as the appropriate type
   */
  @ImplicitCast
  @CompilerDirectives.TruffleBoundary
  public static long castLong(int value) {
    return value;
  }
}
