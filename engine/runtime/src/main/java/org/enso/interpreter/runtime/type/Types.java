package org.enso.interpreter.runtime.type;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.ImplicitCast;
import com.oracle.truffle.api.dsl.TypeSystem;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.RuntimeError;

import java.util.Optional;

/**
 * This class defines the interpreter-level type system for Enso.
 *
 * <p>While the language has support for rich types, the interpreter only cares about a small set of
 * primitive-level types in order to make execution fast. All higher-level types can be desugared in
 * terms of the more limited set of types expressed here.
 *
 * <p>By declaring the primitive types here, the interpreter obtains automatically generated
 * utilities for working with them.
 */
@TypeSystem({
  long.class,
  String.class,
  Function.class,
  Atom.class,
  AtomConstructor.class,
  Thunk.class,
  RuntimeError.class
})
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

  /**
   * A simple pair type
   *
   * @param <A> the type of the first element
   * @param <B> the type of the second element
   */
  public static class Pair<A, B> {
    private A first;
    private B second;

    private Pair(A first, B second) {
      this.first = first;
      this.second = second;
    }

    /**
     * Gets the first element.
     *
     * @return the first element.
     */
    public A getFirst() {
      return first;
    }

    /**
     * Gets the second element.
     *
     * @return the second element.
     */
    public B getSecond() {
      return second;
    }
  }

  /**
   * Asserts that the arguments array is empty.
   *
   * @param arguments the arguments array.
   * @throws ArityException if the array is not empty.
   */
  public static void extractArguments(Object[] arguments) throws ArityException {
    if (arguments.length != 0) {
      throw ArityException.create(0, arguments.length);
    }
  }

  /**
   * Return a type of the given object as a string.
   *
   * @param value an object of interest.
   * @return the string representation of object's type.
   */
  public static Optional<String> getName(Object value) {
    if (TypesGen.isLong(value)) {
      return Optional.of("Number");
    } else if (TypesGen.isString(value)) {
      return Optional.of("Text");
    } else if (TypesGen.isFunction(value)) {
      return Optional.of("Function");
    } else if (TypesGen.isAtom(value)) {
      return Optional.of(TypesGen.asAtom(value).getConstructor().getName());
    } else if (TypesGen.isAtomConstructor(value)) {
      return Optional.of(TypesGen.asAtomConstructor(value).getName());
    } else if (TypesGen.isThunk(value)) {
      return Optional.of("Thunk");
    } else if (TypesGen.isRuntimeError(value)) {
      return Optional
        .of("Error " + TypesGen.asRuntimeError(value).getPayload().toString());
    } else {
      return Optional.empty();
    }
  }

  /**
   * Asserts that the arguments array has exactly one element of a given type and extracts it.
   *
   * @param arguments the arguments array
   * @param cls the class of the only element
   * @param <A> the type of the only element
   * @return the only element of the array
   * @throws ArityException if the array does not have exactly one element
   * @throws UnsupportedTypeException if the only element is not an instance of {@code cls}
   */
  @SuppressWarnings("unchecked")
  public static <A> A extractArguments(Object[] arguments, Class<A> cls)
      throws ArityException, UnsupportedTypeException {
    if (arguments.length != 1) {
      throw ArityException.create(1, arguments.length);
    }
    if (!(cls.isInstance(arguments[0]))) {
      throw UnsupportedTypeException.create(
          arguments, "The argument must be a " + cls.getSimpleName() + ".");
    }
    return (A) arguments[0];
  }

  /**
   * Asserts that the arguments array has exactly two elements of given types and extracts them.
   *
   * @param arguments the arguments array
   * @param cls1 the class of the first element
   * @param cls2 the class of the second element
   * @param <A> the type of the first element
   * @param <B> the type of the second element
   * @return the pair of elements of the array
   * @throws ArityException if the array does not have exactly two elements
   * @throws UnsupportedTypeException if the elements are not instances of the proper classes
   */
  @SuppressWarnings("unchecked")
  public static <A, B> Pair<A, B> extractArguments(Object[] arguments, Class<A> cls1, Class<B> cls2)
      throws ArityException, UnsupportedTypeException {
    if (arguments.length != 2) {
      throw ArityException.create(2, arguments.length);
    }
    if (!(cls1.isInstance(arguments[0]))) {
      throw UnsupportedTypeException.create(
          arguments, "The first argument must be a " + cls1.getSimpleName() + ".");
    }
    if (!(cls2.isInstance(arguments[1]))) {
      throw UnsupportedTypeException.create(
          arguments, "The second argument must be a " + cls2.getSimpleName() + ".");
    }
    return new Pair<>((A) arguments[0], (B) arguments[1]);
  }
}
