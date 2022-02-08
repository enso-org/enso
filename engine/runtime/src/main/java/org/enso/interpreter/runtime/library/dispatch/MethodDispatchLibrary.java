package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * A library used for equipping data structures with Enso-style (fully unapplied) method dispatch.
 * This is used for all method calls in the language.
 */
@GenerateLibrary
@GenerateLibrary.DefaultExport(DefaultLongExports.class)
@GenerateLibrary.DefaultExport(DefaultDoubleExports.class)
@GenerateLibrary.DefaultExport(DefaultBooleanExports.class)
public abstract class MethodDispatchLibrary extends Library {

  /**
   * Returns a resolved library factory for this library.
   *
   * @return a library factory instance
   */
  public static LibraryFactory<MethodDispatchLibrary> getFactory() {
    return FACTORY;
  }

  /**
   * Returns the uncached instance of this library.
   *
   * @return the uncached instance of this library
   */
  public static MethodDispatchLibrary getUncached() {
    return FACTORY.getUncached();
  }

  /** An exception thrown when the library cannot lookup the method definition. */
  public static class NoSuchMethodException extends Exception {}

  private static final LibraryFactory<MethodDispatchLibrary> FACTORY =
      LibraryFactory.resolve(MethodDispatchLibrary.class);

  /**
   * Checks if the receiver supports Enso-style method dispatch
   *
   * @param receiver the receiver to check
   * @return whether the receiver supports method dispatch through this library
   */
  @GenerateLibrary.Abstract(ifExported = {"getFunctionalDispatch"})
  public boolean hasFunctionalDispatch(Object receiver) {
    return false;
  }

  /**
   * Denotes an object that has special dispatch semantics, that must be handled specially in the
   * method dispatch logic.
   *
   * @param receiver the receiver to check
   * @return whether the receiver has special dispatch semantics
   */
  public boolean hasSpecialDispatch(Object receiver) {
    return false;
  }

  /**
   * Looks up the method definition for the given receiver and symbol
   *
   * @param receiver the method call receiver
   * @param symbol the symbol being dispatched
   * @return the corresponding function definition
   * @throws NoSuchMethodException if the function definition could not be found
   */
  @GenerateLibrary.Abstract(ifExported = {"hasFunctionalDispatch"})
  public Function getFunctionalDispatch(Object receiver, UnresolvedSymbol symbol)
      throws NoSuchMethodException {
    throw new NoSuchMethodException();
  }

  /* * Conversions */

  /** An exception thrown when the library cannot lookup the conversion definition. */
  public static class NoSuchConversionException extends Exception {}

  //@GenerateLibrary.Abstract(ifExported = {"getConversionFunction"})
  public boolean canConvertFrom(Object receiver) {
    return false;
  }

  public boolean hasSpecialConversion(Object receiver) {
    return false;
  }

  @GenerateLibrary.Abstract(ifExported = {"canConvertFrom"})
  public Function getConversionFunction(
      Object receiver, AtomConstructor target, UnresolvedConversion symbol)
      throws MethodDispatchLibrary.NoSuchConversionException {
    throw new MethodDispatchLibrary.NoSuchConversionException();
  }
}
