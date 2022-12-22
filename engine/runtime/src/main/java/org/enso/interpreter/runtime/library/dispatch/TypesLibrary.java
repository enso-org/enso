package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;
import org.enso.interpreter.runtime.data.Type;

/**
 * A library used for equipping data structures with Enso-style (fully unapplied) method dispatch.
 * This is used for all method calls in the language.
 */
@GenerateLibrary
@GenerateLibrary.DefaultExport(DefaultLongExports.class)
@GenerateLibrary.DefaultExport(DefaultDoubleExports.class)
@GenerateLibrary.DefaultExport(DefaultBooleanExports.class)
public abstract class TypesLibrary extends Library {

  /**
   * Returns a resolved library factory for this library.
   *
   * @return a library factory instance
   */
  public static LibraryFactory<TypesLibrary> getFactory() {
    return FACTORY;
  }

  /**
   * Returns the uncached instance of this library.
   *
   * @return the uncached instance of this library
   */
  public static TypesLibrary getUncached() {
    return FACTORY.getUncached();
  }

  private static final LibraryFactory<TypesLibrary> FACTORY =
      LibraryFactory.resolve(TypesLibrary.class);

  /**
   * Checks if the receiver supports Enso-style method dispatch.
   *
   * @param receiver the receiver to check
   * @return whether the receiver supports method dispatch through this library
   */
  @GenerateLibrary.Abstract(ifExported = {"getType"})
  public boolean hasType(Object receiver) {
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
   * Returns the receiver Type.
   *
   * @param receiver the typed object
   * @return the corresponding type
   */
  @GenerateLibrary.Abstract(ifExported = {"hasType"})
  public Type getType(Object receiver) {
    return null;
  }
}
