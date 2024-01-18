package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;

/**
 * Provides access to fields of an {@link Atom}. This library provides low level (by index) access
 * to fields of any {@link Atom}. Use with care.
 */
@GenerateLibrary
public abstract class StructsLibrary extends Library {
  private static final LibraryFactory<StructsLibrary> FACTORY =
      LibraryFactory.resolve(StructsLibrary.class);

  /** Creates library factory. */
  public static LibraryFactory<StructsLibrary> getFactory() {
    return FACTORY;
  }

  /** Obtains uncached version of this library. */
  public static StructsLibrary getUncached() {
    return FACTORY.getUncached();
  }

  /**
   * Checks whether an object represent a structure/{@link Atom}.
   *
   * @param receiver the object to check
   * @return {@code true} if the {@code receiver} has indexed fields like atoms do
   */
  public boolean isStruct(Object receiver) {
    return false;
  }

  /**
   * Read i-th field.
   *
   * @param receiver the object to work on
   * @param index the index to read
   * @return boxed value at given index
   */
  public abstract Object getField(Object receiver, int index);

  /**
   * Sets i-th field. Use with care - {@link Atom} are supposed to be mostly immutable. Changing a
   * field is an advanced operation used for example by {@code AtomWithHoleNode}.
   */
  public abstract void setField(Object receiver, int index, Object value);
}
