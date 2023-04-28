package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.GenerateLibrary;
import com.oracle.truffle.api.library.Library;
import com.oracle.truffle.api.library.LibraryFactory;
import com.oracle.truffle.api.nodes.Node;

@GenerateLibrary
public abstract class WarningsLibrary extends Library {

  /**
   * Returns the uncached instance of this library.
   *
   * @return the uncached instance of this library
   */
  public static WarningsLibrary getUncached() {
    return FACTORY.getUncached();
  }

  private static final LibraryFactory<WarningsLibrary> FACTORY =
      LibraryFactory.resolve(WarningsLibrary.class);

  /**
   * Returns a resolved library factory for this library.
   *
   * @return a library factory instance
   */
  public static LibraryFactory<WarningsLibrary> getFactory() {
    return FACTORY;
  }

  /**
   * Checks if the receiver has any warnings.
   *
   * @param receiver the receiver to check
   * @return whether the receiver has any warnings associated with it
   */
  @GenerateLibrary.Abstract(ifExported = {"getWarnings"})
  public boolean hasWarnings(Object receiver) {
    return false;
  }

  /**
   * Returns all unique warnings associated with the receiver.
   *
   * @param receiver the receiver to analyze
   * @param location optional parameter specifying the node to which the warnings should be
   *     reassigned to
   * @return the associated warnings
   */
  @GenerateLibrary.Abstract(ifExported = {"hasWarnings"})
  public Warning[] getWarnings(Object receiver, Node location) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  /**
   * Returns the object with all warnings removed.
   *
   * @param receiver the receiver to analyze
   * @return the receiver with all warnings removed, if any
   */
  @GenerateLibrary.Abstract(ifExported = {"hasWarnings"})
  public Object removeWarnings(Object receiver) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }
}
