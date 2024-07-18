package org.enso.interpreter.runtime.warning;

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
   * Returns all unique warnings associated with the receiver.
   *
   * @param receiver the receiver to analyze
   * @param location optional parameter specifying the node to which the warnings should be
   *     reassigned to
   * @param shouldWrap if true, warnings attached to elements in array-likes are wrapped in
   *     Map_Error
   * @return the associated warnings
   */
  public Warning[] getWarnings(Object receiver, Node location, boolean shouldWrap)
      throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  /**
   * Returns the object with all warnings removed.
   *
   * @param receiver the receiver to analyze
   * @return the receiver with all warnings removed, if any
   */
  public Object removeWarnings(Object receiver) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  /**
   * Checks if the receiver reached a maximal number of warnings that could be reported.
   *
   * @param receiver the receiver to analyze
   * @return whether the receiver reached a maximal number of warnings
   */
  public boolean isLimitReached(Object receiver) {
    return false;
  }
}
