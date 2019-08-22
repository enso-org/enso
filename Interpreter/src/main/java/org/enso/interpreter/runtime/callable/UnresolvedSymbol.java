package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.interop.TruffleObject;

/** Simple runtime value representing a yet-unresolved by-name symbol. */
public class UnresolvedSymbol implements TruffleObject {
  private final String name;

  /**
   * Creates a new unresolved symbol.
   *
   * @param name the name of this symbol.
   */
  public UnresolvedSymbol(String name) {
    this.name = name.intern();
  }

  /**
   * Gets the symbol name.
   *
   * <p>All names for dynamic symbols are interned, making it safe to compare symbol names using the
   * standard {@code ==} equality operator.
   *
   * @return the name of this symbol.
   */
  public String getName() {
    return name;
  }
}
