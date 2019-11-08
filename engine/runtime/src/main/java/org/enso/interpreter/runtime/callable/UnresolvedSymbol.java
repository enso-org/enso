package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Simple runtime value representing a yet-unresolved by-name symbol. */
public class UnresolvedSymbol implements TruffleObject {
  private final String name;
  private final ModuleScope scope;

  /**
   * Creates a new unresolved symbol.
   *
   * @param name the name of this symbol
   * @param scope the scope in which this symbol was created
   */
  public UnresolvedSymbol(String name, ModuleScope scope) {
    this.name = name.intern();
    this.scope = scope;
  }

  /**
   * Gets the symbol name.
   *
   * <p>All names for dynamic symbols are interned, making it safe to compare symbol names using the
   * standard {@code ==} equality operator.
   *
   * @return the name of this symbol
   */
  public String getName() {
    return name;
  }

  /**
   * Resolves the symbol for a given constructor.
   *
   * @param cons the constructor for which this symbol should be resolved
   * @return the resolved function definition, or null if not found
   */
  public Function resolveFor(AtomConstructor cons) {
    return scope.lookupMethodDefinitionForAtom(cons, name);
  }

  /**
   * Resolves the symbol for a number.
   *
   * @return the resolved function definition, or null if not found
   */
  @CompilerDirectives.TruffleBoundary
  public Function resolveForNumber() {
    return scope.lookupMethodDefinitionForAny(name).orElse(null);
  }

  /**
   * Resolves the symbol for a function.
   *
   * @return the resolved function definition, or null if not found
   */
  @CompilerDirectives.TruffleBoundary
  public Function resolveForFunction() {
    return scope.lookupMethodDefinitionForAny(name).orElse(null);
  }

  /**
   * Resolves the symbol for an error.
   *
   * @return the resolved function definition, or null if not found
   */
  @CompilerDirectives.TruffleBoundary
  public Function resolveForError() {
    return scope.lookupMethodDefinitionForAny(name).orElse(null);
  }
}
