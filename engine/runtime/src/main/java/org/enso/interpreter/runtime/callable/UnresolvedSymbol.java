package org.enso.interpreter.runtime.callable;

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
  private UnresolvedSymbol(String name, ModuleScope scope) {
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
   * Resolves the symbol for a given hierarchy of constructors.
   *
   * <p>The constructors are checked in the first to last order, and the first match for this symbol
   * is returned. This is useful for certain subtyping relations, such as "any constructor is a
   * subtype of Any" or "Nat is a subtype of Int, is a subtype of Number".
   *
   * @param constructors the constructors hierarchy for which this symbol should be resolved
   * @return the resolved function definition, or null if not found
   */
  public Function resolveFor(AtomConstructor... constructors) {
    for (AtomConstructor constructor : constructors) {
      Function candidate = scope.lookupMethodDefinition(constructor, name);
      if (candidate != null) {
        return candidate;
      }
    }
    return null;
  }

  @Override
  public String toString() {
    return "UnresolvedSymbol<" + this.name + ">";
  }

  /**
   * Creates an instance of this node.
   *
   * @param name the name that is unresolved
   * @param scope the scope in which the lookup will occur
   * @return a node representing an unresolved symbol {@code name} in {@code scope}
   */
  public static UnresolvedSymbol build(String name, ModuleScope scope) {
    return new UnresolvedSymbol(name, scope);
  }
}
