package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.callable.MethodResolverNode;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** Simple runtime value representing a yet-unresolved by-name symbol. */
@ExportLibrary(InteropLibrary.class)
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
    this.name = name;
    this.scope = scope;
  }

  /**
   * Gets the symbol name.
   *
   * @return the name of this symbol
   */
  public String getName() {
    return name;
  }

  /** @return the scope this symbol was used in. */
  public ModuleScope getScope() {
    return scope;
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

  /**
   * Marks this object as executable through the interop library.
   *
   * @return always true
   */
  @ExportMessage
  public boolean isExecutable() {
    return true;
  }

  /** Implements the logic of executing {@link UnresolvedSymbol} through the interop library. */
  @ExportMessage
  @ImportStatic(Constants.CacheSizes.class)
  public static class Execute {
    @Specialization
    static Object doDispatch(
        UnresolvedSymbol symbol,
        Object[] arguments,
        @Cached MethodResolverNode methodResolverNode,
        @CachedLibrary(limit = "BUILTIN_INTEROP_DISPATCH") InteropLibrary library)
        throws ArityException, UnsupportedTypeException, UnsupportedMessageException {
      if (arguments.length == 0) throw ArityException.create(1, 0);
      Function function = methodResolverNode.execute(symbol, arguments[0]);
      return library.execute(function, arguments);
    }
  }
}
