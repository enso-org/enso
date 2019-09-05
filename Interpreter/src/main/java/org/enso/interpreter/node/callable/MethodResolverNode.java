package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.MethodDoesNotExistException;

/**
 * A node performing lookups of method definitions. Uses a polymorphic inline cache to ensure the
 * best performance.
 */
public abstract class MethodResolverNode extends Node {

  /**
   * DSL method to generate the actual cached code. Uses Cached arguments and performs all the logic
   * through the DSL. Not for manual use.
   */
  @Specialization(guards = "isValidCache(symbol, cachedSymbol, atom, cachedConstructor)")
  public Function resolveCached(
      UnresolvedSymbol symbol,
      Atom atom,
      @CachedContext(Language.class) TruffleLanguage.ContextReference<Context> contextRef,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("atom.getConstructor()") AtomConstructor cachedConstructor,
      @Cached("resolveMethod(cachedConstructor, cachedSymbol)") Function function) {
    return function;
  }

  /**
   * Entry point for this node.
   *
   * @param symbol Method name to resolve.
   * @param atom Object for which to resolve the method.
   * @return Resolved method.
   */
  public abstract Function execute(UnresolvedSymbol symbol, Atom atom);

  /**
   * Handles the actual method lookup. Not for manual use.
   *
   * @param cons Type for which to resolve the method.
   * @param symbol symbol representing the method to resolve
   * @return Resolved method definition.
   */
  public Function resolveMethod(
      AtomConstructor cons,
      UnresolvedSymbol symbol) {
    Function result = symbol.resolveFor(cons);
    if (result == null) {
      throw new MethodDoesNotExistException(cons, symbol.getName(), this);
    }
    return result;
  }

  /**
   * Checks the cache validity. For use by the DSL. The cache entry is valid if it's resolved for
   * the same method name and this argument type. Not for manual use.
   */
  public boolean isValidCache(
      UnresolvedSymbol symbol, UnresolvedSymbol cachedSymbol, Atom atom, AtomConstructor cachedConstructor) {
    return (symbol == cachedSymbol) && (atom.getConstructor() == cachedConstructor);
  }
}
