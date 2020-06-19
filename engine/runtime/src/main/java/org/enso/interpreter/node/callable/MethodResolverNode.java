package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.MethodDoesNotExistException;
import org.enso.interpreter.runtime.error.RuntimeError;

/**
 * A node performing lookups of method definitions.
 *
 * <p>Uses a polymorphic inline cache to ensure the best performance.
 *
 * <p>The dispatch algorithm works by matching the kind of value the method is requested for and
 * delegating to the proper lookup method of {@link UnresolvedSymbol}.
 */
@NodeInfo(shortName = "MethodResolver", description = "Resolves method calls to concrete targets")
@GenerateUncached
public abstract class MethodResolverNode extends Node {

  MethodResolverNode() {}

  /**
   * Creates an instance of this node.
   *
   * @return a method resolver node
   */
  public static MethodResolverNode build() {
    return MethodResolverNodeGen.create();
  }

  /**
   * Entry point for this node.
   *
   * @param symbol Method name to resolve.
   * @param self Object for which to resolve the method.
   * @return Resolved method.
   */
  public abstract Function execute(UnresolvedSymbol symbol, Object self);

  @Specialization(guards = "isValidAtomCache(symbol, cachedSymbol, atom, cachedConstructor)")
  Function resolveAtomCached(
      UnresolvedSymbol symbol,
      Atom atom,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @Cached(value = "atom.getConstructor()", allowUncached = true)
          AtomConstructor cachedConstructor,
      @Cached(value = "resolveMethodOnAtom(cachedConstructor, cachedSymbol)", allowUncached = true)
          Function function) {
    return function;
  }

  @Specialization(guards = {"cachedSymbol == symbol", "atomConstructor == cachedConstructor"})
  Function resolveAtomConstructorCached(
      UnresolvedSymbol symbol,
      AtomConstructor atomConstructor,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @Cached(value = "atomConstructor", allowUncached = true) AtomConstructor cachedConstructor,
      @Cached(value = "resolveMethodOnAtom(cachedConstructor, cachedSymbol)", allowUncached = true)
          Function function) {
    return function;
  }

  @Specialization(guards = "cachedSymbol == symbol")
  Function resolveNumberCached(
      UnresolvedSymbol symbol,
      long self,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @Cached(value = "resolveMethodOnNumber(cachedSymbol)", allowUncached = true)
          Function function) {
    return function;
  }

  @Specialization(guards = "cachedSymbol == symbol")
  Function resolveStringCached(
      UnresolvedSymbol symbol,
      String self,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @Cached(value = "resolveMethodOnString(cachedSymbol)", allowUncached = true)
          Function function) {
    return function;
  }

  @Specialization(guards = "cachedSymbol == symbol")
  Function resolveFunctionCached(
      UnresolvedSymbol symbol,
      Function self,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @Cached(value = "resolveMethodOnFunction(cachedSymbol)", allowUncached = true)
          Function function) {
    return function;
  }

  @Specialization(guards = "cachedSymbol == symbol")
  Function resolveErrorCached(
      UnresolvedSymbol symbol,
      RuntimeError self,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @Cached(value = "resolveMethodOnError(cachedSymbol)", allowUncached = true)
          Function function) {
    return function;
  }

  @Specialization(guards = {"cachedSymbol == symbol", "ctx.getEnvironment().isHostObject(target)"})
  Function resolveHostCached(
      UnresolvedSymbol symbol,
      Object target,
      @Cached(value = "symbol", allowUncached = true) UnresolvedSymbol cachedSymbol,
      @CachedContext(Language.class) Context ctx,
      @Cached(value = "buildHostResolver(cachedSymbol, ctx)", allowUncached = true)
          Function function) {
    return function;
  }

  private Function ensureMethodExists(Function function, Object target, UnresolvedSymbol symbol) {
    if (function == null) {
      throw new MethodDoesNotExistException(target, symbol.getName(), this);
    }
    return function;
  }

  Function resolveMethodOnAtom(AtomConstructor cons, UnresolvedSymbol symbol) {
    return ensureMethodExists(symbol.resolveFor(cons, getBuiltins().any()), cons, symbol);
  }

  Function resolveMethodOnNumber(UnresolvedSymbol symbol) {
    return ensureMethodExists(
        symbol.resolveFor(getBuiltins().number(), getBuiltins().any()), "Number", symbol);
  }

  Function resolveMethodOnString(UnresolvedSymbol symbol) {
    return ensureMethodExists(
        symbol.resolveFor(getBuiltins().text(), getBuiltins().any()), "Text", symbol);
  }

  Function resolveMethodOnFunction(UnresolvedSymbol symbol) {
    return ensureMethodExists(
        symbol.resolveFor(getBuiltins().function(), getBuiltins().any()), "Function", symbol);
  }

  Function resolveMethodOnError(UnresolvedSymbol symbol) {
    return ensureMethodExists(symbol.resolveFor(getBuiltins().any()), "Error", symbol);
  }

  Function buildHostResolver(UnresolvedSymbol symbol, Context context) {
    if (symbol.getName().equals("new")) {
      return context.getBuiltins().getConstructorDispatch();
    } else {
      return context.getBuiltins().buildPolyglotMethodDispatch(symbol.getName());
    }
  }

  static boolean isValidAtomCache(
      UnresolvedSymbol symbol,
      UnresolvedSymbol cachedSymbol,
      Atom atom,
      AtomConstructor cachedConstructor) {
    return (symbol == cachedSymbol) && (atom.getConstructor() == cachedConstructor);
  }

  private Builtins getBuiltins() {
    return lookupContextReference(Language.class).get().getBuiltins();
  }
}
