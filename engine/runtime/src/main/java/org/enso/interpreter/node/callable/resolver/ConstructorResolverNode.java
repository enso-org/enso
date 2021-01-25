package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.*;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

@GenerateUncached
@ReportPolymorphism
public abstract class ConstructorResolverNode extends BaseResolverNode {
  public abstract Function execute(UnresolvedSymbol symbol, AtomConstructor self);

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "_this == cachedConstructor",
        "function != null"
      },
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      AtomConstructor _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("_this") AtomConstructor cachedConstructor,
      @Cached("resolveMethodOnAtom(context, cachedConstructor, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolve(
      UnresolvedSymbol symbol,
      AtomConstructor _this,
      @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnAtom(context, _this, symbol);
    return throwIfNull(context, function, _this, symbol);
  }
}
