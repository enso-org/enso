package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.*;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;

@GenerateUncached
@ReportPolymorphism
public abstract class AnyResolverNode extends BaseResolverNode {

  public abstract Function execute(UnresolvedSymbol symbol, Object _this);

  @Specialization(
      guards = {
        "!getContext().isInlineCachingDisabled()",
        "cachedSymbol == symbol",
        "function != null"
      },
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      Object _this,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnAny(cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolve(UnresolvedSymbol symbol, Object _this) {
    return throwIfNull(resolveMethodOnAny(symbol), _this, symbol);
  }
}
