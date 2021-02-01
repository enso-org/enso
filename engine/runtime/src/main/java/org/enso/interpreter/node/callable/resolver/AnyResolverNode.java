package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.*;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;

@GenerateUncached
@ReportPolymorphism
public abstract class AnyResolverNode extends BaseResolverNode {

  public abstract Function execute(UnresolvedSymbol symbol, Object _this);

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      Object _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnAny(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolve(
      UnresolvedSymbol symbol, Object _this, @CachedContext(Language.class) Context context) {
    return throwIfNull(context, resolveMethodOnAny(context, symbol), _this, symbol);
  }
}
