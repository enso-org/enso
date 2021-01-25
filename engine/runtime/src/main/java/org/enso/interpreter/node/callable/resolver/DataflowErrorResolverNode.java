package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;

@GenerateUncached
@ReportPolymorphism
public abstract class DataflowErrorResolverNode extends BaseResolverNode {

  public abstract Function execute(UnresolvedSymbol symbol, DataflowError _this);

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol"},
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      DataflowError _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnError(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolve(
      UnresolvedSymbol symbol, DataflowError _this, @CachedContext(Language.class) Context context) {
    return resolveMethodOnError(context, symbol);
  }
}
