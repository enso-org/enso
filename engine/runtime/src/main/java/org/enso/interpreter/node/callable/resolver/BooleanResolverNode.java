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

@GenerateUncached
@ReportPolymorphism
public abstract class BooleanResolverNode extends BaseResolverNode {
  public abstract Function execute(UnresolvedSymbol symbol, boolean self);

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      boolean _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnPrimBoolean(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "_this",
        "function != null"
      },
      limit = "CACHE_SIZE",
      replaces = "resolveCached")
  Function resolveTrueCached(
      UnresolvedSymbol symbol,
      boolean _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnBool(context, true, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "!_this",
        "function != null"
      },
      limit = "CACHE_SIZE",
      replaces = "resolveCached")
  Function resolveFalseCached(
      UnresolvedSymbol symbol,
      boolean _this,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @CachedContext(Language.class) Context context,
      @Cached("resolveMethodOnBool(context, false, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = {"resolveTrueCached", "resolveFalseCached"})
  Function resolve(
      UnresolvedSymbol symbol, boolean _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnBool(context, _this, symbol);
    return throwIfNull(context, function, _this, symbol);
  }
}
