package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.PanicException;

@GenerateUncached
@ReportPolymorphism
public abstract class OtherResolverNode extends BaseResolverNode {
  public abstract Function execute(UnresolvedSymbol symbol, Object _this);

  @Specialization(
      guards = {
          "!context.isCachingDisabled()",
          "symbol == cachedSymbol",
          "isPolyglotArrayMethod(cachedSymbol)",
          "arrays.hasArrayElements(_this)"
      },
      limit = "CACHE_SIZE")
  Function resolvePolyglotArrayCached(
      UnresolvedSymbol symbol,
      Object _this,
      @CachedContext(Language.class) Context context,
      @CachedLibrary("_this") InteropLibrary arrays,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnPolyglotArray(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = {"isPolyglotArrayMethod(symbol)", "arrays.hasArrayElements(_this)"},
      replaces = "resolvePolyglotArrayCached")
  Function resolvePolyglotArray(
      UnresolvedSymbol symbol,
      Object _this,
      @CachedContext(Language.class) Context context,
      @CachedLibrary(limit = "CACHE_SIZE") InteropLibrary arrays) {
    return resolveMethodOnPolyglotArray(context, symbol);
  }

  @Specialization(
      guards = {
          "!context.isCachingDisabled()",
          "cachedSymbol == symbol",
          "context.getEnvironment().isHostObject(_this)"
      },
      limit = "CACHE_SIZE")
  Function resolveHostCached(
      UnresolvedSymbol symbol,
      Object _this,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @CachedContext(Language.class) Context context,
      @Cached("buildHostResolver(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = "context.getEnvironment().isHostObject(_this)",
      replaces = "resolveHostCached")
  Function resolveHost(
      UnresolvedSymbol symbol, Object _this, @CachedContext(Language.class) Context context) {
    return buildHostResolver(context, symbol);
  }

  @Fallback
  Function resolveUnknown(UnresolvedSymbol symbol, Object _this) {
    CompilerDirectives.transferToInterpreter();
    Context context = lookupContextReference(Language.class).get();
    throw new PanicException(
        context.getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
  }
}
