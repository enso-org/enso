package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NonIdempotent;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;

@GenerateUncached
@ReportPolymorphism
public abstract class MethodResolverNode extends Node {
  protected static final int CACHE_SIZE = 10;

  @NonIdempotent
  EnsoContext getContext() {
    return EnsoContext.get(this);
  }

  public abstract Function execute(Type type, UnresolvedSymbol symbol);

  public Function expectNonNull(Object self, Type type, UnresolvedSymbol symbol) {
    var result = execute(type, symbol);
    if (result == null) {
      throw new PanicException(
          EnsoContext.get(this).getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
    return result;
  }

  @Specialization(
      guards = {
        "!getContext().isInlineCachingDisabled()",
        "cachedSymbol == symbol",
        "cachedType == type"
      },
      limit = "CACHE_SIZE")
  Function resolveCached(
      Type type,
      UnresolvedSymbol symbol,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("type") Type cachedType,
      @Cached("resolveUncached(cachedType, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolveUncached(Type self, UnresolvedSymbol symbol) {
    return symbol.resolveFor(self);
  }
}
