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
import org.graalvm.collections.Pair;

@GenerateUncached
@ReportPolymorphism
public abstract class MethodResolverNode extends Node {
  protected static final int CACHE_SIZE = 10;

  @NonIdempotent
  EnsoContext getContext() {
    return EnsoContext.get(this);
  }

  public abstract Pair<Function, Type> execute(Type type, UnresolvedSymbol symbol);

  public final Function executeResolution(Type type, UnresolvedSymbol symbol) {
    var pair = execute(type, symbol);
    return pair == null ? null : pair.getLeft();
  }

  public final Function expectNonNull(Object self, Type type, UnresolvedSymbol symbol) {
    var result = execute(type, symbol);
    if (result == null) {
      throw new PanicException(
          EnsoContext.get(this).getBuiltins().error().makeNoSuchMethod(self, symbol), this);
    }
    return result.getLeft();
  }

  @Specialization(
      guards = {
        "!getContext().isInlineCachingDisabled()",
        "cachedSymbol == symbol",
        "cachedType == type"
      },
      limit = "CACHE_SIZE")
  Pair<Function, Type> resolveCached(
      Type type,
      UnresolvedSymbol symbol,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("type") Type cachedType,
      @Cached("resolveUncached(cachedType, cachedSymbol)") Pair<Function, Type> function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Pair<Function, Type> resolveUncached(Type self, UnresolvedSymbol symbol) {
    return symbol.resolveFor(this, self);
  }
}
