package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.DataflowError;

@GenerateUncached
@ReportPolymorphism
public abstract class DataflowErrorResolverNode extends BaseResolverNode {

  public abstract Function execute(UnresolvedSymbol symbol, DataflowError self);

  @Specialization(
      guards = {"!getContext().isInlineCachingDisabled()", "cachedSymbol == symbol"},
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      DataflowError self,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnError(cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolve(UnresolvedSymbol symbol, DataflowError self) {
    return resolveMethodOnError(symbol);
  }
}
