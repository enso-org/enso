package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

@GenerateUncached
@ReportPolymorphism
public abstract class AtomResolverNode extends BaseResolverNode {
  public abstract Function execute(UnresolvedSymbol symbol, Atom self);

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "symbol == cachedSymbol",
        "_this.getConstructor() == cachedConstructor",
        "function != null"
      },
      limit = "CACHE_SIZE")
  Function resolveCached(
      UnresolvedSymbol symbol,
      Atom _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("_this.getConstructor()") AtomConstructor cachedConstructor,
      @Cached("resolveMethodOnAtom(context, cachedConstructor, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveCached")
  Function resolve(
      UnresolvedSymbol symbol, Atom atom, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnAtom(context, atom.getConstructor(), symbol);
    return throwIfNull(context, function, atom, symbol);
  }
}
