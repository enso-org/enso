package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.PanicException;

public class BaseResolverNode extends Node {
  protected static final int CACHE_SIZE = 10;

  protected Context getContext() {
    return Context.get(this);
  }

  protected Function throwIfNull(Function function, Object _this, UnresolvedSymbol sym) {
    if (function == null) {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(
          getContext().getBuiltins().error().makeNoSuchMethodError(_this, sym), this);
    }
    return function;
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnError(UnresolvedSymbol symbol) {
    return symbol.resolveFor(getContext().getBuiltins().dataflowError().constructor());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnAny(UnresolvedSymbol symbol) {
    return symbol.resolveFor(getContext().getBuiltins().any());
  }
}
