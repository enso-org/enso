package org.enso.interpreter.node.callable.resolver;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Bool;
import org.enso.interpreter.runtime.builtin.Number;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.PanicException;

public class BaseResolverNode extends Node {
  protected static final int CACHE_SIZE = 10;

  protected Function throwIfNull(
      Context context, Function function, Object _this, UnresolvedSymbol sym) {
    if (function == null) {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(
          context.getBuiltins().error().makeNoSuchMethodError(_this, sym), this);
    }
    return function;
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnError(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(context.getBuiltins().dataflowError().constructor());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnAny(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(context.getBuiltins().any());
  }
}
