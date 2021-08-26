package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Number;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;

@ExportLibrary(value = MethodDispatchLibrary.class, receiverType = Double.class)
public class DefaultDoubleExports {
  @ExportMessage
  static boolean hasFunctionalDispatch(Double receiver) {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    @CompilerDirectives.TruffleBoundary
    static Function doResolve(Context context, UnresolvedSymbol symbol) {
      Number number = context.getBuiltins().number();
      return symbol.resolveFor(
          number.getDecimal(), number.getNumber(), context.getBuiltins().any());
    }

    static final int CACHE_SIZE = 10;

    @Specialization(
        guards = {
          "!context.isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Double _this,
        UnresolvedSymbol symbol,
        @CachedContext(Language.class) Context context,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(context, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        Double _this, UnresolvedSymbol symbol, @CachedContext(Language.class) Context context)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(context, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }
}
