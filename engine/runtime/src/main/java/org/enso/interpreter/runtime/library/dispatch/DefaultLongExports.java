package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Number;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;

@ExportLibrary(value = MethodDispatchLibrary.class, receiverType = Long.class)
public class DefaultLongExports {
  @ExportMessage
  static boolean hasFunctionalDispatch(Long receiver) {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    @CompilerDirectives.TruffleBoundary
    static Function doResolve(UnresolvedSymbol symbol) {
      Context context = getContext();
      Number number = context.getBuiltins().number();
      return symbol.resolveFor(
          number.getSmallInteger(),
          number.getInteger(),
          number.getNumber(),
          context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    static final int CACHE_SIZE = 10;

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Long self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Long self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  public static boolean canConvertFrom(Long receiver) {
    return true;
  }

  @ExportMessage
  public static boolean hasSpecialConversion(Long receiver) {
    return false;
  }

  @ExportMessage
  static class GetConversionFunction {
    @CompilerDirectives.TruffleBoundary
    static Function doResolve(AtomConstructor target, UnresolvedConversion conversion) {
      Context context = getContext();
      Number number = context.getBuiltins().number();
      return conversion.resolveFor(
          target,
          number.getSmallInteger(),
          number.getInteger(),
          number.getNumber(),
          context.getBuiltins().any());
    }

    static Context getContext() {
      return Context.get(null);
    }

    static final int CACHE_SIZE = 10;

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Long self,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("doResolve(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(Long self, AtomConstructor target, UnresolvedConversion conversion)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = doResolve(target, conversion);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
