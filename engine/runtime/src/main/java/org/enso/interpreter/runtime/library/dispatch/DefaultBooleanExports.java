package org.enso.interpreter.runtime.library.dispatch;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;

@ExportLibrary(value = MethodDispatchLibrary.class, receiverType = Boolean.class)
public class DefaultBooleanExports {

  static final int CACHE_SIZE = 10;

  static boolean unbox(Boolean b) {
    return b;
  }

  @ExportMessage
  static boolean hasFunctionalDispatch(Boolean receiver) {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {
    @CompilerDirectives.TruffleBoundary
    static Function resolveMethodOnBool(UnresolvedSymbol symbol) {
      Context context = getContext();
      Builtins builtins = context.getBuiltins();
      return symbol.resolveFor(builtins.bool().getType());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Boolean self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("resolveMethodOnBool(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = {"resolveCached"})
    static Function resolve(Boolean self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = resolveMethodOnBool(symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }

  @ExportMessage
  public static boolean canConvertFrom(Boolean receiver) {
    return true;
  }

  @ExportMessage
  public static boolean hasSpecialConversion(Boolean receiver) {
    return false;
  }

  @ExportMessage
  static class GetConversionFunction {
    @CompilerDirectives.TruffleBoundary
    static Function resolveMethodOnBool(Type target, UnresolvedConversion conversion) {
      Context context = Context.get(null);
      Builtins builtins = context.getBuiltins();
      return conversion.resolveFor(target, builtins.bool().getType());
    }

    static Context getContext() {
      return Context.get(null);
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        Boolean self,
        Type target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") Type cachedTarget,
        @Cached("resolveMethodOnBool(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = {"resolveCached"})
    static Function resolve(Boolean self, Type target, UnresolvedConversion symbol)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = resolveMethodOnBool(target, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
