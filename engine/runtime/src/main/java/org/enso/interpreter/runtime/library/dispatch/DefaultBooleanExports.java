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
    static Function resolveMethodOnPrimBoolean(UnresolvedSymbol symbol) {
      Context context = getContext();
      Builtins builtins = context.getBuiltins();
      if (symbol.resolveFor(builtins.bool().getFalse()) != null) {
        return null;
      }
      if (symbol.resolveFor(builtins.bool().getTrue()) != null) {
        return null;
      }
      return symbol.resolveFor(builtins.bool().getBool(), context.getBuiltins().any());
    }

    @CompilerDirectives.TruffleBoundary
    static Function resolveMethodOnBool(boolean self, UnresolvedSymbol symbol) {
      Context context = getContext();
      Builtins builtins = context.getBuiltins();
      AtomConstructor cons = self ? builtins.bool().getTrue() : builtins.bool().getFalse();
      return symbol.resolveFor(cons, builtins.bool().getBool(), context.getBuiltins().any());
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
        @Cached("resolveMethodOnPrimBoolean(cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "unbox(self)",
          "function != null"
        },
        limit = "CACHE_SIZE",
        replaces = "resolveCached")
    static Function resolveTrueCached(
        Boolean self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("resolveMethodOnBool(self, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "!unbox(self)",
          "function != null"
        },
        limit = "CACHE_SIZE",
        replaces = "resolveCached")
    static Function resolveFalseCached(
        Boolean self,
        UnresolvedSymbol symbol,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("resolveMethodOnBool(self, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = {"resolveTrueCached", "resolveFalseCached"})
    static Function resolve(Boolean self, UnresolvedSymbol symbol)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = resolveMethodOnBool(self, symbol);
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
    static Function resolveMethodOnPrimBoolean(
        AtomConstructor target, UnresolvedConversion conversion) {
      Context context = Context.get(null);
      Builtins builtins = context.getBuiltins();
      if (conversion.resolveFor(target, builtins.bool().getFalse()) != null) {
        return null;
      }
      if (conversion.resolveFor(target, builtins.bool().getTrue()) != null) {
        return null;
      }
      return conversion.resolveFor(target, builtins.bool().getBool(), context.getBuiltins().any());
    }

    @CompilerDirectives.TruffleBoundary
    static Function resolveMethodOnBool(
        boolean self, AtomConstructor target, UnresolvedConversion conversion) {
      Context context = Context.get(null);
      Builtins builtins = context.getBuiltins();
      AtomConstructor cons = self ? builtins.bool().getTrue() : builtins.bool().getFalse();
      return conversion.resolveFor(
          target, cons, builtins.bool().getBool(), context.getBuiltins().any());
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
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("resolveMethodOnPrimBoolean(cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "unbox(self)",
          "function != null"
        },
        limit = "CACHE_SIZE",
        replaces = "resolveCached")
    static Function resolveTrueCached(
        Boolean self,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("resolveMethodOnBool(self, cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(
        guards = {
          "!getContext().isInlineCachingDisabled()",
          "cachedConversion == conversion",
          "cachedTarget == target",
          "!unbox(self)",
          "function != null"
        },
        limit = "CACHE_SIZE",
        replaces = "resolveCached")
    static Function resolveFalseCached(
        Boolean self,
        AtomConstructor target,
        UnresolvedConversion conversion,
        @Cached("conversion") UnresolvedConversion cachedConversion,
        @Cached("target") AtomConstructor cachedTarget,
        @Cached("resolveMethodOnBool(self, cachedTarget, cachedConversion)") Function function) {
      return function;
    }

    @Specialization(replaces = {"resolveTrueCached", "resolveFalseCached"})
    static Function resolve(Boolean self, AtomConstructor target, UnresolvedConversion symbol)
        throws MethodDispatchLibrary.NoSuchConversionException {
      Function function = resolveMethodOnBool(self, target, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchConversionException();
      }
      return function;
    }
  }
}
