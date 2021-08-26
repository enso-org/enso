package org.enso.interpreter.runtime.number;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;

import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Number;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

import java.math.BigInteger;

/** Internal wrapper for a {@link BigInteger}. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(MethodDispatchLibrary.class)
public class EnsoBigInteger implements TruffleObject {
  private final BigInteger value;

  /**
   * Wraps a {@link BigInteger}.
   *
   * @param value the value to wrap.
   */
  public EnsoBigInteger(BigInteger value) {
    this.value = value;
  }

  /** @return the contained {@link BigInteger}. */
  public BigInteger getValue() {
    return value;
  }

  @Override
  @CompilerDirectives.TruffleBoundary
  public String toString() {
    return value.toString();
  }

  @ExportMessage
  String toDisplayString(boolean allowSideEffects) {
    return value.toString();
  }

  @ExportMessage
  boolean hasFunctionalDispatch() {
    return true;
  }

  @ExportMessage
  static class GetFunctionalDispatch {

    static final int CACHE_SIZE = 10;

    @CompilerDirectives.TruffleBoundary
    static Function doResolve(Context context, UnresolvedSymbol symbol) {
      Number number = context.getBuiltins().number();
      return symbol.resolveFor(
          number.getBigInteger(),
          number.getInteger(),
          number.getNumber(),
          context.getBuiltins().any());
    }

    @Specialization(
        guards = {
          "!context.isInlineCachingDisabled()",
          "cachedSymbol == symbol",
          "function != null"
        },
        limit = "CACHE_SIZE")
    static Function resolveCached(
        EnsoBigInteger _this,
        UnresolvedSymbol symbol,
        @CachedContext(Language.class) Context context,
        @Cached("symbol") UnresolvedSymbol cachedSymbol,
        @Cached("doResolve(context, cachedSymbol)") Function function) {
      return function;
    }

    @Specialization(replaces = "resolveCached")
    static Function resolve(
        EnsoBigInteger _this,
        UnresolvedSymbol symbol,
        @CachedContext(Language.class) Context context)
        throws MethodDispatchLibrary.NoSuchMethodException {
      Function function = doResolve(context, symbol);
      if (function == null) {
        throw new MethodDispatchLibrary.NoSuchMethodException();
      }
      return function;
    }
  }
}
