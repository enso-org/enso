package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Bool;
import org.enso.interpreter.runtime.builtin.Number;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.RuntimeError;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

/**
 * A node performing lookups of method definitions.
 *
 * <p>Uses a polymorphic inline cache to ensure the best performance.
 *
 * <p>The dispatch algorithm works by matching the kind of value the method is requested for and
 * delegating to the proper lookup method of {@link UnresolvedSymbol}.
 */
@NodeInfo(shortName = "MethodResolver", description = "Resolves method calls to concrete targets")
@GenerateUncached
@ReportPolymorphism
public abstract class MethodResolverNode extends Node {
  static final int CACHE_SIZE = 10;

  /**
   * Creates an instance of this node.
   *
   * @return a method resolver node
   */
  public static MethodResolverNode build() {
    return MethodResolverNodeGen.create();
  }

  /**
   * Entry point for this node.
   *
   * @param symbol Method name to resolve.
   * @param self Object for which to resolve the method.
   * @return Resolved method.
   */
  public abstract Function execute(UnresolvedSymbol symbol, Object self);

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "symbol == cachedSymbol",
        "_this.getConstructor() == cachedConstructor",
        "function != null"
      },
      limit = "CACHE_SIZE")
  Function resolveAtomCached(
      UnresolvedSymbol symbol,
      Atom _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("_this.getConstructor()") AtomConstructor cachedConstructor,
      @Cached("resolveMethodOnAtom(context, cachedConstructor, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveAtomCached")
  Function resolveAtom(
      UnresolvedSymbol symbol, Atom atom, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnAtom(context, atom.getConstructor(), symbol);
    return throwIfNull(context, function, atom, symbol);
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "_this == cachedConstructor",
        "function != null"
      },
      limit = "CACHE_SIZE")
  Function resolveAtomConstructorCached(
      UnresolvedSymbol symbol,
      AtomConstructor _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("_this") AtomConstructor cachedConstructor,
      @Cached("resolveMethodOnAtom(context, cachedConstructor, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveAtomConstructorCached")
  Function resolveAtomConstructor(
      UnresolvedSymbol symbol,
      AtomConstructor _this,
      @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnAtom(context, _this, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveBigIntegerCached(
      UnresolvedSymbol symbol,
      EnsoBigInteger _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnBigInteger(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveBigIntegerCached")
  Function resolveBigInteger(
      UnresolvedSymbol symbol,
      EnsoBigInteger _this,
      @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnBigInteger(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveLongCached(
      UnresolvedSymbol symbol,
      long _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnLong(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveLongCached")
  Function resolveLong(
      UnresolvedSymbol symbol, long _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnLong(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveDoubleCached(
      UnresolvedSymbol symbol,
      double _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnDouble(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveDoubleCached")
  Function resolveDouble(
      UnresolvedSymbol symbol, double _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnDouble(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveBooleanCached(
      UnresolvedSymbol symbol,
      boolean _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnPrimBoolean(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "_this",
        "function != null"
      },
      limit = "CACHE_SIZE",
      replaces = "resolveBooleanCached")
  Function resolveTrueCached(
      UnresolvedSymbol symbol,
      boolean _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnBool(context, true, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "!_this",
        "function != null"
      },
      limit = "CACHE_SIZE",
      replaces = "resolveBooleanCached")
  Function resolveFalseCached(
      UnresolvedSymbol symbol,
      boolean _this,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @CachedContext(Language.class) Context context,
      @Cached("resolveMethodOnBool(context, false, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = {"resolveTrueCached", "resolveFalseCached"})
  Function resolveBoolean(
      UnresolvedSymbol symbol, boolean _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnBool(context, _this, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveStringCached(
      UnresolvedSymbol symbol,
      Text _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnString(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveStringCached")
  Function resolveString(
      UnresolvedSymbol symbol, Text _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnString(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveFunctionCached(
      UnresolvedSymbol symbol,
      Function _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnFunction(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveFunctionCached")
  Function resolveFunction(
      UnresolvedSymbol symbol, Function _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnFunction(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveErrorCached(
      UnresolvedSymbol symbol,
      RuntimeError _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnError(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveErrorCached")
  Function resolveError(
      UnresolvedSymbol symbol, RuntimeError _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnError(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {"!context.isCachingDisabled()", "cachedSymbol == symbol", "function != null"},
      limit = "CACHE_SIZE")
  Function resolveArrayCached(
      UnresolvedSymbol symbol,
      Array _this,
      @CachedContext(Language.class) Context context,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnArray(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(replaces = "resolveArrayCached")
  Function resolveArray(
      UnresolvedSymbol symbol, Array _this, @CachedContext(Language.class) Context context) {
    Function function = resolveMethodOnArray(context, symbol);
    return throwIfNull(context, function, _this, symbol);
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "symbol == cachedSymbol",
        "isPolyglotArrayMethod(cachedSymbol)",
        "arrays.hasArrayElements(_this)"
      },
      limit = "CACHE_SIZE")
  Function resolvePolyglotArrayCached(
      UnresolvedSymbol symbol,
      Object _this,
      @CachedContext(Language.class) Context context,
      @CachedLibrary("_this") InteropLibrary arrays,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @Cached("resolveMethodOnPolyglotArray(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = {"isPolyglotArrayMethod(symbol)", "arrays.hasArrayElements(_this)"},
      replaces = "resolvePolyglotArrayCached")
  Function resolvePolyglotArray(
      UnresolvedSymbol symbol,
      Object _this,
      @CachedContext(Language.class) Context context,
      @CachedLibrary(limit = "CACHE_SIZE") InteropLibrary arrays) {
    return resolveMethodOnPolyglotArray(context, symbol);
  }

  @Specialization(
      guards = {
        "!context.isCachingDisabled()",
        "cachedSymbol == symbol",
        "context.getEnvironment().isHostObject(_this)"
      },
      limit = "CACHE_SIZE")
  Function resolveHostCached(
      UnresolvedSymbol symbol,
      Object _this,
      @Cached("symbol") UnresolvedSymbol cachedSymbol,
      @CachedContext(Language.class) Context context,
      @Cached("buildHostResolver(context, cachedSymbol)") Function function) {
    return function;
  }

  @Specialization(
      guards = "context.getEnvironment().isHostObject(_this)",
      replaces = "resolveHostCached")
  Function resolveHost(
      UnresolvedSymbol symbol, Object _this, @CachedContext(Language.class) Context context) {
    return buildHostResolver(context, symbol);
  }

  @Fallback
  Function resolveUnknown(UnresolvedSymbol symbol, Object _this) {
    CompilerDirectives.transferToInterpreter();
    Context context = lookupContextReference(Language.class).get();
    throw new PanicException(
        context.getBuiltins().error().makeNoSuchMethodError(_this, symbol), this);
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnAtom(Context context, AtomConstructor cons, UnresolvedSymbol symbol) {
    return symbol.resolveFor(cons, context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnLong(Context context, UnresolvedSymbol symbol) {
    Number number = context.getBuiltins().number();
    return symbol.resolveFor(
        number.getSmallInteger(),
        number.getInteger(),
        number.getNumber(),
        context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnBigInteger(Context context, UnresolvedSymbol symbol) {
    Number number = context.getBuiltins().number();
    return symbol.resolveFor(
        number.getBigInteger(),
        number.getInteger(),
        number.getNumber(),
        context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnDouble(Context context, UnresolvedSymbol symbol) {
    Number number = context.getBuiltins().number();
    return symbol.resolveFor(number.getDecimal(), number.getNumber(), context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnPrimBoolean(Context context, UnresolvedSymbol symbol) {
    Bool bool = context.getBuiltins().bool();
    if (symbol.resolveFor(bool.getFalse()) != null) {
      return null;
    }
    if (symbol.resolveFor(bool.getTrue()) != null) {
      return null;
    }
    return symbol.resolveFor(bool.getBool(), context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnBool(Context context, boolean self, UnresolvedSymbol symbol) {
    Bool bool = context.getBuiltins().bool();
    AtomConstructor cons = self ? bool.getTrue() : bool.getFalse();
    return symbol.resolveFor(cons, bool.getBool(), context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnString(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(context.getBuiltins().text().getText(), context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnFunction(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(context.getBuiltins().function(), context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnError(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnArray(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(
        context.getBuiltins().mutable().array(), context.getBuiltins().any());
  }

  @CompilerDirectives.TruffleBoundary
  Function buildHostResolver(Context context, UnresolvedSymbol symbol) {
    if (symbol.getName().equals("new")) {
      return context.getBuiltins().polyglot().getConstructorDispatch();
    } else if (symbol.getName().equals("to_text")) {
      return context.getBuiltins().polyglot().getPolyglotToTextFunction();
    } else if (symbol.getName().equals("catch")) {
      return symbol.resolveFor(context.getBuiltins().any());
    } else if (symbol.getName().equals("==")) {
      return symbol.resolveFor(context.getBuiltins().any());
    } else {
      return context.getBuiltins().polyglot().buildPolyglotMethodDispatch(symbol);
    }
  }

  static boolean isPolyglotArrayMethod(UnresolvedSymbol symbol) {
    return symbol.getName().equals("at") || symbol.getName().equals("length");
  }

  Function resolveMethodOnPolyglotArray(Context context, UnresolvedSymbol symbol) {
    if (symbol.getName().equals("length")) {
      return context.getBuiltins().polyglot().getPolyglotArrayLengthFunction();
    } else {
      return context.getBuiltins().polyglot().getPolyglotArrayAtFunction();
    }
  }

  private Function throwIfNull(
      Context context, Function function, Object _this, UnresolvedSymbol sym) {
    if (function == null) {
      CompilerDirectives.transferToInterpreter();
      throw new PanicException(
          context.getBuiltins().error().makeNoSuchMethodError(_this, sym), this);
    }
    return function;
  }
}
