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
  protected Function resolveMethodOnAtom(
      Context context, AtomConstructor cons, UnresolvedSymbol symbol) {
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
    return symbol.resolveFor(context.getBuiltins().dataflowError().constructor());
  }

  @CompilerDirectives.TruffleBoundary
  Function resolveMethodOnArray(Context context, UnresolvedSymbol symbol) {
    return symbol.resolveFor(context.getBuiltins().mutable().array(), context.getBuiltins().any());
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
}
