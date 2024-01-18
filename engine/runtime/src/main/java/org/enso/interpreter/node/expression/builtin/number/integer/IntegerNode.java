package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.callable.InteropConversionCallNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.state.State;

abstract class IntegerNode extends Node {
  private final String symbol;
  @Child ToEnsoNumberNode toEnsoNumberNode = ToEnsoNumberNode.create();

  IntegerNode(String symbol) {
    this.symbol = symbol;
  }

  @TruffleBoundary
  final PanicException throwTypeErrorIfNotInt(Object self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    var selfType = TypesLibrary.getUncached().getType(self);
    if (selfType != intType) {
      return new PanicException(builtins.error().makeTypeError(intType, self, "self"), this);
    } else {
      return new PanicException(builtins.error().makeTypeError(intType, that, "that"), this);
    }
  }

  @TruffleBoundary
  final PanicException throwTypeErrorIfNotInt(Object self) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    return new PanicException(builtins.error().makeTypeError(intType, self, "self"), this);
  }

  final boolean isForeignNumber(InteropLibrary iop, TruffleObject obj) {
    if (obj instanceof EnsoBigInteger) {
      return false;
    }
    return iop.isNumber(obj);
  }

  final Object doInterop(
      VirtualFrame frame,
      Object self,
      TruffleObject that,
      InteropLibrary iop,
      IntegerNode delegate) {
    try {
      if (iop.fitsInLong(that)) {
        return delegate.execute(frame, self, iop.asLong(that));
      } else if (iop.fitsInDouble(that)) {
        return delegate.execute(frame, self, iop.asDouble(that));
      } else if (iop.fitsInBigInteger(that)) {
        return delegate.execute(frame, self, toEnsoNumberNode.execute(iop.asBigInteger(that)));
      }
    } catch (UnsupportedMessageException ex) {
    }
    return doOther(frame, self, that);
  }

  Object execute(VirtualFrame frame, Object self, Object that) {
    throw new AbstractMethodError();
  }

  Object doOther(VirtualFrame frame, Object self, Object that) {
    if (doThatConversion(frame, self, that) instanceof Object result) {
      return result;
    } else {
      throw throwTypeErrorIfNotInt(self, that);
    }
  }

  final Object doThatConversion(VirtualFrame frame, Object self, Object that) {
    var node = DoThatConversionNode.getUncached();
    return node.executeThatConversion(frame, symbol, self, that);
  }

  /**
   * A node that checks the type of {@code that} argument and performs conversion of {@code self} to
   * {@code that} type, if it contains the requested symbol implementation.
   */
  @GenerateUncached
  abstract static class DoThatConversionNode extends Node {
    @NeverDefault
    static DoThatConversionNode getUncached() {
      return IntegerNodeFactory.DoThatConversionNodeGen.getUncached();
    }

    @NeverDefault
    static DoThatConversionNode create() {
      return IntegerNodeFactory.DoThatConversionNodeGen.create();
    }

    /**
     * @return {code null} if the conversion makes no sense or result of doing the conversion an
     *     executing the {@link #symbol}
     */
    abstract Object executeThatConversion(
        VirtualFrame frame, String symbol, Object self, Object that);

    static Type findType(TypeOfNode typeOfNode, Object obj) {
      var rawType = typeOfNode.execute(obj);
      return rawType instanceof Type type ? type : null;
    }

    static Type findTypeUncached(Object obj) {
      return findType(TypeOfNode.getUncached(), obj);
    }

    final Function findSymbol(String symbol, Type type) {
      var unresolved = UnresolvedSymbol.build(symbol, type.getDefinitionScope());
      var found = unresolved.resolveFor(this, type);
      if (found != null) {
        return found.getLeft();
      } else {
        return null;
      }
    }

    @NeverDefault
    static UnresolvedConversion buildConversion(Type type) {
      return UnresolvedConversion.build(type.getDefinitionScope());
    }

    @Specialization(
        limit = "3",
        guards = {
          "cachedSymbol.equals(symbol)",
          "selfType != null",
          "thatType != null",
          "symbolFn != null",
          "thatType == findType(typeOfNode, that)"
        })
    final Object doThatConversion(
        VirtualFrame frame,
        String symbol,
        Object self,
        Object that,
        @Cached("symbol") String cachedSymbol,
        @Cached TypeOfNode typeOfNode,
        @Cached(value = "findType(typeOfNode, self)", uncached = "findTypeUncached(self)")
            Type selfType,
        @Cached(value = "findType(typeOfNode, that)", uncached = "findTypeUncached(that)")
            Type thatType,
        @Cached(allowUncached = true, value = "findSymbol(cachedSymbol, thatType)")
            Function symbolFn,
        @Cached(allowUncached = true, value = "buildConversion(thatType)")
            UnresolvedConversion convert,
        @Cached InteropConversionCallNode convertNode,
        @Cached(allowUncached = true, value = "buildWithArity(2)") InvokeFunctionNode invokeNode) {
      var ctx = EnsoContext.get(this);
      var state = State.create(ctx);
      try {
        var selfAsThat = convertNode.execute(convert, state, new Object[] {thatType, self});
        var result = invokeNode.execute(symbolFn, frame, state, new Object[] {selfAsThat, that});
        return result;
      } catch (ArityException ex) {
        return null;
      }
    }

    @Specialization
    final Object doThatConversionSlow(VirtualFrame frame, String symbol, Object self, Object that) {
      return null;
    }
  }
}
