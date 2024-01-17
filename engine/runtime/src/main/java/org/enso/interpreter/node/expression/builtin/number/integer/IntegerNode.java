package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.node.expression.literal.LiteralNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
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

  final Object doThatConversion(VirtualFrame frame, Object self, Object that) {
    var ctx = EnsoContext.get(this);
    var typeOfNode = TypeOfNode.getUncached();
    var rawSelfType = typeOfNode.execute(self);
    var rawThatType = typeOfNode.execute(that);
    ApplicationNode convertNode = null;
    Function symbolFn = null;
    InvokeFunctionNode invokeNode = null;
    if (rawSelfType == ctx.getBuiltins().number().getInteger()
        && rawThatType instanceof Type thatType) {
      CompilerDirectives.transferToInterpreter();
      var convert = UnresolvedConversion.build(thatType.getDefinitionScope());
      var unresolved = UnresolvedSymbol.build(this.symbol, thatType.getDefinitionScope());
      var found = unresolved.resolveFor(this, thatType);
      if (found != null) {
        symbolFn = found.getLeft();
        var conversionFn =
            convert.resolveFor(ctx, thatType, ctx.getBuiltins().number().getInteger());
        if (conversionFn != null) {
          var convNode = LiteralNode.build(conversionFn);
          var intoNode = LiteralNode.build(thatType);
          var valueNode = LiteralNode.build((Long) self); // completely wrong!!!
          var args =
              new CallArgument[] {
                new CallArgument(null, intoNode), new CallArgument(null, valueNode)
              };
          convertNode = ApplicationNode.build(convNode, args, DefaultsExecutionMode.EXECUTE);

          var schema =
              new CallArgumentInfo[] {new CallArgumentInfo("self"), new CallArgumentInfo("that")};
          invokeNode =
              InvokeFunctionNode.build(
                  schema, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE);
        }
      }
    }

    if (convertNode != null && rawThatType instanceof Type type) {
      var state = State.create(ctx);
      var convertedValue = convertNode.executeGeneric(frame);
      var result = invokeNode.execute(symbolFn, frame, state, new Object[] {convertedValue, that});
      return result;
    } else {
      return null;
    }
  }

  Object doOther(VirtualFrame frame, Object self, Object that) {
    if (doThatConversion(frame, self, that) instanceof Object result) {
      return result;
    } else {
      throw throwTypeErrorIfNotInt(self, that);
    }
  }
}
