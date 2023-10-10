package org.enso.interpreter.node.expression.builtin.meta;


import com.oracle.truffle.api.nodes.Node;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.PanicException;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;

@BuiltinMethod(
    type = "Meta",
    name = "instrumentor_builtin",
    description = "Handles instrumentation operations.",
    autoRegister = false)
public class InstrumentorBuiltin extends Node {
  @Child
  private ArrayLikeAtNode atNode = ArrayLikeAtNode.create();

  Object execute(Text operation, Object args) {
    var ctx = EnsoContext.get(this);
    var op = operation.toString();
    try {
      Object ret = "newBuilder".equals(op) ? newBuilder(ctx, atNode.executeAt(args, 0)) : null;
      if (atNode.executeAt(args, 0) instanceof Instrumentor b) {
        ret = switch (op) {
          case "onEnter" -> onEnter(b, atNode.executeAt(args, 1));
          case "onReturn" -> onReturn(b, atNode.executeAt(args, 1));
          case "onCall" -> onCall(b, atNode.executeAt(args, 1));
          case "activate" -> activate(b, atNode.executeAt(args, 1));
          case "deactivate" -> b.deactivate();
          default -> null;
        };
      }
      if (ret == null) {
        var err = ctx.getBuiltins().error().makeUnimplemented(operation.toString());
        throw new PanicException(err, this);
      }
      return ret;
    } catch (InvalidArrayIndexException ex) {
      CompilerDirectives.transferToInterpreter();
      var len = ArrayLikeLengthNode.getUncached().executeLength(args);
      var err = ctx.getBuiltins().error().makeIndexOutOfBounds(ex.getInvalidIndex(), len);
      throw new PanicException(err, this);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Instrumentor newBuilder(EnsoContext ctx, Object arg) {
    if (arg instanceof UnresolvedSymbol symbol) {
      var fnAndType = symbol.resolveFor(this, symbol.getScope().getAssociatedType());
      if (fnAndType != null) {
        var service = ctx.getIdValueExtractor();
        if (service != null) {
          return new Instrumentor(symbol.getScope().getModule(), service, fnAndType.getLeft().getCallTarget());
        }
      }
    }
    return null;
  }

  @CompilerDirectives.TruffleBoundary
  private Object onEnter(Instrumentor b, Object arg) {
    if (arg instanceof Function fn) {
      return new Instrumentor(b, fn, null, null, false);
    } else {
      return null;
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object onReturn(Instrumentor b, Object arg) {
    if (arg instanceof Function fn) {
      return new Instrumentor(b, null, fn, null, false);
    } else {
      return null;
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object onCall(Instrumentor b, Object arg) {
    if (arg instanceof Function fn) {
      return new Instrumentor(b, null, null, fn, false);
    } else {
      return null;
    }
  }

  @CompilerDirectives.TruffleBoundary
  private Object activate(Instrumentor b, Object arg) {
    if (arg instanceof Function fn) {
      var builder = new Instrumentor(b, null, null, null, true);
      var ctx = EnsoContext.get(this);
      return ctx.getResourceManager().register(builder, fn);
    } else {
      return null;
    }
  }
}
