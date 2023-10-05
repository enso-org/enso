package org.enso.interpreter.node.expression.builtin.meta;

import java.lang.ref.PhantomReference;
import java.util.UUID;

import org.enso.distribution.locking.ResourceManager;

import com.oracle.truffle.api.nodes.Node;

import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.instrument.Timer;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.ManagedResource;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCoerceToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.polyglot.debugger.IdExecutionService;

import com.ibm.icu.impl.number.MutablePatternModifier;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;

@BuiltinMethod(
    type = "Meta",
    name = "instrumentor_builtin",
    description = "Handles instrumentation operations.",
    autoRegister = false)
public class InstrumentorBuiltin extends Node {
  private static final class Builder implements EnsoObject {
    final IdExecutionService service;
    final CallTarget target;
    final Module module;
    Object onEnter;
    Object onReturn;
    Object onCall;
    EventBinding<ExecutionEventNodeFactory> close;

    Builder(Module module, IdExecutionService service, CallTarget target) {
      this.module = module;
      this.service = service;
      this.target = target;
    }
  }

  @Child
  private ArrayLikeCoerceToArrayNode coerceToArrayNode = ArrayLikeCoerceToArrayNode.build();

  Object execute(Text operation, Object args) {
    var ctx = EnsoContext.get(this);
    var arr = coerceToArrayNode.execute(args);
    var ret = switch (operation.toString()) {
      case "newBuilder" -> newBuilder(ctx, arr);
      case "onEnter" -> onEnter((Builder) arr[0], (Function) arr[1]);
      case "onReturn" -> onReturn((Builder) arr[0], (Function) arr[1]);
      case "onCall" -> onCall((Builder) arr[0], (Function) arr[1]);
      case "activate" -> activate((Builder) arr[0], (Function)arr[1]);
      case "deactivate" -> deactivate((Builder) arr[0]);
      default -> null;
    };
    if (ret == null) {
      var err = ctx.getBuiltins().error().makeUnimplemented(operation.toString());
      throw new PanicException(err, this);
    }
    return ret;
  }

  @CompilerDirectives.TruffleBoundary
  private Builder newBuilder(EnsoContext ctx, Object[] args) {
    if (args.length > 0 && args[0] instanceof UnresolvedSymbol symbol) {
      var fnAndType = symbol.resolveFor(this, symbol.getScope().getAssociatedType());
      if (fnAndType != null) {
        var service = ctx.getIdValueExtractor();
        if (service != null) {
          return new Builder(symbol.getScope().getModule(), service, fnAndType.getLeft().getCallTarget());
        }
      }
    }
    return null;
  }

  @CompilerDirectives.TruffleBoundary
  private Object onEnter(Builder b, Function fn) {
    b.onEnter = fn;
    return b;
  }

  @CompilerDirectives.TruffleBoundary
  private Object onReturn(Builder b, Function fn) {
    b.onReturn = fn;
    return b;
  }

  @CompilerDirectives.TruffleBoundary
  private Object onCall(Builder b, Function fn) {
    b.onCall = fn;
    return b;
  }

  @CompilerDirectives.TruffleBoundary
  private Object activate(Builder builder, Function fn) {
    class Observe implements IdExecutionService.Callbacks {
      @Override
      public Object findCachedResult(UUID nodeId) {
        try {
          if (builder.onEnter != null) {
            var ret = InteropLibrary.getUncached().execute(builder.onEnter, nodeId.toString());
            return InteropLibrary.getUncached().isNull(ret) ? null : ret;
          }
        } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
        }
        return null;
      }

      @Override
      public void updateCachedResult(UUID nodeId, Object result, boolean isPanic, long nanoElapsedTime) {
        try {
          if (builder.onReturn != null) {
            InteropLibrary.getUncached().execute(builder.onReturn, nodeId.toString(), result);
          }
        } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
        }
      }

      @Override
      public Object onFunctionReturn(UUID nodeId, TruffleObject result) {
        try {
          if (builder.onCall != null && result instanceof FunctionCallInstrumentationNode.FunctionCall call) {
            var ret = InteropLibrary.getUncached().execute(
              builder.onCall,
              nodeId.toString(),
              call.getFunction(),
              ArrayLikeHelpers.asVectorWithCheckAt(call.getArguments())
            );
            return InteropLibrary.getUncached().isNull(ret) ? null : ret;
          }
        } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
        }
        return null;
      }
    }
    builder.close = builder.service.bind(
      builder.module, builder.target, new Observe(), new Timer.Disabled()
    );
    var ctx = EnsoContext.get(this);
    return ctx.getResourceManager().register(ctx, fn);
  }

  @CompilerDirectives.TruffleBoundary
  private Object deactivate(Builder builder) {
    if (builder.close != null) {
      builder.close.dispose();
    }
    return builder;
  }
}
