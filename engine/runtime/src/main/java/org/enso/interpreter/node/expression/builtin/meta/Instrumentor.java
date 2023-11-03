package org.enso.interpreter.node.expression.builtin.meta;

import java.util.UUID;

import org.enso.interpreter.instrument.Timer;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.polyglot.debugger.DefaultExecutionSupport;
import org.enso.polyglot.debugger.IdExecutionService;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;

final class Instrumentor implements EnsoObject, IdExecutionService.Callbacks {

  private final IdExecutionService service;
  private final CallTarget target;
  private final Module module;
  private final Object onEnter;
  private final Object onReturn;
  private final Object onCall;
  private final EventBinding<?> handle;

  Instrumentor(Module module, IdExecutionService service, CallTarget target) {
    this.module = module;
    this.service = service;
    this.target = target;
    this.onEnter = null;
    this.onReturn = null;
    this.onCall = null;
    this.handle = null;
  }

  Instrumentor(Instrumentor orig, Object onEnter, Object onReturn, Object onCall, boolean activate) {
    this.module = orig.module;
    this.service = orig.service;
    this.target = orig.target;
    this.onEnter = onEnter != null ? onEnter : orig.onEnter;
    this.onReturn = onReturn != null ? onReturn : orig.onReturn;
    this.onCall = onCall != null ? onCall : orig.onCall;
    this.handle = !activate ? null : service.bind(
      module, target, this, DefaultExecutionSupport.getInstance(), new Timer.Disabled()
    );
  }

  @CompilerDirectives.TruffleBoundary
  final Instrumentor deactivate() {
    if (handle != null && handle.isAttached()) {
      handle.dispose();
    }
    return this;
  }

  //
  // Callbacks
  //
  @Override
  public void onEnter(UUID nodeId) {
    try {
      if (onEnter != null) {
        InteropLibrary.getUncached().execute(onEnter, nodeId.toString());
      }
    } catch (InteropException ignored) {
    }
  }

  @Override
  public void onExpressionReturn(UUID nodeId, Object result, long nanoElapsedTime) {
    try {
      if (onReturn != null) {
        InteropLibrary.getUncached().execute(onReturn, nodeId.toString(), result);
      }
    } catch (InteropException ignored) {
    }
  }

  @Override
  public Object onFunctionReturn(UUID nodeId, TruffleObject result) {
    try {
      if (onCall != null && result instanceof FunctionCallInstrumentationNode.FunctionCall call) {
        var args = (Object[])call.getArguments().clone();
        for (var i = 0; i < args.length; i++) {
          if (args[i] == null) {
            args[i] = EnsoContext.get(null).getBuiltins().nothing();
          }
        }
        var ret = InteropLibrary.getUncached().execute(
                onCall,
                nodeId.toString(),
                call.getFunction(),
                ArrayLikeHelpers.asVectorWithCheckAt(args)
        );
        ret = InteropLibrary.getUncached().isNull(ret) ? null : ret;
        return handle.isDisposed() ? null : ret;
      }
    } catch (InteropException ignored) {
    }
    return null;
  }
}
