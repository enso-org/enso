package org.enso.interpreter.node.expression.builtin.meta;

import org.enso.interpreter.instrument.Timer;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.polyglot.debugger.IdExecutionService;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;

final class Instrumentor implements EnsoObject, IdExecutionService.Callbacks {

  private final IdExecutionService service;
  private final CallTarget target;
  private final Module module;
  private final Object onEnter;
  private final Object onReturn;
  private final Object onReturnExpr;
  private final Object onCall;
  private final EventBinding<?> handle;

  Instrumentor(Module module, IdExecutionService service, CallTarget target) {
    this.module = module;
    this.service = service;
    this.target = target;
    this.onEnter = null;
    this.onReturn = null;
    this.onReturnExpr = null;
    this.onCall = null;
    this.handle = null;
  }

  Instrumentor(Instrumentor orig, Object onEnter, Object onReturn, Object onReturnExpr, Object onCall, boolean activate) {
    this.module = orig.module;
    this.service = orig.service;
    this.target = orig.target;
    this.onEnter = onEnter != null ? onEnter : orig.onEnter;
    this.onReturn = onReturn != null ? onReturn : orig.onReturn;
    this.onReturnExpr = onReturnExpr != null ? onReturnExpr : orig.onReturnExpr;
    this.onCall = onCall != null ? onCall : orig.onCall;
    this.handle = !activate ? null : service.bind(
      module, target, this, new Timer.Disabled()
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
  public Object findCachedResult(IdExecutionService.Info info) {
    try {
      if (onEnter != null) {
        var ret = InteropLibrary.getUncached().execute(onEnter, info.getId().toString());
        ret = InteropLibrary.getUncached().isNull(ret) ? null : ret;
        return handle.isDisposed() ? null : ret;    }
    } catch (InteropException ignored) {
    }
    return null;
  }

  @Override
  public void updateCachedResult(IdExecutionService.Info info) {
    try {
      if (onReturn != null) {
        var iop = InteropLibrary.getUncached();
        var result = onReturnExpr == null || !iop.isString(onReturnExpr) ?
          info.getResult()
          :
          InstrumentorEvalNode.asSuspendedEval(onReturnExpr, info);
        iop.execute(onReturn, info.getId().toString(), result);
      }
    } catch (InteropException ignored) {
    }
  }

  @Override
  public Object onFunctionReturn(IdExecutionService.Info info) {
    try {
      if (onCall != null
          && info.getResult() instanceof FunctionCallInstrumentationNode.FunctionCall call) {
        var args = (Object[]) call.getArguments().clone();
        for (var i = 0; i < args.length; i++) {
          if (args[i] == null) {
            args[i] = EnsoContext.get(null).getBuiltins().nothing();
          }
        }
        var ret = InteropLibrary.getUncached().execute(
                onCall,
                info.getId().toString(),
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
