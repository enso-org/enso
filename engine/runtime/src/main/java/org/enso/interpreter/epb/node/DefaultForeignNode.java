package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.DirectCallNode;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.epb.EpbContext;
import org.enso.interpreter.epb.EpbLanguage;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;

public abstract class DefaultForeignNode extends ForeignFunctionCallNode {

  private @Children ContextFlipNode[] argConverters;
  private @Child ContextFlipNode resultConverter = ContextFlipNodeGen.create();
  private @Child DirectCallNode callNode;

  DefaultForeignNode(int argsCount, CallTarget foreignCT) {
    this.argConverters = new ContextFlipNode[argsCount];
    for (int i = 0; i < argsCount; i++) {
      argConverters[i] = ContextFlipNodeGen.create();
    }
    this.callNode = DirectCallNode.create(foreignCT);
  }

  @Specialization
  Object doExecute(
      Object[] arguments,
      @CachedContext(EpbLanguage.class) TruffleLanguage.ContextReference<EpbContext> ctxRef) {
    EpbContext context = ctxRef.get();
    GuardedTruffleContext outer = context.getCurrentContext();
    GuardedTruffleContext inner = context.getInnerContext();
    Object[] args = prepareArgs(arguments, inner, outer);
    Object p = inner.enter();
    try {
      Object r = callNode.call(args);
      return resultConverter.execute(r, inner, outer);
    } finally {
      inner.leave(p);
    }
  }

  @ExplodeLoop
  private Object[] prepareArgs(Object[] args, GuardedTruffleContext inner, GuardedTruffleContext outer) {
    Object[] newArgs = new Object[argConverters.length];
    for (int i = 0; i < argConverters.length; i++) {
      newArgs[i] = argConverters[i].execute(args[i], outer, inner);
    }
    return newArgs;
  }
}
