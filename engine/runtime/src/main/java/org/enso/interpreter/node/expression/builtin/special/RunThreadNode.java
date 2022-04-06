package org.enso.interpreter.node.expression.builtin.special;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNodeGen;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(type = "Special", name = "<run_thread>")
public abstract class RunThreadNode extends Node {
  static RunThreadNode build() {
    return RunThreadNodeGen.create();
  }

  abstract Thread execute(@MonadicState Object state, @Suspend Object th);

  @CompilerDirectives.TruffleBoundary
  @Specialization
  Thread doExecute(Object state, Object th) {
    Context ctx = Context.get(this);
    Thread thread =
        ctx.getEnvironment()
            .createThread(
                () -> {
                  Object p = ctx.getThreadManager().enter();
                  try {
                    ThunkExecutorNodeGen.getUncached()
                        .executeThunk(th, state, BaseNode.TailStatus.NOT_TAIL);
                  } finally {
                    ctx.getThreadManager().leave(p);
                  }
                });
    thread.start();
    return thread;
  }
}
