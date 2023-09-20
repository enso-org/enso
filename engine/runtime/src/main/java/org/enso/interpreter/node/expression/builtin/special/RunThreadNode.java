package org.enso.interpreter.node.expression.builtin.special;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNodeGen;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(type = "Special", name = "<run_thread>")
public abstract class RunThreadNode extends Node {
  static RunThreadNode build() {
    return RunThreadNodeGen.create();
  }

  abstract Thread execute(VirtualFrame frame, State state, @Suspend Object self);

  @CompilerDirectives.TruffleBoundary
  @Specialization
  Thread doExecute(MaterializedFrame frame, State state, Object self) {
    EnsoContext ctx = EnsoContext.get(this);
    Thread thread =
        ctx.createThread(
            false,
            () -> {
              Object p = ctx.getThreadManager().enter();
              try {
                ThunkExecutorNodeGen.getUncached()
                    .executeThunk(frame, self, state, BaseNode.TailStatus.NOT_TAIL);
              } finally {
                ctx.getThreadManager().leave(p);
              }
            });
    thread.start();
    return thread;
  }
}
