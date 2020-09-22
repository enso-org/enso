package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;

import com.ibm.icu.text.BreakIterator;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Text",
    name = "each_with_index",
    description = "Iteration over grapheme clusters.")
public abstract class EachClusterNode extends Node {
  private @Child InvokeCallableNode invokeCallableNode =
      InvokeCallableNode.build(
          new CallArgumentInfo[] {new CallArgumentInfo(), new CallArgumentInfo()},
          InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
          InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);

  static EachClusterNode build() {
    return EachClusterNodeGen.create();
  }

  @CompilerDirectives.TruffleBoundary
  Object iterate(Object state, String str, Object fun, MaterializedFrame frame) {
    BreakIterator it = BreakIterator.getCharacterInstance();
    it.setText(str);
    long ix = 0;
    int s = it.first();
    int e = it.next();
    while (e != BreakIterator.DONE) {
      state =
          invokeCallableNode
              .execute(fun, frame, state, new Object[] {ix, str.substring(s, e)})
              .getState();
      s = e;
      e = it.next();
      ix++;
    }
    return state;
  }

  abstract Stateful execute(
      @MonadicState Object state, VirtualFrame frame, String _this, Object function);

  @Specialization
  Stateful doExecute(
      Object state,
      VirtualFrame frame,
      String str,
      Object fun,
      @CachedContext(Language.class) Context ctx) {
    state = iterate(state, str, fun, frame.materialize());
    return new Stateful(state, ctx.getBuiltins().unit().newInstance());
  }
}
