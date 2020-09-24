package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;

import com.ibm.icu.text.BreakIterator;
import org.enso.interpreter.dsl.MonadicState;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.text.Text;
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
  private @Child ToJavaStringNode toJavaStringNode = ToJavaStringNode.build();

  static EachClusterNode build() {
    return EachClusterNodeGen.create();
  }

  @CompilerDirectives.TruffleBoundary
  private Object iterate(Object state, Text txt, Object fun, MaterializedFrame frame) {
    BreakIterator it = BreakIterator.getCharacterInstance();
    String str = toJavaStringNode.execute(txt);
    it.setText(str);
    long ix = 0;
    int s = it.first();
    int e = it.next();
    while (e != BreakIterator.DONE) {
      state =
          invokeCallableNode
              .execute(fun, frame, state, new Object[] {ix, Text.create(str.substring(s, e))})
              .getState();
      s = e;
      e = it.next();
      ix++;
    }
    return state;
  }

  abstract Stateful execute(
      @MonadicState Object state, VirtualFrame frame, Text _this, Object function);

  @Specialization
  Stateful doExecute(
      Object state,
      VirtualFrame frame,
      Text str,
      Object fun,
      @CachedContext(Language.class) Context ctx) {
    state = iterate(state, str, fun, frame.materialize());
    return new Stateful(state, ctx.getBuiltins().unit().newInstance());
  }
}
