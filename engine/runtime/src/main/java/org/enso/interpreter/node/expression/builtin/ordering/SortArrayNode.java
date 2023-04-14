package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.state.State;

/** Just a wrapper for {@code Array.sort_builtin} that delegates to {@code Vector.sort_builtin}. */
@BuiltinMethod(type = "Array", name = "sort_builtin")
public final class SortArrayNode extends Node {
  @Child private SortVectorNode sortVectorNode = SortVectorNode.build();

  public Object execute(
      State state,
      @AcceptsError Object self,
      long ascending,
      Object comparators,
      Object compareFunctions,
      Object byFunc,
      Object onFunc,
      long problemBehavior) {
    return sortVectorNode.execute(
        state, self, ascending, comparators, compareFunctions, byFunc, onFunc, problemBehavior);
  }

  public static SortArrayNode build() {
    return new SortArrayNode();
  }
}
