package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Ordering;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;
import org.enso.interpreter.runtime.state.data.EmptyMap;

@NodeInfo(
    shortName = "sortComparator",
    description = "The implementation of the comparator for Array sorting.")
public abstract class ComparatorNode extends Node {
  private @Child InvokeCallableNode invokeNode;

  public static ComparatorNode build() {
    return ComparatorNodeGen.create();
  }

  ComparatorNode() {
    CallArgumentInfo[] callArguments = {new CallArgumentInfo(), new CallArgumentInfo()};
    invokeNode =
        InvokeCallableNode.build(
            callArguments, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.PRE_EXECUTED);
  }

  abstract int execute(VirtualFrame frame, Object comparator, Object l, Object r);

  Ordering getOrdering() {
    return Context.get(this).getBuiltins().ordering();
  }

  @Specialization
  int execute(
      VirtualFrame frame,
      Object comparator,
      Object l,
      Object r,
      @Cached("getOrdering().newLess()") Atom less,
      @Cached("getOrdering().newEqual()") Atom equal,
      @Cached("getOrdering().newGreater()") Atom greater) {
    Stateful result = invokeNode.execute(comparator, frame, EmptyMap.create(), new Object[] {l, r});
    Object atom = result.getValue();
    if (atom == less) {
      return -1;
    } else if (atom == equal) {
      return 0;
    } else if (atom == greater) {
      return 1;
    } else {
      CompilerDirectives.transferToInterpreter();
      var ordering = getOrdering().ordering();
      throw new PanicException(
          Context.get(this).getBuiltins().error().makeTypeError(ordering, atom, "comparator"), this);
    }
  }
}
