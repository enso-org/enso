package org.enso.interpreter.runtime.state;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.DynamicObjectLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.node.BaseNode.TailStatus;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.EnsoContext;

/** Use this node to manipulate {@link State}. */
@BuiltinMethod(
    type = "State",
    name = "get",
    description = "Returns the current value of monadic state.",
    autoRegister = false)
@ReportPolymorphism
@GenerateUncached
public abstract class GetStateNode extends Node {
  public static GetStateNode build() {
    return GetStateNodeGen.create();
  }

  public static GetStateNode getUncached() {
    return GetStateNodeGen.getUncached();
  }

  GetStateNode() {}

  final Object execute(VirtualFrame frame, Object key, @Suspend Object ifMissing) {
    return executeGet(frame, key, ifMissing);
  }

  /**
   * Reads value associated with a key from the {@link State}.
   *
   * @param frame the current execution frame
   * @param key the key to read the value for
   * @param ifMissing value to evaluate when state isn't set
   * @return the value associated with the key
   * @throws {@link PanicException} when there is no such key in the {@link State}
   */
  public abstract Object executeGet(VirtualFrame frame, Object key, Object ifMissing);

  final State state() {
    return EnsoContext.get(this).currentState();
  }

  @Specialization(guards = "objects.containsKey(data, key)")
  Object doRead(
      Object key,
      Object ifMissing,
      @Bind("state().getContainer()") State.Container data,
      @CachedLibrary(limit = "10") DynamicObjectLibrary objects) {
    return objects.getOrDefault(data, key, null);
  }

  @Fallback
  Object doMissing(
      VirtualFrame frame, Object key, Object ifMissing, @Cached ThunkExecutorNode evalNode) {
    var missingResult = evalNode.executeThunk(frame, ifMissing, state(), TailStatus.NOT_TAIL);
    return missingResult;
  }
}
