package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.state.State;

/**
 * Helper node for invocation of {@code Comparable.hash_callback atom}. Note that emulating the
 * semantics of that function in Java code would be too complicated, so we rather implemented it in
 * Enso and just call it from this node.
 */
@GenerateUncached
public abstract class HashCallbackNode extends Node {

  public static HashCallbackNode getUncached() {
    return HashCallbackNodeGen.getUncached();
  }

  /**
   * Dispatches to the appropriate comparator for the given atom and calls {@code hash} method on
   * it. Returns the value from that method.
   *
   * <p>Note that the given atom should have a custom comparator, otherwise it could be handled by
   * {@link org.enso.interpreter.node.expression.builtin.meta.HashCodeNode}.
   *
   * @param atom Atom, preferably with a custom comparator, for which we get the custom comparator
   *     and call {@code hash} method on the comparator.
   * @return Hash code for the atom, as returned by the custom comparator.
   */
  public abstract long execute(Atom atom);

  @Specialization
  long hashCallbackCached(
      Atom atom,
      @Cached(value = "getHashCallbackFunction()", allowUncached = true) Function hashCallbackFunc,
      @Cached(value = "buildInvokeNodeWithAtomArgument()", allowUncached = true)
          InvokeFunctionNode hashCallbackInvokeNode,
      @CachedLibrary(limit = "5") InteropLibrary interop) {
    var ctx = EnsoContext.get(this);
    var comparableType = ctx.getBuiltins().comparable().getType();
    Object res =
        hashCallbackInvokeNode.execute(
            hashCallbackFunc, null, State.create(ctx), new Object[] {comparableType, atom});
    try {
      return interop.asLong(res);
    } catch (UnsupportedMessageException e) {
      throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
    }
  }

  /**
   * Builds an {@link InvokeFunctionNode} for a method with just one argument named {@code atom}.
   */
  @NeverDefault
  static InvokeFunctionNode buildInvokeNodeWithAtomArgument() {
    return InvokeFunctionNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo("self"), new CallArgumentInfo("atom")},
        DefaultsExecutionMode.EXECUTE,
        ArgumentsExecutionMode.EXECUTE);
  }

  @NeverDefault
  @TruffleBoundary
  Function getHashCallbackFunction() {
    var comparableType = EnsoContext.get(this).getBuiltins().comparable().getType();
    Function hashCallback =
        comparableType.getDefinitionScope().getMethodForType(comparableType, "hash_callback");
    assert hashCallback != null : "Comparable.hash_callback function must exist";
    return hashCallback;
  }
}
