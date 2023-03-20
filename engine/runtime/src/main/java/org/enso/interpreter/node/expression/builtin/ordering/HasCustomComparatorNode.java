package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
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
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.state.State;

/**
 * Helper node for invocation of {@code Comparable.has_custom_comparator atom}. Note that emulating
 * the semantics of that function in Java code would be too complicated, so we rather implemented it
 * in Enso and just call it from this node.
 */
@GenerateUncached
public abstract class HasCustomComparatorNode extends Node {

  public static HasCustomComparatorNode getUncached() {
    return HasCustomComparatorNodeGen.getUncached();
  }

  /**
   * Returns true if the given atom has a custom comparator, that is a comparator that is different
   * than the default (internal) ones.
   *
   * @param atom Atom for which we check whether it has custom comparator
   * @return true iff the given atom has a custom comparator
   */
  public abstract boolean execute(Atom atom);

  @Specialization
  boolean hasCustomComparatorCached(
      Atom atom,
      @Cached(value = "getHasCustomComparatorFunction()", allowUncached = true)
          Function hasCustomComparatorFunc,
      @Cached(value = "buildInvokeNodeWithAtomArgument()", allowUncached = true)
          InvokeFunctionNode hasCustomComparatorInvokeNode,
      @CachedLibrary(limit = "5") InteropLibrary interop) {
    var ctx = EnsoContext.get(this);
    var comparableType = ctx.getBuiltins().comparable().getType();
    Object res =
        hasCustomComparatorInvokeNode.execute(
            hasCustomComparatorFunc, null, State.create(ctx), new Object[] {comparableType, atom});
    assert interop.isBoolean(res);
    try {
      return interop.asBoolean(res);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(
          "Return type from Comparable.has_custom_comparator should be Boolean", e);
    }
  }

  /**
   * Builds an {@link InvokeFunctionNode} for a method with just one argument named {@code atom}.
   */
  static InvokeFunctionNode buildInvokeNodeWithAtomArgument() {
    return InvokeFunctionNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo("self"), new CallArgumentInfo("atom")},
        DefaultsExecutionMode.EXECUTE,
        ArgumentsExecutionMode.EXECUTE);
  }

  @TruffleBoundary
  Function getHasCustomComparatorFunction() {
    var comparableType = EnsoContext.get(this).getBuiltins().comparable().getType();
    Function hasCustomComparatorFunc =
        comparableType
            .getDefinitionScope()
            .getMethods()
            .get(comparableType)
            .get("has_custom_comparator");
    assert hasCustomComparatorFunc != null : "Comparable.has_custom_comparator function must exist";
    return hasCustomComparatorFunc;
  }
}
