package org.enso.interpreter.node.expression.builtin.ordering;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.InvokeConversionNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.state.State;

/**
 * Helper node for invocation of {@code Comparable.has_custom_comparator atom}. Note that emulating
 * the semantics of that function in Java code would be too complicated, so we rather implemented it
 * in Enso and just call it from this node.
 */
@GenerateUncached
public abstract class CustomComparatorNode extends Node {

  public static CustomComparatorNode getUncached() {
    return CustomComparatorNodeGen.getUncached();
  }

  /**
   * Returns the given atom's comparator if it is a comparator that is different
   * than the default (internal) one.
   *
   * @param atom Atom for which we check whether it has custom comparator
   * @return {@code null} if the atom has default comparator. Otherwise it returns the real comparator type.
   */
  public abstract Type execute(Atom atom);

  @Specialization
  Type hasCustomComparatorCached(
      Atom atom,
      @Cached(value = "buildConvertionNode()", allowUncached = true) InvokeConversionNode convertNode,
      @Cached(value = "createConversion()", allowUncached = true) UnresolvedConversion conversion
  ) {
    var ctx = EnsoContext.get(this);
    var comparableType = ctx.getBuiltins().comparable().getType();
    var state = State.create(ctx);
    Object res = convertNode.execute(null, state, conversion, comparableType, atom, new Object[] { comparableType, atom });
    return res instanceof Type result && result != ctx.getBuiltins().defaultComparator().getType() ? result : null;
  }

  UnresolvedConversion createConversion() {
    var ctx = EnsoContext.get(this);
    var comparableType = ctx.getBuiltins().comparable().getType();
    return UnresolvedConversion.build(comparableType.getDefinitionScope());
  }

  static InvokeConversionNode buildConvertionNode() {
    CallArgumentInfo[] argSchema = new CallArgumentInfo[2];
    argSchema[0] = new CallArgumentInfo();
    argSchema[1] = new CallArgumentInfo();

    return InvokeConversionNode.build(argSchema, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE, 1);
  }
}
