package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;

@NodeInfo(
    shortName = "invalidComparison",
    description = "Propagates the result of the invalid comparison via a Panic.")
public abstract class InvalidComparisonNode extends Node {

  public abstract int execute(Object result);

  public static InvalidComparisonNode build() {
    return InvalidComparisonNodeGen.create();
  }

  @Specialization
  int doDataFlowError(DataflowError dataflowError) {
    CompilerDirectives.transferToInterpreter();
    throw new PanicException(dataflowError.getPayload(), dataflowError.getLocation());
  }

  @Specialization
  int doPanic(Object result) {
    CompilerDirectives.transferToInterpreter();
    var ordering = EnsoContext.get(this).getBuiltins().ordering().getType();
    throw new PanicException(
        EnsoContext.get(this).getBuiltins().error().makeTypeError(ordering, result, "result"),
        this);
  }
}
